(ns reading-pci.recommendation
  (:use [clojure.contrib.combinatorics :only (combinations)]))

(def critics
     {"Lisa Rose"     {"Lady in the Water" 2.5,
                       "Snakes on a Plane" 3.5,
                       "Just My Luck" 3.0,
                       "Superman Returns" 3.5,
                       "You, Me and Dupree" 2.5,
                       "The Night Listener" 3.0}
      
      "Gene Seymour"  {"Lady in the Water" 3.0,
                       "Snakes on a Plane" 3.5,
                       "Just My Luck" 1.5,
                       "Superman Returns" 5.0,
                       "The Night Listener" 3.0,
                       "You, Me and Dupree" 3.5}
      
      "Michael Phillips" {"Lady in the Water" 2.5,
                          "Snakes on a Plane" 3.0,
                          "Superman Returns" 3.5,
                          "The Night Listener" 4.0}
      
      "Claudia Puig"  {"Snakes on a Plane" 3.5,
                       "Just My Luck" 3.0,
                       "The Night Listener" 4.5,
                       "Superman Returns" 4.0,
                       "You, Me and Dupree" 2.5}

      "Mick LaSalle" {"Lady in the Water" 3.0,
                       "Snakes on a Plane" 4.0,
                       "Just My Luck" 2.0,
                       "Superman Returns" 3.0,
                       "The Night Listener" 3.0,
                       "You, Me and Dupree" 2.0}
      
      "Jack Matthews" {"Lady in the Water" 3.0,
                       "Snakes on a Plane" 4.0,
                       "The Night Listener" 3.0,
                       "Superman Returns" 5.0,
                       "You, Me and Dupree" 3.5}
      
      "Toby" {"Snakes on a Plane" 4.5,
              "You, Me and Dupree" 1.0,
              "Superman Returns" 4.0}
      })


(defn- pow  [v n] (Math/pow v n))
(defn- sqrt [v]   (Math/sqrt v))
(defn- nilzero [v] (if (nil? v) 0 v))
(defn- mapsum [f coll] (apply + (map f coll)))
(defn- flatten-merge [coll] (apply merge (flatten coll)))
(defn- score
  ([m k] (nilzero (m k)))
  ([m k1 k2] (if (nil? (m k1))
               0
               (nilzero ((m k1) k2)))))


(defn sim-distance
  "ユーザ間の類似度をユークリッド距離で算出する"
  [prefs person1 person2]
  (let [si (filter #((prefs person1) %) (keys (prefs person2)))]
    (if (empty? si)
      0
      (let [sum-of-squares (mapsum #(pow (- ((prefs person1) %)
                                            ((prefs person2) %)) 2)
                                   si)]
        (/ 1 (+ 1 sum-of-squares))))))

(defn sim-pearson
  "ユーザ間の類似度をピアソン係数で算出する"
  [prefs p1 p2]
  (let [;; p1, p2が両方見ている映画のシーケンスを作る
        si (filter #((prefs p1) %) (keys (prefs p2)))
        n  (count si)]
    (if (zero? n)
      0
      (let [sum1   (mapsum #((prefs p1) %) si)
            sum2   (mapsum #((prefs p2) %) si)
            sum1Sq (mapsum #(pow ((prefs p1) %) 2) si)
            sum2Sq (mapsum #(pow ((prefs p2) %) 2) si)
            ;; 積を合算する
            pSum   (mapsum #(* ((prefs p1) %) ((prefs p2) %)) si)
            ;; ピアソンによるスコアを計算する
            num    (- pSum (/ (* sum1 sum2) n))
            den    (sqrt (* (- sum1Sq (/ (pow sum1 2) n))
                            (- sum2Sq (/ (pow sum2 2) n))))]
        (if (zero? den)
          0
          (/ num den))))))

(defn top-matches
  "ハッシュマップprefsからpersonにもともマッチするものたちを返す
   結果の数と類似性関数はオプションパラメータ"
  ([prefs person n similarity]
     (let [scores (for [other (keys prefs) :when (not= person other)]
                    (vector (similarity prefs person other) other))]
       ;; 高スコアがリストの最初に来るように並び替える
       (take n (reverse (sort scores)))))
  ([prefs person n]
     (top-matches prefs person n sim-pearson))
  ([prefs person]
     (top-matches prefs person 5)))

(defn get-recommendations
  "person以外の全ユーザの評点の重み付き平均を使い、personへの推薦を算出する"
  ([prefs person similarity]
     (let [;; person以外の評者のシーケンスを作る
           others (for [other (keys prefs)
                        ;; 自分自身とは比較しない
                        :when (not= other person)] other)
           ;; 評者をkey、personとの類似度をvalueにしたハッシュマップをつくる
           sims   (apply merge
                         (for [o others
                               :let [sim (similarity prefs person o)]
                               ;; 類似度が0より大きい場合のみ計算する
                               :when (< 0 sim)]
                           {o sim}))
           ;;　personが見ていない映画のシーケンスをつくる
           movies (distinct (for [o others, m (keys (prefs o))
                                  :when (nil? ((prefs person) m))]
                              m))
           ;; 映画と、映画の評点*評者の類似度をまとめたシーケンスを作る
           totals (for [m movies]
                    (vector (mapsum #(* (score prefs % m)
                                        (score sims %))
                                    others)
                            m))
           ;; 映画について点数をつけている評者の類似度を合算する
           simSums (zipmap movies
                          (for [m movies
                                ;; mに点数をつけている評者のリストpをつくる
                                :let [p (filter #((prefs %) m) others)]]
                            (mapsum #(score sims %) p)))
           ;; 正規化したリストを作る
           rankings (for [[total m] totals]
                      (vector (/ total (simSums m)) m))]
       ;; ソート済みのリストを返す
       (reverse (sort rankings))))
  ([prefs person]
     (get-recommendations prefs person sim-pearson)))

(defn transform-prefs
  ""
  ([prefs]
     (let [movies (distinct (for [p (keys prefs), m (keys (prefs p))] m))]
       (zipmap movies
               (for [m movies
                     ;; mに評点をつけている評者たち
                     :let [people (filter #((prefs %) m) (keys prefs))]]
                 (zipmap people
                         (map #((prefs %) m) people)))))))
