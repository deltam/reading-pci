(ns reading-pci.delicious
  (:use [clojure.xml :only (parse)])
  (:use [clojure.contrib.seq :only (find-first)])
  (:use [clojure.contrib.string :only (replace-re)]))

(def delicious-popular-url   "http://delicious.com/v2/rss/popular/")
(def delicious-userposts-url "http://delicious.com/v2/rss/")
(def delicious-urlposts-url  "http://delicious.com/v2/rss/url/")

;; https://gist.github.com/1302024
(defn md5
  "Generate a md5 checksum for the given string"
  [token]
  (let [hash-bytes
         (doto (java.security.MessageDigest/getInstance "MD5")
               (.reset)
               (.update (.getBytes token)))]
       (.toString
         (new java.math.BigInteger 1 (.digest hash-bytes)) ; Positive and the size of the number
         16))) ; Use base16 i.e. hex

(defn parse-delicious-rss
  "del.icio.us APIから受け取るRSSをハッシュマップのvectorに変換する"
  [xs]
  (for [;; itemタグだけフィルタする
        item (filter #(= :item (:tag %)) xs)
        :let [get-content (fn [tag] (let [{[cont] :content} (find-first #(= tag (:tag %)) (:content item))] cont))
              ;; itemタグ内のtitle、source、linkタグの内容だけ取得する
              desc (get-content :title)   ; ページタイトル
              user (get-content :source)  ; ユーザ名
              url  (get-content :link)    ; URL
              tags (vec (map #(first (:content %)) (filter #(= :category (:tag %)) (:content item))))]]
    {:description desc
     :href        url
     ;; sourceタグのコンテンツを投稿ユーザ名に変換する
     :user        (replace-re #"'s links$" "" user)
     :tag         tags}))

(defn get-popular [tag]
  (parse-delicious-rss (xml-seq (parse (str delicious-popular-url tag)))))
  
(defn get-userposts [user]
  (parse-delicious-rss (xml-seq (parse (str delicious-userposts-url user)))))

(defn get-urlposts [url]
  (parse-delicious-rss (xml-seq (parse (str delicious-urlposts-url (md5 url))))))


;; deliciousrec.py

(defn initialize-user-dict
  ([tag count]
     (zipmap
      (for [;; popularな投稿をcount番目まで取得
            p1 (take count (get-popular tag)),
            ;; このリンクを投稿したすべてのユーザを取得
            p2 (get-urlposts (p1 :href))]
        (p2 :user))
      (repeat nil)))
  ([tag]
     (initialize-user-dict tag 5)))

(defn try-get-userposts [user]
  (loop [i 0]
    (if (< i 3)
      (try
       (do (get-userposts user)
           (recur 3))
       (catch Exception _
         (println "Failed user " user ", retrying")
         (Thread/sleep 4000)))
      (recur (inc i)))))

(defn fill-items
  ([user-dict]
     (let [users (keys user-dict)
           dict  (zipmap users
                         (for [u users
                               :let [post-urls (map :href (get-userposts u))]]
                           (zipmap post-urls (repeat 1.0))))
           all-items (distinct (flatten (map #(keys (dict %)) users)))]
       (zipmap users
               (for [u users :let [user-items (dict u)]]
                 (zipmap all-items
                         (map #(if (user-items %)
                                 (user-items %)
                                 0.0)
                              all-items)))))))
