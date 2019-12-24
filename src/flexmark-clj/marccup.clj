(ns flexmark-clj.marccup
  (:require
   [camel-snake-kebab.core :as csk]
   [clojure.string :as str]
   [clojure.walk :as walk])
  (:import
   (com.vladsch.flexmark.ast Heading
                             Text)
   (com.vladsch.flexmark.ext.admonition AdmonitionBlock)
   (com.vladsch.flexmark.util.ast Node)))

(defn class-name
  [o]
  (-> o
      (class)
      (.getSimpleName)
      (csk/->kebab-case)))

(defn parse-attrs
  "Extracts the attributes of a class from its string representation.
   Returns a Clojure map of the attributes."
  [s]
  (let [[_ attrs] (re-matches #"[^{]+\{([^}]+)*\}" s)]
    (if (empty? attrs)
      {}
      (->> (for [attr (str/split attrs #", ")]
             (str/split attr #"=" 2))
           (into {})
           (walk/keywordize-keys)))))

(defprotocol Renderer
  (render [this]))

(extend-protocol Renderer
  AdmonitionBlock
  (render [this]
    [(-> this class-name keyword) {:title (str (.getTitle this))
                                   :info  (str (.getInfo this))}])

  Heading
  (render [this]
    [(-> this class-name keyword) {:level (.getLevel this)}])

  Node
  (render [this]
    [(-> this class-name keyword) (parse-attrs (str this))])

  Text
  (render [this]
    [(-> this class-name keyword) {} (str (.getChars this))]))
