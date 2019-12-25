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

(defn ^:private class-name
  [o]
  (-> o
      (class)
      (.getSimpleName)
      (csk/->kebab-case)))

(defn ^:private ->tag
  [o]
  (-> o
      (class-name)
      (keyword)))

(defn ^:private parse-attrs
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
    [(->tag this) {:title (str (.getTitle this))
                   :info  (str (.getInfo this))}])

  Heading
  (render [this]
    [(->tag this) {:level (.getLevel this)}])

  Node
  (render [this]
    [(->tag this) (parse-attrs (str this))])

  Text
  (render [this]
    [(->tag this) {} (str (.getChars this))]))
