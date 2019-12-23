(ns flexmark-clj.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   (com.vladsch.flexmark.ext.admonition AdmonitionExtension)
   (com.vladsch.flexmark.html HtmlRenderer)
   (com.vladsch.flexmark.parser Parser)
   (com.vladsch.flexmark.util.ast Node)
   (com.vladsch.flexmark.util.data MutableDataSet)
   (java.util Arrays)))

(defn pull-api
  "Pulls latest Besu API documentation from Github."
  []
  (with-open [in  (io/input-stream "https://raw.githubusercontent.com/hyperledger/besu-docs/master/docs/Reference/API-Methods.md")
              out (io/output-stream "./resources/API-Methods.md")]
    (io/copy in out)))

(defn siblings
  [node]
  (if (nil? node)
    []
    (cons node (siblings (.getNext node)))))

(defn children
  [parent]
  (siblings (.getFirstChild parent)))

(defprotocol Visitor
  (visit [this]))

(extend-protocol Visitor
  Node
  (visit [this]
    (prn (class this))
    (doseq [child (children this)]
      (prn (class this))
      (visit child))))

(defn parse
  "Slurps a markdown string and spits a Clojure map."
  [markdown]
  (let [extensions (Arrays/asList (into-array [(AdmonitionExtension/create)]))
        options    (doto (MutableDataSet.) (.set Parser/EXTENSIONS extensions))
        parser     (.build (Parser/builder options))
        parsed     (.parse parser markdown)]
    (visit parsed)
    parsed))

(defn render-to-file
  "Renders markdown to HTML."
  [in out]
  (with-open [reader (io/reader in)
              writer (io/writer out)]
    (let [markdown (->> reader line-seq (str/join "\n"))
          renderer (.build (HtmlRenderer/builder))]
      (->> markdown
           (parse)
           (.render renderer)
           (.write writer)))))

(comment
  (render-to-file "./resources/API-Methods.md" "./resources/API-Methods.html"))
