(ns flexmark-clj.core
  (:require
   [camel-snake-kebab.core :as csk]
   [clojure.java.io :as io]
   [clojure.pprint :as pp]
   [clojure.string :as str])
  (:import
   (com.vladsch.flexmark.ast Text)
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

(defn class-name
  [o]
  (.getSimpleName (class o)))

(defn node-key
  [o]
  (keyword (csk/->kebab-case (class-name o))))

(defprotocol Converter
  (convert [this children]))

(extend-protocol Converter
  Node
  (convert [this children]
    {(node-key this) (into [] children)})

  Text
  (convert [this _]
    {(node-key this) (str (.getChars this))}))

(defn siblings
  [node]
  (if (nil? node)
    []
    (lazy-seq (cons node (siblings (.getNext node))))))

(defn children
  [parent]
  (siblings (.getFirstChild parent)))

(defprotocol Visitor
  (visit [this]))

(extend-protocol Visitor
  Node
  (visit [this]
    (let [children (for [child (children this)]
                     (visit child))]
      (convert this children))))

(defn parse
  "Slurps a markdown string and spits a Clojure map."
  [markdown]
  (let [extensions (Arrays/asList (into-array [(AdmonitionExtension/create)]))
        options    (doto (MutableDataSet.) (.set Parser/EXTENSIONS extensions))
        parser     (.build (Parser/builder options))
        parsed     (.parse parser markdown)]
    (pp/pprint (visit parsed))
    parsed))

(defn render
  "Takes in a markdown in AST form, spits the HTML."
  [parsed]
  (let [renderer (.build (HtmlRenderer/builder))]
    (.render renderer parsed)))

(defn markdown->html
  [in out]
  (with-open [reader (io/reader in)
              writer (io/writer out)
              edn    (io/writer "./resources/API-Methods.edn")]
    (let [markdown (->> reader line-seq (str/join "\n"))
          ast      (parse markdown)
          html     (render ast)]
      (pp/pprint (visit ast) edn)
      (.write writer html))))

(comment
  (markdown->html "./resources/API-Methods.md" "./resources/API-Methods.html"))
