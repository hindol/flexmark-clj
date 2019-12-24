(ns flexmark-clj.core
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [flexmark-clj.marccup :as marccup])
  (:import
   (com.vladsch.flexmark.ext.admonition AdmonitionExtension)
   (com.vladsch.flexmark.html HtmlRenderer)
   (com.vladsch.flexmark.parser Parser)
   (com.vladsch.flexmark.util.data MutableDataSet)
   (java.util Arrays)))

(defn pull-api
  "Pulls latest Besu API documentation from Github."
  []
  (with-open [in  (io/input-stream "https://raw.githubusercontent.com/hyperledger/besu-docs/master/docs/Reference/API-Methods.md")
              out (io/output-stream "./resources/API-Methods.md")]
    (io/copy in out)))

(re-matches #"[^{]+\{([^}]*)\}" "Link{text=--rpc-http-host, url=CLI/CLI-Syntax.md#rpc-http-host, title=}")

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
    (into (marccup/render this) (for [child (children this)]
                                 (visit child)))))

(defn parse
  "Slurps a markdown string and spits a Clojure map."
  [markdown]
  (let [extensions (Arrays/asList (into-array [(AdmonitionExtension/create)]))
        options    (doto (MutableDataSet.) (.set Parser/EXTENSIONS extensions))
        parser     (.build (Parser/builder options))
        parsed     (.parse parser markdown)]
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

(markdown->html "./resources/API-Methods.md" "./resources/API-Methods.html")
