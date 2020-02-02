(ns erracle.conllu
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]))

(def conllu-line-parser
  (insta/parser
    ;; This is a parser for a line in the CONLL-U format. Only the vanilla 10 columns are supported.
    ;; This aims to be complete for the specification as it existed in January 2020.
    ;; The result is this parser is a seq of 11-item vectors, the first being a keyword
    ;; indicating the line's type, and the remaining columns containing the CONLLU data.
    ;; This parser tries to restrict input wherever there are explicit specifications,
    ;; and otherwise takes input that is as broad as sensibly possible.
    ;; Cf. https://universaldependencies.org/format.html
    "
    <line> ::= comment-line
             | token-line
             | supertoken-line
             | ellipsis-line
             | <empty-line>

    <EOL> ::= <#'\\r\\n'>
            | <#'\\r'>
            | <#'\\n'>
            | <#'\\Z'> (* \\Z means EOF *)

    (* cool [^\\S\\r\\n] pattern from https://stackoverflow.com/questions/3469080/match-whitespace-but-not-newlines *)
    empty-line   ::= #'[^\\S\\r\\n]*' EOL
    comment-line ::= #'#[^\\n]*' EOL
    token-line      ::= id-token      <'\t'> 9-columns EOL
    supertoken-line ::= id-supertoken <'\t'> 9-columns EOL
    ellipsis-line   ::= id-ellipsis   <'\t'> 9-columns EOL

    (* columns ****************************************************************************)
    (* column 1 used to differentiate lines: supertoken, ellipsis, and token lines are all
     * distinguished based on their IDs.
     *)
    id-token      ::= #'\\d+'
    id-supertoken ::= #'\\d+-\\d+'
    id-ellipsis   ::= #'\\d+\\.\\d+'

    (* repeat literally because we don't want a :id-token, etc. node in a ref *)
    <token-ref> ::= #'\\d+' | #'\\d+-\\d+' | #'\\d+\\.\\d+'

    <9-columns> ::= form
             <'\t'> lemma
             <'\t'> upos
             <'\t'> xpos
             <'\t'> feats
             <'\t'> head
             <'\t'> deprel
             <'\t'> deps
             <'\t'> misc

    <underscore-or-not-tab>               ::= '_' | #'[^\t]+'
    <underscore-or-not-tab-or-whitespace> ::= '_' | #'[^\t\\s]+'

    form   ::= underscore-or-not-tab
    lemma  ::= underscore-or-not-tab
    upos   ::= underscore-or-not-tab-or-whitespace
    xpos   ::= underscore-or-not-tab-or-whitespace
    head   ::= '_' | token-ref
    deprel ::= '_' | #'[a-z]+(:[a-z]+)?'

    (* e.g., `Case=Nom|NumType=Card` *)
    feats       ::= '_' | feat-list
    <feat-list> ::= ε | feat feat-tail
    <feat-tail> ::= ε | <'|'> feat feat-tail
    feat        ::= feat-name <'='> feat-val
    <feat-name> ::= #'[A-Z0-9][A-Z0-9a-z]*(\\[[a-z0-9]+\\])?'
    <feat-val>  ::= #'[A-Z0-9][a-zA-Z0-9]*'

    deps        ::= '_' | dep-list
    <dep-list>  ::= ε | dep dep-tail
    <dep-tail>  ::= ε | <'|'> dep dep-tail
    dep         ::= dep-head <':'> dep-rest
    <dep-head>  ::= token-ref
    <dep-rest>  ::= #'[a-z]+'
                    (<':'> #'[a-z]+')?
                    (<':'> #'[\\p{Ll}\\p{Lm}\\p{Lo}\\p{M}]+(_[\\p{Ll}\\p{Lm}\\p{Lo}\\p{M}]+)*')?
                    (<':'> #'[a-z]+')?

    misc        ::= '_' | misc-list
    <misc-list> ::= ε | misc-item misc-tail
    <misc-tail> ::= ε | <'|'> misc-item misc-tail
    misc-item   ::= #'\\w+' <'='> #'.+'
    "))


;; light postprocessing ------------------------------------------------------------
;; - turn FEATS, DEPS, MISC into a dict or nil
;; - made ID uniform
;; - turn HEAD and ID into numbers
(defmulti postprocess-column first)
(defmethod postprocess-column :default [x] x)

(defmethod postprocess-column :id-token
  [[_ val]]
  [:id (clojure.edn/read-string val)])

(defmethod postprocess-column :id-supertoken
  [[_ val]]
  [:id [(map clojure.edn/read-string (clojure.string/split val #"-"))]])

(defmethod postprocess-column :id-ellipsis
  [[_ val]]
  [:id (clojure.edn/read-string val)])

(defmethod postprocess-column :head
  [[tag val]]
  (if (= val "_")
    [tag nil]
    [tag (clojure.edn/read-string val)]))

(defmethod postprocess-column :feats
  [[tag & data]]
  (if (and (= (count data) 1) (= (first data) "_"))
    [tag nil]
    [tag (into {} (for [[_ k v] data]
                    [(keyword k) v]))]))

(defmethod postprocess-column :misc
  [[tag & data]]
  (if (and (= (count data) 1) (= (first data) "_"))
    [tag nil]
    [tag (into {} (for [[_ k v] data]
                    [(keyword k) v]))]))

(defmethod postprocess-column :deps
  [[tag & deps]]
  (if (and (= (count deps) 1) (= (first deps) "_"))
    [tag nil]
    [tag (into {} (for [[_ head deprel] deps]
                    [(clojure.edn/read-string head) deprel]))]))

(defn- postprocess-columns
  [[tag & cols :as line]]
  (if (#{:token-line :supertoken-line :ellipsis-line} tag)
    [tag (into {} (map postprocess-column cols))]
    line))


;; records -------------------------------------------------------------------
;; turn every line type into a record
(defrecord TokenLine [id form lemma upos xpos feats head deprel deps misc])
(defrecord SuperTokenLine [id form lemma upos xpos feats head deprel deps misc])
(defrecord EllipsisLine [id form lemma upos xpos feats head deprel deps misc])
(defrecord CommentLine [body])

(defmulti recordify first)
(defmethod recordify :token-line
  [[_ fields]]
  (map->TokenLine fields))
(defmethod recordify :supertoken-line
  [[_ fields]]
  (map->SuperTokenLine fields))
(defmethod recordify :ellipsis-line
  [[_ fields]]
  (map->EllipsisLine fields))
(defmethod recordify :comment-line
  [[_ text]]
  (->CommentLine text))

;; top level fns -----------------------------------------------------------------
(defn- parse-line
  [line]
  (some-> line
          conllu-line-parser
          first
          postprocess-columns
          recordify))

(defn- sentencify
  "Turns a seq of lines into a seq of sentences (which are seqs of lines) by partitioning
  by ascending :id."
  [line-seq]
  (let [last-id (atom nil)]
    (partition-by
      (fn [{:keys [id] :as x}]
        ;; begin a new partition when current item has an id and either
        ;;   (1) this ID is nil and the last one was not nil
        ;;       (happens when a sentence begins with header comments
        ;;   (2) this and the last id are non-nil and this id is lesser than the last one
        ;;       (happens when
        ;; was nil or (2) the last id was not nil and the current id is lesser than it
        (let [retval (or (and (nil? id) (some? @last-id))
                         (and (some? id) (some? @last-id) (< id @last-id)))]

          ;; the cases above assume comments can't occur inside sentences, throw if this happens
          (when (and (nil? @last-id) (some? id) (> id 1))
            (throw (ex-info "There was a comment in the middle of a sentence!" x)))

          (reset! last-id id)
          retval))
      line-seq)))

(defn parse-file
  "Parses a conllu file and returns a seq of sentences, where each sentence is a seq of lines.
  Comments are discarded."
  [filepath]
  (when-not (.exists (io/as-file filepath))
    (throw (ex-info "File doesn't exist" {:filepath filepath})))

  (->> filepath
       slurp
       clojure.string/split-lines
       (pmap parse-line)
       (remove nil?)
       sentencify))

(defn remove-comments
  [sentences]
  (->> (for [sentence sentences]
         (remove #(instance? CommentLine %) sentence))
       (remove nil?)))

(defn remove-supertokens
  [sentences]
  (->> (for [sentence sentences]
        (remove #(instance? SuperTokenLine %) sentence))
      (remove nil?)))

(defn remove-ellipsis-tokens
  [sentences]
  (->> (for [sentence sentences]
         (remove #(instance? EllipsisLine %) sentence))
       (remove nil?)))

(defn parse-files
  [filepaths]
  (map parse-file filepaths))
