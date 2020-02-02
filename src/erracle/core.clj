(ns erracle.core
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.java.io :as io]
            [erracle.conllu :refer [parse-files]]))

(s/def ::conll-file
  (fn [s]
    (and (string? s)
         (or (clojure.string/ends-with? s ".conllu")
             (clojure.string/ends-with? s ".conll")))))

(s/def ::args
  (s/+ ::conll-file))

(defn process-files
  [filepaths]
  (let [parsed-files (parse-files filepaths)]
    (prn (->> parsed-files
        first
        (take 5)))
    ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [files (s/conform ::args args)]
    (if (or (= files ::s/invalid)
            (= (count files) 0))
      (throw (Exception. "Command-line arguments must include one or more `.conll` or `.conllu` files."))
      (process-files files)))

  ;; shut down pmap agents
  (shutdown-agents))
