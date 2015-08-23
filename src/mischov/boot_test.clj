(ns mischov.boot-test
  {:boot/export-tasks true}
  (:refer-clojure :exclude [test])
  (:require [boot.core :as core]
            [clojure.java.io :as io]
            [clojure.test :as t]
            [clojure.test.junit :as tj]
            [clojure.tools.namespace.find :as namespace-find]
            [clojure.tools.namespace.repl :as namespace-repl]
            [pjstadig.humane-test-output :refer [activate!]]
            ))

(activate!)

(defonce repeated? false)

;; Get all namespaces.
(defn get-all-ns [dirs]
  (let [dirs' (map (memfn getPath) dirs)]
    (mapcat #(namespace-find/find-namespaces-in-dir (io/file %)) dirs')))
     
;; Narrow down namespaces.
(defn filter-namespaces
  [namespaces regex]
  (if regex
    (filter #(re-find regex (str %)) namespaces)
    namespaces))

;; Custom core.test impls that support test filtering.
(defn test-ns
  [pred ns]
  (binding [t/*report-counters* (ref t/*initial-report-counters*)]
    (let [ns-obj (the-ns ns)]
      (t/do-report {:type :begin-test-ns :ns ns-obj})
      (t/test-vars (filter pred (vals (ns-interns ns-obj))))
      (t/do-report {:type :end-test-ns :ns ns-obj}))
    @t/*report-counters*))

(defn run-tests
  ([pred] (run-tests pred *ns*))
  ([pred & namespaces]
   (let [summary (assoc (apply merge-with + (map #(test-ns pred %)
                                                 namespaces))
                        :type :summary)]
     (t/do-report summary)
     summary)))
     
;; Run tests.
(defn test-with-formatting
  [pred formatter namespaces]
  (case formatter
    :junit (tj/with-junit-output
             (apply run-tests pred namespaces))
    (apply run-tests pred namespaces)))

(defn boot-run-tests
  [pred output-path formatter namespaces]
  (if output-path
    (with-open [writer (io/writer output-path)]
      (binding [t/*test-out* writer]
        (test-with-formatting pred formatter namespaces)))
    (test-with-formatting pred formatter namespaces)))

(if ((loaded-libs) 'boot.user)
  (ns-unmap 'boot.user 'test))

(core/deftask test
  "Run clojure.test tests in a pod."
  [n namespaces NAMESPACE #{sym} "The set of namespace symbols to run tests in."
   l limit-regex REGEX regex "A regex for limiting namespaces to be tested."
   t test-filters EXPR #{edn} "The set of expressions to use to filter tests."
   o output-path PATH str "A string representing the filepath to output test results to. Defaults to *out*."
   f formatter FORMATTER kw "Tag defining formatter to use with test results. Currently accepts `junit`. Defaults to standard clojure.test output."]

  (core/with-pre-wrap fileset
    (let [input-dirs (core/input-dirs fileset)]
      (when repeated?
        (apply namespace-repl/set-refresh-dirs input-dirs)
        (with-bindings {#'*ns* *ns*}
          (namespace-repl/refresh)))
      (println "Starting test...")
      (let [all-ns (get-all-ns input-dirs)
            namespaces (or (seq namespaces)
                           (filter-namespaces all-ns limit-regex))]
        (if (seq namespaces)
          (let [test-predicate (eval `(~'fn [~'%] (and ~@test-filters)))
                _ (doseq [ns namespaces] (require ns))
                summary (boot-run-tests test-predicate
                                        output-path
                                        formatter
                                        namespaces)]
            (println "Test complete.")
            (if (> (apply + (map summary [:fail :error])) 0)
              (throw (ex-info "Failed or errored tests"
                              (dissoc summary :type)))
              (println "Test summary: " (dissoc summary :type))))
          (println "No namespaces were tested."))
        (when-not repeated?
          (alter-var-root #'repeated?
                          (fn [_]
                            (future
                              (doseq [ns all-ns]
                                (require ns)))
                            true)))
        fileset))))
