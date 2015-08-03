(ns mischov.boot-test
  {:boot/export-tasks true}
  (:refer-clojure :exclude [test])
  (:require [boot.pod  :as pod]
            [boot.core :as core]))

(def pod-deps
  '[[org.clojure/tools.namespace "0.2.11" :exclusions [org.clojure/clojure]]
    [pjstadig/humane-test-output "0.6.0"  :exclusions [org.clojure/clojure]]])

(defn init [fresh-pod]
  (doto fresh-pod
    (pod/with-eval-in
     (require '[clojure.test :as t]
              '[clojure.test.junit :as tj]
              '[clojure.java.io :as io]
              '[pjstadig.humane-test-output :refer [activate!]]
              '[clojure.tools.namespace.find :as namespace-find])
     (activate!)
     
     ;; Get all namespaces.
     (defn get-all-ns [& dirs]
       (-> (mapcat #(namespace-find/find-namespaces-in-dir (io/file %)) dirs)))
     
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
         (test-with-formatting pred formatter namespaces))))))

(defn destroy
  [pod]
  (when pod
    (pod/with-eval-in pod
      ; println to see return value
      (println
       "Shutting down core.async: "
       (when-let [executor-var
                  (try (ns-resolve 'clojure.core.async.impl.exec.threadpool
                                   'the-executor)
                       (catch java.lang.Exception e nil))]
         ; arraylist into vector
         (into [] (.shutdownNow ^java.util.concurrent.ThreadPoolExecutor
                                @executor-var)))))
    (.invoke pod "clojure.core/shutdown-agents")
    (.. pod getClassLoader close)))

;;; This prevents a name collision WARNING between the test task and
;;; clojure.core/test, a function that nobody really uses or cares
;;; about.
(if ((loaded-libs) 'boot.user)
  (ns-unmap 'boot.user 'test))


(core/deftask test
  "Run clojure.test tests in a pod."
  [n namespaces NAMESPACE #{sym} "The set of namespace symbols to run tests in."
   l limit-regex REGEX regex "A regex for limiting namespaces to be tested."
   t test-filters EXPR #{edn} "The set of expressions to use to filter tests."
   o output-path PATH str "A string representing the filepath to output test results to. Defaults to *out*."
   f formatter FORMATTER kw "Tag defining formatter to use with test results. Currently accepts `junit`. Defaults to standard clojure.test output."]

  (let [worker-pods (pod/pod-pool (update-in (core/get-env) [:dependencies] into pod-deps) :init init :destroy destroy)]
    (core/cleanup (worker-pods :shutdown))
    (core/with-pre-wrap fileset
      (println "Starting test...")
      (let [worker-pod (worker-pods :refresh)
            namespaces (or (seq namespaces)
                               (pod/with-eval-in worker-pod
                                 (-> (get-all-ns ~@(->> fileset
                                                        core/input-dirs
                                                        (map (memfn getPath))))
                                     (filter-namespaces ~limit-regex))))]
        (if (seq namespaces)
          (let [test-predicate `(~'fn [~'%] (and ~@test-filters))
                summary (pod/with-eval-in worker-pod
                          (doseq [ns '~namespaces] (require ns))
                          (boot-run-tests ~test-predicate
                                          ~output-path
                                          ~formatter
                                          '~namespaces))]
            (println "Test complete.")
            (if (> (apply + (map summary [:fail :error])) 0)
              (throw (ex-info "Failed or errored tests"
                              (dissoc summary :type)))
              (println "Test summary: " (dissoc summary :type))))
          (println "No namespaces were tested."))
        fileset))))
