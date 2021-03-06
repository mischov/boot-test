(set-env!
 :source-paths #{"src"}
 :dependencies '[[org.clojure/clojure "1.6.0"      :scope "provided"]
                 [boot/core           "2.0.0"      :scope "provided"]
                 [adzerk/bootlaces    "0.1.11"     :scope "test"]
                 [org.clojure/tools.namespace "0.2.11"
                  :exclusions [org.clojure/clojure]]
                 [pjstadig/humane-test-output "0.6.0"
                  :exclusions [org.clojure/clojure]]])

(require '[adzerk.bootlaces :refer :all]
         '[mischov.boot-test :refer [test]])

(def +version+ "1.0.8")

(bootlaces! +version+)

(task-options!
 pom  {:project     'mischov/boot-test
       :version     +version+
       :description "Run some tests in boot!"
       :url         "https://github.com/mischov/boot-test"
       :scm         {:url "https://github.com/mischov/boot-test"}
       :license     {"Eclipse Public License"
                     "http://www.eclipse.org/legal/epl-v10.html"}}
 test {:namespaces '#{mischov.boot-test.test}})
