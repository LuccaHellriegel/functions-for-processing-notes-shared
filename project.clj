(defproject functions-for-processing-notes-shared "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [babashka/fs "0.0.5"]]
  :main functions-for-processing-notes-shared.core
  :profiles {:uberjar {:aot :all}}
  :aot [functions-for-processing-notes-shared.core]
  :repl-options {:init-ns functions-for-processing-notes-shared.core})
