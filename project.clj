(defproject sablono-tools "0.1.0-SNAPSHOT"
  :description "Text replacement tools for Sablono"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2322"]
                 [sablono "0.2.22"]
                 [hickory "0.5.3"]]

  :min-lein-version "2.0.0"
  :source-paths ["src/clj" "target/generated/clj"]

  :plugins [[lein-cljsbuild "1.0.3"]
            [com.cemerick/clojurescript.test "0.3.1"]]

  :profiles {:dev {:plugins [[com.cemerick/austin "0.1.5"]
                             [com.keminglabs/cljx "0.4.0" :exclusions [org.clojure/clojure]]]}}


  :cljsbuild {:builds [{:source-paths ["src/cljs" "test"]
                        :compiler {:output-to "target/cljs/whitespace.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}
                       {:source-paths ["src/cljs" "test"]
                        :compiler {:output-to "target/cljs/simple.js"
                                   :optimizations :simple
                                   :pretty-print true}}
                       {:source-paths ["src/cljs" "test"]
                        :compiler {:output-to "target/cljs/advanced.js"
                                   :optimizations :advanced
                                   :pretty-print true}}]

              :test-commands {"phantom-whitespace" ["phantomjs" :runner
                                                    "target/cljs/whitespace.js"]
                              "phantom-simple" ["phantomjs" :runner
                                                "target/cljs/simple.js"]
                              "phantom-advanced" ["phantomjs" :runner
                                                  "target/cljs/advanced.js"]}})
