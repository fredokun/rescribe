(defproject rescribe "0.2.0-SNAPSHOT"
  :description "Rewrite systems in Clojure"
  :url "https://github.com/fredokun/rescribe"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :profiles {:dev {:dependencies [[midje "1.8.2"
                                   :exclusions [org.clojure/clojure]]
                                  [org.clojure/tools.nrepl "0.2.12"]]
                   :plugins [[lein-midje "3.2"]
                             [lein-codox "0.9.0"]]}
             :midje {}}
  :codox {:source-uri "https://github.com/fredokun/rescribe/blob/master/{filepath}#L{line}"
          :metadata {:doc/format :markdown}})


