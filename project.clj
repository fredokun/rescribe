(defproject rescribe "0.4.0-SNAPSHOT"
  :description "Rewrite systems in Clojure"
  :url "https://github.com/fredokun/rescribe"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]]
  :profiles {:dev {:dependencies [[midje "1.8.3"
                                   :exclusions [org.clojure/clojure]]]
                   :plugins [[lein-midje "3.2.1"]
                             [lein-codox "0.10.2"]]}
             :midje {}}
  :codox {:source-uri "https://github.com/fredokun/rescribe/blob/master/{filepath}#L{line}"
          :metadata {:doc/format :markdown}})


