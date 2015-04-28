{:user {:plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]
                  [lein-try "0.3.2"]
                  [lein-pprint "1.1.1"]
                  [lein-ancient "0.6.0-SNAPSHOT"]
                  [lein-bikeshed "0.1.3"]
                  [lein-kibit "0.0.8"]
                  [jonase/eastwood "0.2.1"]
                  [lein-midje "3.1.3"]
                  [lein-vanity "0.2.1"]
                  [refactor-nrepl "0.3.0-SNAPSHOT"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.10"]
                       [org.clojure/tools.reader "0.9.1"]
                       [jonase/kibit "0.0.8"]
                       [acyclic/squiggly-clojure "0.1.2-SNAPSHOT"]]
        :repl-options {:init (do
                               (set! *print-length* 200)
                               (set! *print-level* 20))}}}
