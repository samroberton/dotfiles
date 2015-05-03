{:user {:plugins [[cider/cider-nrepl "0.9.0-20150502.155350-34"]
                  [lein-try "0.4.3"]
                  [lein-pprint "1.1.2"]
                  [lein-ancient "0.6.7"] ; `lein ancient profiles`
                  [lein-bikeshed "0.2.0"]
                  [lein-kibit "0.1.2"]
                  [jonase/eastwood "0.2.1"]
                  [lein-midje "3.1.3"]
                  [lein-vanity "0.2.0"]
                  [refactor-nrepl "1.0.5"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.10"]
                       [org.clojure/tools.reader "0.9.2"]
                       [jonase/kibit "0.1.2"]
                       [acyclic/squiggly-clojure "0.1.2-SNAPSHOT"]]
        :repl-options {:init (do
                               (set! *print-length* 200)
                               (set! *print-level* 20))}}}
