{:user {:plugins [[cider/cider-nrepl "0.7.0"]
                  [lein-try "0.3.2"]
                  [lein-pprint "1.1.1"]
                  [lein-ancient "0.4.4"]
                  [lein-bikeshed "0.1.3"]]
        :repl-options {:init (do
                               (set! *print-length* 200)
                               (set! *print-level* 20))}}
 :repl {:resource-paths ["/usr/lib/jvm/java-8-oracle/lib/tools.jar"]}}
