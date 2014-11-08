{:user {:plugins [[cider/cider-nrepl "0.7.0"]
                  [lein-ancient "0.6.0-SNAPSHOT"]]
        :repl-options {:init (do
                               (set! *print-length* 200)
                               (set! *print-level* 20))}}
 :repl {:resource-paths ["/usr/lib/jvm/java-8-oracle/lib/tools.jar"]}}
