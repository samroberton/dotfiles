{:user {:plugins [[cider/cider-nrepl "0.11.0"]
                  [refactor-nrepl "1.2.0"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.12"]
                       [org.clojure/tools.reader "0.10.0"]]

        :repl-options {:init (do
                               (set! *print-length* 200)
                               (set! *print-level* 20))}}}
