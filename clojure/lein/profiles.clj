{:user {:plugins [[cider/cider-nrepl "0.14.0" :exclusions [org.clojure/clojure org.clojure/tools.reader]]]
        :dependencies [[org.clojure/tools.nrepl "0.2.12"]
                       [org.clojure/tools.reader "1.0.0-beta4" :exclusions [org.clojure/clojure]]]
        :repl-options {:init (do
                               (set! *print-length* 200)
                               (set! *print-level* 20))}}}
