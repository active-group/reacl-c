(defproject de.active-group/reacl-c "0.10.0-SNAPSHOT"
  :description "Compositional and declarative user interface library for Clojure and Clojurescript."
  :url "http://github.com/active-group/reacl-c"
  
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]
                 [org.clojure/clojurescript "1.10.773" :scope "provided"]
                 [de.active-group/active-clojure "0.36.0"]
                 [org.clojure/test.check "0.10.0" :scope "provided"]
                 [prismatic/schema "1.1.12"]
                 [reacl "2.2.9"]
                 [de.active-group/cljs-async "2.0.0"]]

  :plugins [[lein-codox "0.10.7"]
            [lein-auto "0.1.3"]]

  :profiles {:shadow {:dependencies [[thheller/shadow-cljs "2.11.7"]
                                     [binaryage/devtools "1.0.2"]]
                      :source-paths ["src" "test" "examples"]
                      :resource-paths ["target" "resources"]}
             
             :codox {:dependencies [[codox-theme-rdash "0.1.2"]]}}

  :clean-targets ^{:protect false} [:target-path]

  :aliases {"fig" ["with-profile" "shadow" "run" "-m" "shadow.cljs.devtools.cli" "watch" "test"]
            "build-test" ["with-profile" "shadow" "run" "-m" "shadow.cljs.devtools.cli" "compile" "ci"]}

  :codox {:language :clojurescript ;; :clojure
          :metadata {:doc/format :markdown}
          :themes [:rdash]
          :src-dir-uri "http://github.com/active-group/reacl-c/blob/master/"
          :src-linenum-anchor-prefix "L"}

  :auto {:default {:paths ["src" "test" "examples"]}}
  )
