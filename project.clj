(defproject de.active-group/reacl-c "0.11.6"
  :description "Compositional and declarative user interface library for Clojure and Clojurescript."
  :url "http://github.com/active-group/reacl-c"
  
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]
                 [org.clojure/clojurescript "1.10.773" :scope "provided"]
                 [de.active-group/active-clojure "0.36.0"]
                 [org.clojure/test.check "0.10.0" :scope "provided"]
                 [prismatic/schema "1.1.12"]
                 [de.active-group/cljs-async "2.0.0"]
                 ;; Note: the cljsjs dep is not actually needed when
                 ;; using shadow-cljs, but this surprisingly helps
                 ;; cljdoc not to fail for other libraries! Like
                 ;; reacl-c-basics. Hope it does not hurt otherwise.
                 [cljsjs/create-react-class "15.6.3-0" :exclusions [cljsjs/react]]]

  :profiles {:shadow {:dependencies [[thheller/shadow-cljs "2.11.7"]
                                     [binaryage/devtools "1.0.2"]]
                      :source-paths ["src" "test"]
                      :resource-paths ["target"]}}

  :clean-targets ^{:protect false} [:target-path]

  :aliases {"fig" ["with-profile" "shadow" "run" "-m" "shadow.cljs.devtools.cli" "watch" "test"]
            "build-test" ["with-profile" "shadow" "run" "-m" "shadow.cljs.devtools.cli" "compile" "ci"]
            ;; then run tests with: npx karma start --single-run
            }
  )
