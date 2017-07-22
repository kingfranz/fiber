(defproject fiberweb "1.1.0"
  :description "web app for fiber"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [compojure "1.6.0"]
                 [hiccup "1.0.5"]
                 [clj-time "0.14.0"]
                 [garden "1.3.2"]
                 [cheshire "5.7.1"]
                 [com.novemberain/monger "3.1.0"]
                 [com.taoensso/timbre "4.10.0"]
                 [org.clojure/tools.reader "1.0.2"]
                 [clj-pdf "2.2.28"]
                 [org.clojure/data.csv "0.1.4"]
                 [ring "1.6.2"]
                 [ring/ring-defaults "0.3.0"]]
  :plugins [[lein-ring "0.11.0"]]
  :ring {:handler fiberweb.handler/application
  		 :auto-reload? true
         :auto-refresh? false
     	 :open-browser? true}
  :jvm-opts ["-Dclojure.spec.compile-asserts=true"]
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.0"]]}})
