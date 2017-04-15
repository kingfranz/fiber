(defproject fiberweb "0.1.0"
  :description "web app for fiber"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0-alpha15"]
                 [compojure "1.5.2"]
                 [hiccup "1.0.5"]
                 [clj-time "0.13.0"]
                 [garden "1.3.2"]
                 [com.taoensso/timbre "4.8.0"]
                 [clj-pdf "2.2.21"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/java.jdbc "0.6.1"]
                 [mysql/mysql-connector-java "6.0.6"]
                 [ring/ring-defaults "0.2.3"]]
  :plugins [[lein-ring "0.9.7"]]
  :ring {:handler fiberweb.handler/application
  		 :auto-reload? true
         :auto-refresh? true}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.0"]]}})
