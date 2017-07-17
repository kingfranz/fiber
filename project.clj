(defproject fiberweb "1.1.0"
  :description "web app for fiber"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0-alpha15"]
                 [compojure "1.6.0"]
                 [hiccup "1.0.5"]
                 [clj-time "0.13.0"]
                 [garden "1.3.2"]
                 [com.taoensso/timbre "4.10.0"]
                 [clj-pdf "2.2.24"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/java.jdbc "0.6.1"]
                 [mysql/mysql-connector-java "6.0.6"]
                 [ring "1.6.1"]
                 [ring/ring-defaults "0.3.0"]]
  :plugins [[lein-ring "0.11.0"]]
  :ring {:handler fiberweb.handler/application
  		 :auto-reload? true
         :auto-refresh? false
     	 :open-browser? true}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.0"]]}})
