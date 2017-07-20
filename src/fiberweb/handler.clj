(ns fiberweb.handler
	(:require 	[compojure.core              :refer [defroutes]]
  				[compojure.route             :as route]
            	[compojure.handler           :as handler]
            	[fiberweb.controllers.routes :as fcr]
            	[fiberweb.views.layout       :as layout]
            	[ring.middleware.defaults    :as rmd]
            	[ring.middleware.reload      :as rmr]
            	[ring.middleware.stacktrace  :as rmst]
            	[ring.adapter.jetty          :as ring]
            	(taoensso 		[timbre     	:as log])
            	(taoensso.timbre.appenders 	[core 			:as appenders])
            	[clojure.spec.alpha :as s]))

(defroutes routes
	fcr/routes
  	(route/resources "/")
  	(route/not-found (layout/four-oh-four)))

(defn enable-asserts
	[x]
	(s/check-asserts true)
	x)

(defn dirty-fix
	[x]
	(log/set-level! :trace)
    (log/merge-config! {:appenders {:println {:enabled? false}}})
    (log/merge-config! {:timestamp-opts {:pattern "MM-dd HH:mm:ss"
    					   				 :locale (java.util.Locale. "sv_SE")
    					   				 :timezone (java.util.TimeZone/getTimeZone "Europe/Stockholm")}
    					:output-fn (partial log/default-output-fn {:stacktrace-fonts {}})})
  	(log/merge-config!
  		{:appenders {:spit (appenders/spit-appender {:fname "fiber.log"})}})
	x)

(def application
	(-> routes
		enable-asserts
		dirty-fix
		rmst/wrap-stacktrace
		(rmd/wrap-defaults (assoc-in rmd/site-defaults [:security :anti-forgery] false))))

(defn start
	[port]
  	(ring/run-jetty application {:port port
                                 :join? false}))

(defn -main
	[]
	(println "startar webservern på port 3210")
  	(start 3210))
