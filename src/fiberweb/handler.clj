(ns fiberweb.handler
	(:require 	[compojure.core              :refer [defroutes]]
  				[compojure.route             :as route]
            	[compojure.handler           :as handler]
            	[fiberweb.controllers.routes :as fcr]
            	[fiberweb.views.layout       :as layout]
            	[ring.middleware.defaults    :as rmd]
            	[ring.middleware.reload      :as rmr]
            	[ring.middleware.stacktrace  :as rmst]
            	[ring.adapter.jetty          :as ring]))

(defroutes routes
	fcr/routes
  	(route/resources "/")
  	(route/not-found (layout/four-oh-four)))

(def application
	(-> routes
		(rmr/wrap-reload)
		(rmst/wrap-stacktrace)
		(rmd/wrap-defaults rmd/site-defaults)))

(defn start
	[port]
  	(ring/run-jetty application {:port port
                                 :join? false}))

(defn -main []
  	(start 3000))
