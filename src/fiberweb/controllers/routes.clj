(ns fiberweb.controllers.routes
  	(:require [clojure.string     :as str]
              [ring.util.response :as ring]
              [compojure.core     :as cc]
              (clj-time       [core       :as t]
            	                [local      :as l]
            	                [format     :as f]
            	                [periodic   :as p])
              [fiberweb.views.home   :as home]
              [fiberweb.views.members :as members]
              [fiberweb.views.estates  :as estates]
              [fiberweb.views.system  :as system]))

;;-----------------------------------------------------------------------------

(defn current-year
	[]
	(t/year (l/local-now)))

(defn current-quarter
	[]
	(nth [1 1 1 2 2 2 3 3 3 4 4 4] (t/month (l/local-now))))

;;-----------------------------------------------------------------------------

(cc/defroutes routes
	(cc/GET "/" []
	    (home/home-page))

	(cc/GET "/invoice/membership" []
	    (system/invoice-membership (current-year)))
	(cc/GET "/invoice/membership/:year" [year]
	    (system/invoice-membership year))
	(cc/POST "/invoice/membership" request
	    (ring/redirect (str "/invoice/membership/" (:year request))))
	
	(cc/GET "/invoice/quarter" []
	    (system/invoice-quarter (current-year) (current-quarter)))
	(cc/GET "/invoice/quarter/:year/:quarter" [year quarter]
	    (system/invoice-quarter year quarter))
	(cc/POST "/invoice/quarter" request
	    (ring/redirect (str "/invoice/quarter/" (:year request) "/" (:quarter request))))
	
	(cc/GET "/invoice/yearly" []
	    (system/invoice-yearly (current-year)))
	(cc/GET "/invoice/yearly/:year" [year]
	    (system/invoice-yearly year))
	(cc/POST "/invoice/yearly" request
	    (ring/redirect (str "/invoice/yearly/" (:year request))))
	
	(cc/GET "/enter-payments" []
	    (system/enter-payments))
	(cc/GET "/config" []
	    (system/config))
	(cc/GET "/list-members" []
	    (system/list-members))
	(cc/GET "/list-all" []
	    (system/list-all))

	(cc/GET "/new-member" []
	    (members/new-member))
	(cc/GET "/choose-member" []
	    (members/choose-member))
	(cc/GET "/edit-member/:id" [id]
	    (members/edit-member id))
	(cc/POST "/update-member" request
		(members/update-member request)
		(ring/redirect "/"))

	(cc/GET "/new-estate" []
	    (estates/new-estate))
	(cc/GET "/choose-estate" []
	    (estates/choose-estate))
	(cc/GET "/edit-estate/:id" [id]
	    (estates/edit-estate id))
	(cc/POST "/update-estate" request
		(estates/update-estate request)
		(ring/redirect "/"))
	(cc/GET "/enter-activities" []
	    (estates/enter-activities))
	(cc/POST "/update-activities" request
		(estates/update-activities request)
		(ring/redirect "/")))
