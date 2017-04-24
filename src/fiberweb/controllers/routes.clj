(ns fiberweb.controllers.routes
  	(:require [clojure.string     :as str]
              [ring.util.response :as ring]
              [compojure.core     :as cc]
              (clj-time       [core       :as t]
            	                [local      :as l]
            	                [format     :as f]
            	                [periodic   :as p])
              [fiberweb.views.common   :as common]
              [fiberweb.views.home   :as home]
              [fiberweb.views.members :as members]
              [fiberweb.views.estates  :as estates]
              [fiberweb.views.system  :as system]))

;;-----------------------------------------------------------------------------

(cc/defroutes routes
	(cc/GET "/" []
	    (home/home-page))

	(cc/GET "/invoice/membership" []
	    (system/invoice-membership (common/current-year)))
	(cc/GET "/invoice/membership/:year" [year]
	    (system/invoice-membership year))
	(cc/POST "/invoice/membership" request
	    (ring/redirect (str "/invoice/membership/" (:newyear (:params request)))))
	
	(cc/GET "/invoice/quarter" []
	    (system/invoice-quarter (common/current-year) (common/current-quarter)))
	(cc/GET "/invoice/quarter/:year/:quarter" [year quarter]
	    (system/invoice-quarter (Integer/valueOf year) (Integer/valueOf quarter)))
	(cc/POST "/invoice/quarter" request
	    (ring/redirect (str "/invoice/quarter/" (:newyear (:params request)) "/" (:newquarter (:params request)))))
	
	(cc/GET "/invoice/yearly" []
	    (system/invoice-yearly (common/current-year)))
	(cc/GET "/invoice/yearly/:year" [year]
	    (system/invoice-yearly year))
	(cc/POST "/invoice/yearly" request
	    (ring/redirect (str "/invoice/yearly/" (:newyear (:params request)))))
	
	(cc/GET "/enter-payments" []
	    (system/enter-payments (common/current-year)))
	(cc/GET "/enter-payments/:year" [year]
	    (system/enter-payments year))
	(cc/POST "/enter-payments" request
	    (ring/redirect (str "/enter-payments/" (:newyear (:params request)))))
	(cc/POST "/update-payments" request
		(system/update-payment request)
	    (ring/redirect (str "/enter-payments/" (:current-year (:params request)))))

	(cc/GET "/config" []
	    (system/config))
	(cc/POST "/add-config" request
		(system/add-config request)
	    (ring/redirect "/"))

	(cc/GET "/list-members" []
	    (system/list-members))
	(cc/GET "/export-members-csv" []
	    (system/export-members-csv)
		(ring/redirect "/"))
	(cc/GET "/export-members-pdf" []
	    (system/export-members-pdf)
		(ring/redirect "/"))
	(cc/GET "/list-all" []
	    (system/list-all))
	(cc/GET "/export-all-csv" []
	    (system/export-all-csv)
		(ring/redirect "/"))
	(cc/GET "/export-all-pdf" []
	    (system/export-all-pdf)
		(ring/redirect "/"))

	(cc/GET "/new-member" []
	    (members/new-member))
	(cc/POST "/new-member" request
		(members/create-member request)
		(ring/redirect "/"))

	(cc/GET "/choose-member" []
	    (members/choose-member))

	(cc/GET "/edit-member/:id" [id]
	    (members/edit-member id))
	(cc/POST "/edit-member" request
		(members/update-member request)
		(ring/redirect "/"))

	(cc/GET "/new-estate" []
	    (estates/new-estate))
	(cc/POST "/new-estate" request
		(estates/create-estate request)
		(ring/redirect "/"))

	(cc/GET "/choose-estate" []
	    (estates/choose-estate))

	(cc/GET "/edit-estate/:id" [id]
	    (estates/edit-estate id))
	
	(cc/POST "/update-estate" request
		(estates/update-estate request)
		(ring/redirect "/"))

	(cc/GET "/enter-activities" []
	    (estates/enter-activities (common/current-year)))
	(cc/GET "/enter-activities/:year" [year]
	    (estates/enter-activities year))
	(cc/POST "/enter-activities" request
		(ring/redirect (str "/enter-activities/" (:newyear (:params request)))))

	(cc/POST "/update-activities" request
		(estates/update-activities request)
		(ring/redirect "/")))
