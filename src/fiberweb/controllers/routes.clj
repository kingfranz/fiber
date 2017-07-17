(ns fiberweb.controllers.routes
  	(:require 	(clojure 		[string   :as str])
            	(ring.util 		[response :as ring])
              	(compojure 		[core     :as cc])
              	(clj-time       [core     :as t]
            	                [local    :as l]
            	                [format   :as f]
            	                [periodic :as p])
              	(fiberweb 		[utils    :as utils])
              	(fiberweb.views [common   :as common]
              					[home     :as home]
              					[members  :as members]
              					[estates  :as estates]
              					[export   :as export]
              					[system   :as system])))

;;-----------------------------------------------------------------------------

(cc/defroutes routes
	(cc/GET "/" []
	    (home/home-page))

	(cc/GET "/new-dc/:type/:id" [type id]
	    (common/add-dc type id))
	(cc/POST "/new-dc" request
		(common/new-dc (:params request)))
	(cc/GET "/delete-dc/:type/:id/:dcid" [type id dcid]
		(common/delete-dc type id dcid))

	(cc/GET "/invoice/membership" []
	    (system/invoice-membership (utils/current-year)))
	(cc/GET "/invoice/membership/:year" [year]
	    (system/invoice-membership year))
	(cc/POST "/invoice/membership" request
	    (ring/redirect (str "/invoice/membership/" (:newyear (:params request)))))
	
	(cc/GET "/invoice/quarter" []
	    (system/invoice-quarter (utils/current-year) (utils/current-quarter)))
	(cc/GET "/invoice/quarter/:year/:quarter" [year quarter]
	    (system/invoice-quarter (Integer/valueOf year) (Integer/valueOf quarter)))
	(cc/POST "/invoice/quarter" request
	    (ring/redirect (str "/invoice/quarter/" (:newyear (:params request)) "/" (:newquarter (:params request)))))
	
	(cc/GET "/invoice/yearly" []
	    (system/invoice-yearly (utils/current-year)))
	(cc/GET "/invoice/yearly/:year" [year]
	    (system/invoice-yearly year))
	(cc/POST "/invoice/yearly" request
	    (ring/redirect (str "/invoice/yearly/" (:newyear (:params request)))))
	
	(cc/GET "/enter-payments" []
	    (system/enter-payments (utils/current-year)))
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
	    (export/list-members))
	(cc/GET "/export-members-csv" []
	    (export/export-members-csv)
		(ring/redirect "/"))
	(cc/GET "/export-members-pdf" []
	    (export/export-members-pdf)
		(ring/redirect "/"))
	(cc/GET "/list-all" []
	    (export/list-all))
	(cc/GET "/export-all-csv" []
	    (export/export-all-csv)
		(ring/redirect "/"))
	(cc/GET "/export-all-pdf" []
	    (export/export-all-pdf)
		(ring/redirect "/"))
	(cc/GET "/exit" []
	    (System/exit 0))

	(cc/GET "/new-member" []
	    (members/new-member nil))
	(cc/POST "/new-member" request
		(members/create-member request)
		(ring/redirect "/"))

	(cc/GET "/choose-member" []
	    (members/choose-member))
	(cc/GET "/add-estate/new/" []
	    (members/choose-estate nil))
	(cc/GET "/add-estate/:mid" [mid]
	    (members/choose-estate mid))

	(cc/GET "/edit-member/:id" [id]
	    (members/edit-member id))
	(cc/POST "/update-member" request
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
	    (estates/enter-activities (utils/current-year)))
	(cc/GET "/enter-activities/:year" [year]
	    (estates/enter-activities year))
	(cc/POST "/enter-activities" request
		(ring/redirect (str "/enter-activities/" (:newyear (:params request)))))

	(cc/POST "/update-activities" request
		(estates/update-activities request)
		(ring/redirect "/")))
