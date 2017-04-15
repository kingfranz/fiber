(ns fiberweb.views.home
  	(:require 	(fiberweb 	[db         :as db])
  				(fiberweb.views [layout     :as layout]
            					[common     :as common])
 	          	(garden 	[core       :as g]
            				[units      :as u]
            				[selectors  :as sel]
            				[stylesheet :as stylesheet]
            				[color      :as color])
             	(clj-time 	[core       :as t]
            				[local      :as l]
            				[format     :as f]
            				[periodic   :as p])
            	(hiccup 	[core       :as h]
            				[def        :as hd]
            				[element    :as he]
            				[form       :as hf]
            				[page       :as hp]
            				[util       :as hu])
            	(clojure 	[string     :as str]
            				[set        :as set])))

;;-----------------------------------------------------------------------------

(defn home-page
	[]
	(layout/common "Myrhult-Bålerud Fiber förening" []
		[:ul.tree
			[:ul "System"
				[:li [:a.link-thick {:href "/invoice/membership"} "Skapa saldolista (medlemsavgift)"]]
				[:li [:a.link-thick {:href "/invoice/quarter"} "Skapa saldolista (kvartals)"]]
				[:li [:a.link-thick {:href "/invoice/yearly"} "Skapa saldolista (helår)"]]
				[:li [:a.link-thick {:href "/enter-payments"} "Bokför inbetalningar"]]
				[:li [:a.link-thick {:href "/config"} "Konfigurera"]]
				[:li [:a.link-thick {:href "/list-members"} "Se medlemslistan"]]
				[:li [:a.link-thick {:href "/list-all"} "Se hela listan"]]]
			[:ul "Medlemmar"
				[:li [:a.link-thick {:href "/new-member"} "Ny medlem"]]
				[:li [:a.link-thick {:href "/choose-member"} "Ändra medlem"]]]
			[:ul "Fastigheter"
				[:li [:a.link-thick {:href "/new-estate"} "Ny fastighet"]]
				[:li [:a.link-thick {:href "/choose-estate"} "Ändra fastighet"]]
				[:li [:a.link-thick {:href "/enter-activities"} "Bokför aktiviteter"]]]]))
