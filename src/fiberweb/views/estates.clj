(ns fiberweb.views.estates
  	(:require 	(fiberweb 	[db         :as db])
  				(fiberweb.views [layout     :as layout]
            					[common     :as common])
 	          	(garden 	[core       :as g]
            				[units      :as u]
            				[selectors  :as sel]
            				[stylesheet :as ss]
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

(defn edit-estate
	[estate]
	(let [owner (db/get-owner estate)]
		(layout/common "Ändra en Fastighet" []
		[:table
			[:tr [:td
				[:table
					[:tr
						[:td (hf/label :xx "FastighetsID:")]
						[:td (hf/label :xx (:estateid estate))]]
					[:tr
						[:td (hf/label :xx "Betäckning:")]
						[:td (hf/text-field :location (:location estate))]]
					[:tr
						[:td (hf/label :xx "Adress:")]
						[:td (hf/text-field :address (:address estate))]]
					[:tr
						[:td (hf/label :xx "Ägare:")]
						[:td (hf/label :xx (:name owner))]]
					[:tr
						[:td (hf/label :xx "Notering:")]
						[:td (hf/text-field :note (:note estate))]]]]]
			[:tr [:td
				[:table
					[:tr
						[:th (hf/label :xx "Medlemskapet började")]
						[:th (hf/label :xx "Medlemskapet slutade")]]
					[:tr
						[:td (hf/label :xx "")]
						[:td (hf/label :xx "Avsluta?")
							 (hf/check-box :endestate (some? (:toyear estate)))]]
					[:tr
						[:td (hf/text-field :fromyear (str (:fromyear estate)))
							 (hf/text-field :frommonth (str (:frommonth estate)))]
						[:td (hf/text-field :toyear (str (:toyear estate)))
							 (hf/text-field :tomonth (str (:tomonth estate)))]]]]]
			[:tr [:td
				[:table
					[:tr
						[:th (hf/label :xx "Betalningsinterval")]]
					[:tr
						[:td [:div ]]]
					[:tr
						[:td (hf/label :xx "2017 och framåt")]
						[:td "Helar" (hf/check-box :yearly)]]]]]
			[:tr [:td
				[:table
					[:tr
						[:th (hf/label :xx "Införd")]
						[:th (hf/label :xx "Typ")]
						[:th (hf/label :xx "Belopp")]
						[:th (hf/label :xx "Moms")]
						[:th (hf/label :xx "År")]
						[:th (hf/label :xx "Ta bort")]
					]
					(map #([:tr
						[:td (hf/label :xx (:date %))]
						[:td (hf/drop-down :xx ["Medlemsavgift" "Betalning"] (:type %))]
						[:td (hf/text-field :xx (:amount %))]
						[:td (hf/text-field :xx (:tax %))]
						[:td (hf/text-field :xx (:year %))]
						[:td (hf/check-box :xx)]])
						(db/get-estate-dcs estate))]]]])))

(defn new-estate
	[]
	(layout/common "Ändra en Fastighet" []
		[:table
			[:tr [:td
				[:table
					[:tr
						[:td (hf/label :xx "FastighetsID:")]
						[:td (hf/text-field :estateid "")]]
					[:tr
						[:td (hf/label :xx "Betäckning:")]
						[:td (hf/text-field :location "")]]
					[:tr
						[:td (hf/label :xx "Adress:")]
						[:td (hf/text-field :address "")]]
					[:tr
						[:td (hf/label :xx "Notering:")]
						[:td (hf/text-field :note "")]]]]]
			[:tr [:td
				[:table
					[:tr
						[:th (hf/label :xx "Medlemskapet började")]]
					[:tr
						[:td (hf/text-field :fromyear ())
							 (hf/text-field :frommonth ())]]]]]
			[:tr
				[:td (hf/label :xx "Betalningsinterval")]]
			[:tr
				[:td (hf/label :xx "2017 och framåt")]
				[:td "Helar" (hf/check-box :yearly)]]]))
			
(defn choose-estate
	[])

(defn update-estate
	[])

(defn enter-activities
	[])

(defn update-activities
	[])

