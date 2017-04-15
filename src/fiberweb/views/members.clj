(ns fiberweb.views.members
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

(defn get-month
	[ym]
	(if (nil? ym)
		""
		(str (int (* (- ym (int ym)) 13)))))

(defn edit-member
	[memberid]
	(let [member (db/get-member memberid)]
		(layout/common "Ändra en medlem" []
		[:table
			[:tr
				[:td
					[:table
						[:tr
							[:td (hf/label :xx "Medlemsnummer:")]
							[:td (hf/label :xx (:memberid member))]]
						[:tr
							[:td (hf/label :xx "Namn:")]
							[:td (hf/label :xx (:name member))]]
						[:tr
							[:td (hf/label :xx "Notering:")]
							[:td (hf/label :xx (:note member))]]]]]
			[:tr
				[:td
					[:table
						[:tr
							[:th (hf/label :xx "Medlemskapet började")]
							[:th (hf/label :xx "Medlemskapet slutade")]]
						[:tr
							[:td (hf/label :xx "")]
							[:td (hf/label :xx "Avsluta?")
								 (hf/check-box :endmember)]]
						[:tr
							[:td (hf/text-field :fromyear (int (:fromyear member)))
								 (hf/text-field :frommonth (get-month (:fromyear member)))]
							[:td (hf/text-field :toyear (int (:toyear member)))
								 (hf/text-field :tomonth (get-month (:toyear member)))]]]]]
			[:tr
				[:td
					[:table
						[:tr
							[:th (hf/label :xx "Fastighets ID")]
							[:th (hf/label :xx "Började År")]
							[:th (hf/label :xx "Började Månad")]
							[:th (hf/label :xx "Avsluta?")]
							[:th (hf/label :xx "Slutade År")]
							[:th (hf/label :xx "Slutade Månad")]]
						(map (fn [x]
							[:tr
								[:td (hf/label :xx (:memberid x))]
								[:td (hf/text-field :xx (int (:fromyear x)))]
								[:td (hf/text-field :xx (get-month (:fromyear x)))]
								[:td (hf/check-box :xx (nil? (:toyear x)))]
								[:td (hf/text-field :xx (int (:toyear x)))]
								[:td (hf/text-field :xx (get-month (:toyear x)))]
								])
							(db/get-estates member))]]]
			[:tr
				[:td
					[:table
						[:tr
							[:th (hf/label :xx "Typ:")]
							[:th (hf/label :xx "Value:")]]
						(map (fn [x]
							[:tr
								[:td (hf/drop-down :xx ["Adress" "E-Post" "Telefon"] (:type x))]
								[:td (hf/text-field :xx (:value x))]])
							(db/get-contacts member))]]]
			[:tr
				[:td
					[:table
						[:tr
							[:th (hf/label :xx "Införd")]
							[:th (hf/label :xx "Typ")]
							[:th (hf/label :xx "Belopp")]
							[:th (hf/label :xx "Moms")]
							[:th (hf/label :xx "År")]
							[:th (hf/label :xx "Ta bort")]
						]
						(map (fn [x]
							[:tr
							[:td (hf/label :xx (:date x))]
							[:td (hf/drop-down :xx ["Medlemsavgift" "Betalning"] (:type x))]
							[:td (hf/text-field :xx (:amount x))]
							[:td (hf/text-field :xx (:tax x))]
							[:td (hf/text-field :xx (:year x))]
							[:td (hf/check-box :xx )]
							]) (db/get-member-dcs member))]]]])))


(defn new-member
	[]
	(layout/common "Ny medlem" []
		[:table
			[:tr [:td
				[:table
					[:tr
						[:td (hf/label :xx "Medlemsnummer:")]
						[:td (hf/text-field :xx "")]]
					[:tr
						[:td (hf/label :xx "Namn:")]
						[:td (hf/text-field :xx "")]]
					[:tr
						[:td (hf/label :xx "Notering:")]
						[:td (hf/text-field :xx "")]]]]]
			[:tr [:td
				[:table
					[:tr
						[:th {:colspan 2} (hf/label :xx "Medlemskapet börjar")]]
					[:tr
						[:td (hf/text-field :fromyear "")
							 (hf/text-field :frommonth "")]]]]]
			[:tr [:td
				[:table
					[:tr
						[:th (hf/label :xx "Fastighets ID")]
						[:th (hf/label :xx "Började År")]
						[:th (hf/label :xx "Började Månad")]]
					[:tr
						[:td (hf/text-field :xx "")]
						[:td (hf/text-field :xx "")]
						[:td (hf/text-field :xx "")]]]]]
			[:tr [:td
					[:table
						[:tr
							[:th (hf/label :xx "Typ:")]
							[:th (hf/label :xx "Value:")]]
						[:tr
							[:td (hf/drop-down :xx ["Adress" "E-Post" "Telefon"] "")]
							[:td (hf/text-field :xx "")]]
						[:tr
							[:td (hf/drop-down :xx ["Adress" "E-Post" "Telefon"] "")]
							[:td (hf/text-field :xx "")]]
						[:tr
							[:td (hf/drop-down :xx ["Adress" "E-Post" "Telefon"] "")]
							[:td (hf/text-field :xx "")]]
						[:tr
							[:td (hf/drop-down :xx ["Adress" "E-Post" "Telefon"] "")]
							[:td (hf/text-field :xx "")]]
						[:tr
							[:td (hf/drop-down :xx ["Adress" "E-Post" "Telefon"] "")]
							[:td (hf/text-field :xx "")]]
						[:tr
							[:td (hf/drop-down :xx ["Adress" "E-Post" "Telefon"] "")]
							[:td (hf/text-field :xx "")]]]]]]))

(defn choose-member
	[]
	(layout/common "Välj medlem" []
		[:table
			[:tr
				[:th (hf/label :xx "ID")]
				[:th (hf/label :xx "Namn")]]
			(map (fn [x]
				[:tr
				[:td (hf/label :xx (str (:memberid x)))]
				[:td [:a {:href (str "/edit-member/" (:memberid x))} (hf/label :xx (str (:name x)))]]])
				(db/get-members))]))

(defn update-member
	[])



