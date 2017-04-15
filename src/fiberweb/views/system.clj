(ns fiberweb.views.system
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

(defn q->b
	[q]
	(nth [2r111111111111 2r111000000000 2r000111000000 2r000000111000 2r000000000111] q))

(defn invoice-membership
	[year]
	(layout/common (str "Medlemsavgifter " year) []
		(hf/form-to
			[:post "/invoice/membership"]
        	[:table
				[:tr
					[:td (hf/label :xx "Medlemsavgifter för år")]
					[:td (hf/drop-down :year (range 2010 2020) year)]
					[:td (hf/submit-button "Updatera")]]])
		[:table
			[:tr
				[:th (hf/label :xx "ID:")]
				[:th (hf/label :xx "Namn")]
				[:th (hf/label :xx "Kontakt")]
				[:th (hf/label :xx "Belopp")]
				[:th (hf/label :xx "Moms")]
				[:th (hf/label :xx "Totalt")]
			]
			(map (fn [x]
				[:tr
				[:td (hf/label :xx (:id x))]
				[:td (hf/label :xx (:name x))]
				[:td (hf/label :xx (:contact x))]
				[:td (hf/label :xx (:amount x))]
				[:td (hf/label :xx (:tax x))]
				[:td (hf/label :xx (:total x))]
				]) (db/get-membership-data year))]))

(defn invoice-quarter
	[year quarter]
	(layout/common (str "Användningsavgifter " year " Q" quarter) []
		(hf/form-to
			[:post "/invoice/quarter"]
        	[:table
				[:tr
					[:td (hf/label :xx "Användningsavgifter för år")]
					[:td (hf/drop-down :year (range 2010 2020) year)]
					[:td (hf/label :xx "Kvartal")]
					[:td (hf/drop-down :quarter [1 2 3 4] quarter)]
					[:td (hf/submit-button "Updatera")]]])
		[:table
			[:tr
				[:th (hf/label :xx "ID")]
				[:th (hf/label :xx "Namn")]
				[:th (hf/label :xx "Fastighet")]
				[:th (hf/label :xx "Kontakt")]
				[:th (hf/label :xx "Anslutning")]
				[:th (hf/label :xx "Operatör")]
				[:th (hf/label :xx "Betalt")]
				[:th (hf/label :xx "Totalt")]
			]
			(map (fn [x]
				[:tr
				[:td (hf/label :xx (:id x))]
				[:td (hf/label :xx (:name x))]
				[:td (hf/label :xx (:address x))]
				[:td (hf/label :xx (:contact x))]
				[:td (hf/label :xx (str (:conamount x) " (" (:contax x) ")"))]
				[:td (hf/label :xx (str (:opamount x) " (" (:optax x) ")"))]
				[:td (hf/label :xx (str (:payamount x)))]
				[:td (hf/label :xx (str (:total x)))]
				]) (db/get-usage-data year (q->b quarter) 3))]))

(defn invoice-yearly
	[year]
	(layout/common (str "Användningsavgifter " year) []
		(hf/form-to
			[:post "/invoice/yearly"]
        	[:table
				[:tr
					[:td (hf/label :xx "Användningsavgifter för år")]
					[:td (hf/drop-down :year (range 2010 2020) year)]
					[:td (hf/submit-button "Updatera")]]])
		[:table
			[:tr
				[:th (hf/label :xx "ID")]
				[:th (hf/label :xx "Namn")]
				[:th (hf/label :xx "Fastighet")]
				[:th (hf/label :xx "Kontakt")]
				[:th (hf/label :xx "Anslutning")]
				[:th (hf/label :xx "Operatör")]
				[:th (hf/label :xx "Betalt")]
				[:th (hf/label :xx "Totalt")]
			]
			(map (fn [x]
				[:tr
				[:td (hf/label :xx (:id x))]
				[:td (hf/label :xx (:name x))]
				[:td (hf/label :xx (:address x))]
				[:td (hf/label :xx (:contact x))]
				[:td (hf/label :xx (str (:conamount x) " (" (:contax x) ")"))]
				[:td (hf/label :xx (str (:opamount x) " (" (:optax x) ")"))]
				[:td (hf/label :xx (str (:payamount x)))]
				[:td (hf/label :xx (str (:total x)))]
				]) (db/get-usage-data year (q->b 0) 12))]))

(defn enter-payments
	[])

(defn config
	[])

(defn list-members
	[])

(defn list-all
	[])




