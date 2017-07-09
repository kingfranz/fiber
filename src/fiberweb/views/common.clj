(ns fiberweb.views.common
  	(:require 	(fiberweb.views [layout     :as layout])
 	          	(fiberweb   [db         :as db]
 	          				[utils      :as utils])
 	          	[ring.util.response :as ring]
                (garden 	[core       :as g]
            				[units      :as u]
            				[selectors  :as sel]
            				[stylesheet :as ss]
            				[color      :as color])
             	(clj-time 	[core       :as t]
            				[local      :as l]
            				[coerce     :as c]
            				[format     :as f]
            				[periodic   :as p])
            	(hiccup 	[core       :as h]
            				[def        :as hd]
            				[element    :as he]
            				[form       :as hf]
            				[page       :as hp]
            				[util       :as hu])
            	(clojure 	[string     :as str]
            				[set        :as set]
            				[edn        :as edn]
            				[spec       :as s])))

;;-----------------------------------------------------------------------------

(def memberdc-map (hash-map "membership-fee" "Medlemsavgift"
	                        "payment"        "Inbetalning"))

(def imemberdc-map (set/map-invert memberdc-map))

(def estatedc-map (hash-map "connection-fee" "Anslutningsavgift"
	                        "operator-fee"   "Användningsavgift"
	                        "payment"        "Inbetalning"
	                        "entry-fee"      "Insats"))

(def iestatedc-map (set/map-invert estatedc-map))

(def contact-map (hash-map "address" "Adress"
						   "email"   "E-Post"
						   "phone"   "Telefon"))

(def icontact-map (set/map-invert contact-map))

;;-----------------------------------------------------------------------------

(defn mk-ft
	[title id m header?]
	[:table
		(when header?
			(list
				[:tr
					[:th {:colspan 6} (hf/label :xx title)]]
				[:tr
					[:td (hf/label :xx "ID")]
					[:td {:colspan 2} (hf/label :xx "Började")]
					[:td (hf/label :xx "Avsluta?")]
					[:td {:colspan 2} (hf/label :xx "Slutade")]]))
		[:tr
			[:td.rafield.rpad (hf/label :xx id)]
			[:td (hf/drop-down (utils/mk-tag "fromyear" id) 
							   (range 2013 2031) 
							   (utils/get-year (:fromyear m)))]
			[:td (hf/drop-down (utils/mk-tag "frommonth" id) 
							   (range 1 13)
							   (utils/get-month (:fromyear m)))]

			[:td (hf/check-box {:class "cb"
								:onclick (str "toggleText('" (utils/mk-tag "endtag" id) "','" 
															 (utils/mk-tag "toyear" id) "','"
															 (utils/mk-tag "tomonth" id) "')")}
							   (utils/mk-tag "endtag" id) 
							   (some? (:toyear m)))]
			
			[:td (hf/drop-down {:disabled (when (nil? (:toyear m)) "disabled")}
							   (utils/mk-tag "toyear" id) 
							   (range 2013 2031) 
							   (utils/get-year (:toyear m)))]
			[:td (hf/drop-down {:disabled (when (nil? (:toyear m)) "disabled")}
							   (utils/mk-tag "tomonth" id) 
							   (range 1 13) 
							   (utils/get-month (:toyear m)))]]])

;;-----------------------------------------------------------------------------

(defn mk-acts
	[eid bits]
	[:table
		[:tr
			(map (fn [i]
				[:td {:width 30} (hf/check-box {:class "cb"} (utils/mk-tag eid (inc i)) (bit-test bits i))])
				(range 12))]])

(defn show-acts
	[bits]
	[:table
		[:tr
			(map (fn [i] [:td {:width 30} (when (bit-test bits i) "X")]) (range 12))]])

(defn get-months
	[id params]
	(reduce + (map (fn [i] (if (get params (utils/mk-tag id i))
					(nth [0 1 2 4 8 16 32 64 128 256 512 1024 2048 4096] i)
					0)) (range 1 13))))

;;-----------------------------------------------------------------------------

(defn dc-row
	[id dc member?]
	(let [dcid     (if member? (:memberdcid dc) (:estatedcid dc))
		  ddmap    (if member? memberdc-map estatedc-map)
		  lnk-type (if member? "member" "estate")
		  dc-type  (get ddmap (:type dc))]
		[:tr
			[:td.ddcol.rpad (hf/label {:class "rafield"} :x (some->> (:date dc) utils/ts->d))]
			[:td.rpad       (hf/label {:class "rafield"} :x dc-type)]
			[:td.dcol.rpad  (hf/label {:class "rafield"} :x (:amount dc))]
			[:td.dcol.rpad  (hf/label {:class "rafield"} :x (:tax dc))]
			[:td.dcol       (hf/label {:class "rafield"} :x (:year dc))]
			(when-not member?
				[:td (show-acts (:months dc))])
			[:td [:a.link-thin {:href (str "/delete-dc/" lnk-type "/" id "/" dcid)} "X"]]
		]))

(defn mk-dc-section
	[id dcs member?]
	[:table
		[:tr
			[:th {:colspan 7} (hf/label :xx "Debit - Kredit")]]
		[:tr
			[:td (hf/label :xx "Införd")]
			[:td (hf/label :xx "Typ")]
			[:td (hf/label :xx "Belopp")]
			[:td (hf/label :xx "Moms")]
			[:td (hf/label :xx "År")]
			[:td (hf/label :xx "Ta bort")]
		]
		(map #(dc-row id % member?) dcs)
		[:tr
			[:td [:a.link-head {:href (str "/new-dc/" (if member? "member" "estate") "/" id)} "Lägg till"]]]])

;;-----------------------------------------------------------------------------

(defn add-dc
	[owner-type id]
	(let [dc      {:type "payment" :date (l/local-now) :amount 0M :tax 0M :year 2017}
		  ddmap    (if (= owner-type "member")
		  				memberdc-map
		  				estatedc-map)]
		(layout/common "Ny Debit/Kredit" []
			(hf/form-to
				[:post "/new-dc"]
				(hf/hidden-field :owner-type owner-type)
				(hf/hidden-field :id id)
				[:table
					[:tr
						[:td [:a.link-head {:href (str "/edit-" owner-type "/" id)} "Tillbaka"]]
						[:td (hf/submit-button {:class "button1 button"} "Utför")]]
					[:tr [:td {:height 40}]]]
				[:table
					[:tr
						[:td (hf/label :xx "Införd")]
						[:td (hf/label :xx "Typ")]
						[:td (hf/label :xx "Belopp")]
						[:td (hf/label :xx "Moms")]
						[:td (hf/label :xx "År")]
						(when (= owner-type "estate")
							[:td (hf/label :xx "Månad")])
					]
					[:tr
						[:td.ddcol.rpad (hf/text-field {:class "rafield"} :date   (utils/today-str))]
						[:td.rpad       (hf/drop-down  {:class "rafield"} :type   (vals ddmap) "payment")]
						[:td.dcol.rpad  (hf/text-field {:class "rafield"} :amount 0M)]
						[:td.dcol.rpad  (hf/text-field {:class "rafield"} :tax    0M)]
						[:td.dcol       (hf/text-field {:class "rafield"} :year   (utils/current-year))]
						(when (= owner-type "estate")
							[:td (mk-acts id 0)])
					]]))))

(defn new-dc
	[params]
	(let [id         (:id params)
		  owner-type (:owner-type params)
		  dc-date    (c/to-sql-date (f/parse (:date params)))
		  dc-type    (get (if (= owner-type "member") imemberdc-map iestatedc-map) (:type params))
		  dc-amount  (utils/param->bigdec params :amount)
		  dc-tax     (utils/param->bigdec params :tax)
		  dc-year    (utils/param->int params :year)
		  dc-months  (get-months id params)]
		(if (= owner-type "member")
			(db/add-memberdc {:memberid id :date dc-date :type dc-type
							  :amount dc-amount :tax dc-tax :year dc-year})
			(db/add-estatedc {:estateid id :date dc-date :type dc-type
							  :amount dc-amount :tax dc-tax :year dc-year :months dc-months}))
		(ring/redirect (str (if (= owner-type "member") "/edit-member/" "/edit-estate/") id))))

;;-----------------------------------------------------------------------------

(defn delete-dc
	[owner-type id dcid]
	(if (= owner-type "member")
		(do
			(db/delete-memberdc id dcid)
			(ring/redirect (str "/edit-member/" id)))
		(do
			(db/delete-estatedc id dcid)
			(ring/redirect (str "/edit-estate/" id)))))
	
;;-----------------------------------------------------------------------------

