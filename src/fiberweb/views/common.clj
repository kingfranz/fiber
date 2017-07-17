(ns fiberweb.views.common
  	(:require 	(fiberweb.views [layout     :as layout])
 	          	(fiberweb   	[db         :as db]
 	          					[utils      :as utils]
 	          					[config     :as config]
 	          					[spec       :as spec])
 	          	(ring.util 		[response   :as ring])
                (garden 		[core       :as g]
            					[units      :as u]
            					[selectors  :as sel]
            					[stylesheet :as ss]
            					[color      :as color])
             	(clj-time 		[core       :as t]
            					[local      :as l]
            					[coerce     :as c]
            					[format     :as f]
            					[periodic   :as p])
            	(hiccup 		[core       :as h]
            					[def        :as hd]
            					[element    :as he]
            					[form       :as hf]
            					[page       :as hp]
            					[util       :as hu])
            	(clojure 		[string     :as str]
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

(defn member?
	[m-or-e]
	(some? (re-matches spec/memberid-regex (:_id m-or-e))))

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
							   config/year-range
							   (-> m :from-to :from t/year))]
			[:td (hf/drop-down (utils/mk-tag "frommonth" id) 
							   config/month-range
							   (-> m :from-to :from t/month))]

			[:td (hf/check-box {:class "cb"
								:onclick (str "toggleText('" (utils/mk-tag "endtag" id) "','" 
															 (utils/mk-tag "toyear" id) "','"
															 (utils/mk-tag "tomonth" id) "')")}
							   (utils/mk-tag "endtag" id) 
							   (some? (:toyear m)))]
			
			[:td (hf/drop-down {:disabled (when (-> m :from-to :to nil?) "disabled")}
							   (utils/mk-tag "toyear" id) 
							   config/year-range
							   (some-> m :from-to :to t/year))]
			[:td (hf/drop-down {:disabled (when (-> m :from-to :to nil?) "disabled")}
							   (utils/mk-tag "tomonth" id) 
							   config/month-range
							   (some-> m :from-to :to t/month))]]])

(defn extract-ft
	[params id]
	{:from (t/date-time (Integer/valueOf (get params (utils/mk-tag "fromyear" id)))
						(Integer/valueOf (get params (utils/mk-tag "frommonth" id))) 1)
	 :to   (when (some? (get params (utils/mk-tag "endtag" id)))
	 			(t/date-time (Integer/valueOf (get params (utils/mk-tag "toyear" id)))
	 						 (Integer/valueOf (get params (utils/mk-tag "tomonth" id))) 1))})

;;-----------------------------------------------------------------------------

(defn mk-acts
	[eid months]
	[:table
		[:tr
			(for [i config/month-range]
				[:td {:width 30} (hf/check-box {:class "cb"} (utils/mk-tag eid i) (some #{i} months))])]])
(s/fdef mk-acts
	:args (s/cat :eid :estate/_id :months :estate/months))

(defn show-acts
	[months]
	[:table
		[:tr
			(map (fn [i] [:td {:width 30} (when (some #{i} months) "X")]) config/month-range)]])
(s/fdef show-acts
	:args :estate/months)

(defn get-months
	[id params]
	(set (for [i config/month-range
		:when (some? (get params (utils/mk-tag id i)))]
		i)))
(s/fdef get-months
	:args (s/cat :id :estate/_id :params map?)
	:ret  :estate/months)

;;-----------------------------------------------------------------------------

(defn dc-row
	[m-or-e idx dc]
	(let [ddmap    (if (member? m-or-e) memberdc-map estatedc-map)
		  dc-type  (get ddmap (:dc-type dc))]
		[:tr
			[:td.ddcol.rpad (hf/label {:class "rafield"} :x (some->> (:date dc) utils/ts->d))]
			[:td.rpad       (hf/label {:class "rafield"} :x dc-type)]
			[:td.dcol.rpad  (hf/label {:class "rafield"} :x (:amount dc))]
			[:td.dcol.rpad  (hf/label {:class "rafield"} :x (:tax dc))]
			[:td.dcol       (hf/label {:class "rafield"} :x (:year dc))]
			(when-not (member? m-or-e)
				[:td (show-acts (:months dc))])
			[:td [:a.link-thin {:href (str "/delete-dc/" (:_id m-or-e) "/" idx)} "X"]]
		]))

(defn mk-dc-section
	[m-or-e]
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
		(map-indexed #(dc-row m-or-e %1 %2) (:dcs m-or-e))
		[:tr
			[:td [:a.link-head {:href (str "/new-dc/" (:_id m-or-e))} "Lägg till"]]]])

;;-----------------------------------------------------------------------------

(defn add-dc
	[id]
	(layout/common "Ny Debit/Kredit" []
		(hf/form-to
			[:post "/new-dc"]
			(hf/hidden-field :_id id)
			[:table
				[:tr
					[:td [:a.link-head {:href (str "/edit/" id)} "Tillbaka"]]
					[:td (hf/submit-button {:class "button1 button"} "Utför")]]
				[:tr [:td {:height 40}]]]
			[:table
				[:tr
					[:td (hf/label :xx "Införd")]
					[:td (hf/label :xx "Typ")]
					[:td (hf/label :xx "Belopp")]
					[:td (hf/label :xx "Moms")]
					[:td (hf/label :xx "År")]
					(when-not (member? id)
						[:td (hf/label :xx "Månad")])
				]
				[:tr
					[:td.ddcol.rpad (hf/text-field {:class "rafield"} :date   (utils/today-str))]
					[:td.rpad       (hf/drop-down  {:class "rafield"} :dc-type (vals (if (member? id) memberdc-map estatedc-map)) :payment)]
					[:td.dcol.rpad  (hf/text-field {:class "rafield"} :amount 0M)]
					[:td.dcol.rpad  (hf/text-field {:class "rafield"} :tax    0M)]
					[:td.dcol       (hf/text-field {:class "rafield"} :year   (utils/current-year))]
					(when-not (member? id)
						[:td (mk-acts id 0)])
				]])))

(defn new-dc
	[params]
	(let [dc {:date    (f/parse (:date params))
		      :dc-type (get (if (member? (:_id params)) imemberdc-map iestatedc-map) (:dc-type params))
		      :amount  (utils/param->bigdec params :amount)
		      :tax     (utils/param->bigdec params :tax)
		      :year    (utils/param->int params :year)}]
		(if (member? (:_id params))
			(db/add-memberdc (:_id params) dc)
			(db/add-estatedc (:_id params) (assoc dc :months (get-months (:_id params) params))))
		(ring/redirect (str "/edit/" (:_id params)))))

;;-----------------------------------------------------------------------------

(defn delete-dc
	[id idx]
	(if (member? id)
		(db/delete-memberdc id idx)
		(db/delete-estatedc id idx))
	(ring/redirect (str "/edit/" id)))
	
;;-----------------------------------------------------------------------------

