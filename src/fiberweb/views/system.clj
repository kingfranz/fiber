(ns fiberweb.views.system
  	(:require 	(fiberweb 		[db         :as db]
  								[utils      :as utils])
  				(fiberweb.views [layout     :as layout]
            					[common     :as common])
 	          	(garden 		[core       :as g]
            					[units      :as u]
            					[selectors  :as sel]
            					[stylesheet :as ss]
            					[color      :as color])
             	(clj-time 		[core       :as t]
            					[local      :as l]
            					[format     :as f]
            					[periodic   :as p])
             	(clj-pdf 		[core       :as pdf])
             	(clojure.data 	[csv        :as csv])
            	(hiccup 		[core       :as h]
            					[def        :as hd]
            					[element    :as he]
            					[form       :as hf]
            					[page       :as hp]
            					[util       :as hu])
            	(taoensso 		[timbre     :as timbre])
            	(clojure.java 	[io         :as io])
            	(clojure 		[string     :as str]
            					[spec       :as s]
            					[set        :as set])))

;;-----------------------------------------------------------------------------

(defn calc-member-sum
	[m year]
	(let [dcs (filter #(= (:year %) year) (:dcs m))
		  fee (reduce (fn [f1 f2] {:amount (+ (:amount f1) (:amount f2))
		  						   :tax (+ (:tax f1) (:tax f2))}) {:amount 0M :tax 0M}
		  		(filter #(= (:dc-type %) :membership-fee) dcs))
		  pay (reduce + 0M (map :amount (filter #(= (:dc-type %) :payment) dcs)))]
		{:feeamount (:amount fee)
		 :feetax    (:tax fee)
		 :payamount pay
		 :total     (+ (:amount fee) (:tax fee) pay)}))
(s/fdef calc-member-sum
	:args (s/cat :m :fiber/member :year :fiber/year)
	:ret  :fiber/member-sum)

(defn get-membership-data
	[year]
	(->> (db/get-members)
		 (filter #(utils/within (:from-to %) year 1 1))
		 (map #(merge % (calc-member-sum % year)))
		 (remove #(= (:total %) 0M))))
(s/fdef get-membership-data
	:args :fiber/year
	:ret  (s/* (s/merge :fiber/member :fiber/member-sum)))

;;-----------------------------------------------------------------------------

(defn estate-debt
	[estate year months]
	(let [dcs (filter #(and (= (:year %) year)
							(not-empty (set/intersection months (:months %))))
				(:dcs estate))
		  conamount (->> dcs
		  				 (filter #(= (:dc-type %) :connection-fee))
		  				 (map :amount)
		  				 (reduce +))
		  contax    (->> dcs
		  				 (filter #(= (:dc-type %) :connection-fee))
		  				 (map :tax)
		  				 (reduce +))
		  opamount  (->> dcs
		  				 (filter #(= (:dc-type %) :operator-fee))
		  				 (map :amount)
		  				 (reduce +))
		  optax     (->> dcs
		  				 (filter #(= (:dc-type %) :operator-fee))
		  				 (map :tax)
		  				 (reduce +))
		  payamount (->> dcs
		  				 (filter #(= (:dc-type %) :payment))
		  				 (map :amount)
		  				 (reduce +))]
		{:conamount conamount
		 :contax    contax
		 :opamount  opamount
		 :optax     optax
		 :payamount payamount
		 :total     (+ conamount contax opamount optax payamount)}))
(s/fdef estate-debt
	:args (s/cat :estate :fiber/estate :year :fiber/year :months :fiber/months)
	:ret  :fiber/estate-sum)

(defn get-usage-data
	[year quarter y-or-q]
	(let [months (set (range 1 (inc (* quarter 3))))]
		(->> (get-estates)
			 (filter #(utils/within (:from-to %) year))
			 (filter (fn [e] (= (get (utils/get-year year (:billing-intervals e)) :interval) y-or-q)))
			 (filter (fn [e] (not-empty (set/intersection months (get (utils/get-year year (:activities e)) :months)))))
			 (map #(merge % (estate-debt % year months)))
			 (remove #(= (:total %) 0M)))))
(s/fdef get-usage-data
	:args (s/cat :year :fiber/year :quarter (s/int-in 1 5) :y-or-q :estate/interval)
	:ret  (s/* (s/merge :fiber/estate :fiber/estate-sum)))

;;-----------------------------------------------------------------------------

(defn update-membership-fees
	[year]
	(let [members     (filter #(utils/within (:from-to %) year 1 1) (db/get-members))
		  members-dcs (map (fn [m] 
		  				{:_id (:_id m) :dc (filter #(and (= (:year %) year)
		  												 (= (:dc-type %) :membership-fee)) (:dcs m))})
		  				members)
		  members-owe (filter #(empty? (:dc %)) members-dcs)
		  config      (db/get-config-at year)]
		(doseq [member members-owe]
			(db/add-memberdc (:_id member)
				{:date   (l/local-now)
				 :amount (:membershipfee config)
				 :tax    (:membershiptax config)
				 :type   :membership-fee
				 :year   year}))))

(defn invoice-membership
	[year]
	(update-membership-fees year)
	(layout/common (str "Medlemsavgifter " year) []
		(hf/form-to
			[:post "/invoice/membership"]
        	[:table
				[:tr
					[:td [:a.link-head {:href "/"} "Home"]]]
				[:tr
					[:td (hf/label :xx "Medlemsavgifter för år")]
					[:td (hf/drop-down :newyear (range config/min-year config/max-year) year)]
					[:td (hf/submit-button {:class "button1 button"} "Updatera")]]])
		[:table
			[:tr
				[:th (hf/label :xx "ID")]
				[:th (hf/label :xx "Namn")]
				[:th (hf/label :xx "Kontakt")]
				[:th (hf/label :xx "Belopp")]
				[:th (hf/label :xx "Moms")]
				[:th (hf/label :xx "Totalt")]
			]
			(map (fn [x]
				[:tr
				[:td.rpad.rafield.tafield (hf/label :xx (:_id x))]
				[:td.udpad.tafield {:width 400} (hf/label :xx (:name x))]
				[:td.udpad.tafield {:width 400} (hf/label :xx (:contact x))]
				[:td.rafield.tafield {:width 100} (hf/label :xx (:amount x))]
				[:td.rafield.tafield {:width 100} (hf/label :xx (:tax x))]
				[:td.rafield.tafield {:width 100} (hf/label :xx (:total x))]
				]) (db/get-membership-data year))]))

(defn mk-months-upto
	[quarter]
	(if (zero? quarter)
		0
		(+ (utils/q->b quarter) (mk-months-upto (dec quarter)))))

(defn get-edcs
	[id year qmonths dc-type]
	(->> (db/get-estatedcs id)
		 (filter #(and (= (:year %) year)
					   (= (:type %) dc-type)
				       (not (zero? (bit-and (:months %) qmonths)))))
		 (map #(utils/bit->month (:months %)))))

(defn update-estate-q-fixed
	[year quarter yearly?]
	(let [eids   (->> (db/get-estatebi-year year)
					  (filter #(= (:bimonths %) (if yearly? 12 3)))
					  (map :estateid))
		  qmonths (mk-months-upto quarter)
	      efees  (map (fn [id] {:id id :dcs (get-edcs id year qmonths "connection-fee")}) eids)
	      eowe   (remove #(= (count (:dcs %)) (* quarter 3)) efees)
	      config (db/get-config-at year)]
        (doseq [e eowe]
        	(doseq [m (range 1 13)
        		:when (not (some #{m} (set (:dcs e))))]
        		(db/add-estatedc {:amount   (:connectionfee config)
        						  :tax      (:connectionfee config)
        						  :type     "connection-fee"
        						  :year     year
        						  :months   (utils/month->bit m)
        						  :estateid (:id e)})))))

(defn update-estate-q-activity
	[year quarter yearly?]
	(let [eids   (->> (db/get-estatebi-year year)
					  (filter #(= (:bimonths %) (if yearly? 12 3)))
					  (map :estateid))
	      qmonths (mk-months-upto quarter)
	      efees  (map (fn [id] {:id   id
	      						:dcs  (get-edcs id year qmonths "operator-fee")
	      						:acts (utils/bits->months (db/get-activities-for year id) qmonths)}) eids)
	      eowe   (remove #(= (count (:dcs %)) (count (:acts %))) efees)
	      config (db/get-config-at year)]
        (doseq [e eowe]
        	(doseq [m (range 1 13)
        		:when (and (not (some #{m} (set (:dcs e))))
        				   (some #{m} (set (:acts e))))]
        		(db/add-estatedc {:amount   (:operatorfee config)
        						  :tax      (:operatorfee config)
        						  :type     "operator-fee"
        						  :year     year
        						  :months   (utils/month->bit m)
        						  :estateid (:id e)})))))

(defn update-estates-quarter
	[year quarter]
	(update-estate-q-fixed year quarter false)
	(update-estate-q-activity year quarter false))

(defn invoice-quarter
	[year quarter]
	(update-estates-quarter year quarter)
	(layout/common (str "Användningsavgifter " year " Q" quarter) []
		(hf/form-to
			[:post "/invoice/quarter"]
        	[:table
				[:tr
					[:td {:colspan 3} [:a.link-head {:href "/"} "Home"]]]
				[:tr [:td {:height 30}]]
				[:tr
					[:th {:colspan 3} (hf/label :xx "Användningsavgifter")]]
				[:tr [:td {:height 10}]]
				[:tr
					[:td {:width 100} (hf/label :xx "För år")]
					[:td.rafield (hf/drop-down :newyear (range 2010 2020) year)]
					[:td {:width 30}]
					[:td {:rowspan 2} (hf/submit-button {:class "button1 button"} "Updatera")]]
				[:tr
					[:td {:width 100} (hf/label :xx "Kvartal")]
					[:td.rafield (hf/drop-down :newquarter [1 2 3 4] quarter)]]
				[:tr [:td {:height 30}]]
				])
		[:table
			[:tr
				[:th (hf/label :xx "ID")]
				[:th (hf/label :xx "Namn")]
				[:th (hf/label :xx "Fastighet")]
				[:th (hf/label :xx "Kontakt")]
				[:th {:colspan 2} (hf/label :xx "Anslutning")]
				[:th {:colspan 2} (hf/label :xx "Operatör")]
				[:th (hf/label :xx "Betalt")]
				[:th (hf/label :xx "Totalt")]
			]
			(map (fn [x]
				[:tr
				[:td.rafield.rpad.tafield (hf/label :xx (:memberid x))]
				[:td.udpad.tafield {:width 400} (hf/label :xx (:name x))]
				[:td.udpad.tafield {:width 300} (hf/label :xx (:address x))]
				[:td.udpad.tafield {:width 250} (hf/label :xx (:contact x))]
				[:td.rafield.tafield {:width 120} (hf/label :xx (:conamount x))]
				[:td.rafield.tafield {:width 120} (hf/label :xx (str "(" (:contax x) ")"))]
				[:td.rafield.tafield {:width 120} (hf/label :xx (:opamount x))]
				[:td.rafield.tafield {:width 120} (hf/label :xx (str "(" (:optax x) ")"))]
				[:td.rafield.tafield {:width 100} (hf/label :xx (:payamount x))]
				[:td.rafield.tafield {:width 100} (hf/label :xx (:total x))]
				]) (db/get-usage-data year (utils/q->b quarter) 3))]))

(defn update-estates-yearly
	[year]
	(update-estate-q-fixed year 4 true)
	(update-estate-q-activity year 4 true))

(defn invoice-yearly
	[year]
	(update-estates-yearly year)
	(layout/common (str "Användningsavgifter " year) []
		(hf/form-to
			[:post "/invoice/yearly"]
        	[:table
				[:tr
					[:td {:colspan 2} [:a.link-head {:href "/"} "Home"]]]
				[:tr [:td {:height 30}]]
				[:tr
					[:td {:colspan 3} (hf/label :xx "Användningsavgifter")]]
				[:tr [:td {:height 5}]]
				[:tr
					[:td (hf/label :xx "För år")]
					[:td (hf/drop-down :newyear (range 2013 2030) year)]
					[:td (hf/submit-button {:class "button1 button"} "Updatera")]]
				[:tr [:td {:height 10}]]])
		[:table {:border 1 :color :grey}
			[:tr
				[:th (hf/label :xx "ID")]
				[:th (hf/label :xx "Namn")]
				[:th (hf/label :xx "Fastighet")]
				[:th (hf/label :xx "Kontakt")]
				[:th {:colspan 2} (hf/label :xx "Anslutning")]
				[:th {:colspan 2} (hf/label :xx "Operatör")]
				[:th (hf/label :xx "Betalt")]
				[:th (hf/label :xx "Totalt")]
			]
			(map (fn [x]
				[:tr
				[:td.rafield.rpad.tafield (hf/label :xx (:_id x))]
				[:td.udpad.tafield {:width 400} (hf/label :xx (:name x))]
				[:td.udpad.tafield {:width 300} (hf/label :xx (:address x))]
				[:td.udpad.tafield {:width 250} (hf/label :xx (:contact x))]
				[:td.rafield.tafield {:width 120} (hf/label :xx (:conamount x))]
				[:td.rafield.tafield {:width 120} (hf/label :xx (str "(" (:contax x) ")"))]
				[:td.rafield.tafield {:width 120} (hf/label :xx (:opamount x))]
				[:td.rafield.tafield {:width 120} (hf/label :xx (str "(" (:optax x) ")"))]
				[:td.rafield.tafield {:width 100} (hf/label :xx (:payamount x))]
				[:td.rafield.tafield {:width 100} (hf/label :xx (:total x))]
				]) (db/get-usage-data year (utils/q->b 0) 12))]))

;;-----------------------------------------------------------------------------

(defn swap-estate
	[m-estates estates]
	(->> m-estates
		 (map (fn [e] (utils/find-first #(= (:_id %) (:_id e)) estates)))
		 (remove (fn [e] (or (nil? e) (= (:total e) 0M))))))

(defn e-sum
	[estates]
	(if (empty? estates)
		0M
		(reduce + 0M (map :total estates))))

(defn mk-rows
	[year]
	(let [members  (get-membership-data year)
		  estates  (merge (get-usage-data year 4 :yearly)
		  				  (get-usage-data year 4 :quarterly))
		  m-and-e* (map (fn [m] (update m :estates (fn [est] (swap-estate est estates)))) members)
		  m-and-e  (remove (fn [m] (and (= (:total m) 0M) (= (e-sum (:estates m)) 0M))) m-and-e)]
		(flatten
			(map (fn [m] [
				{:_id     (:_id m)
				 :name    (:name m)
				 :address ""
				 :total   (:total m)}
				(map (fn [e]
					{:_id     (:_id e)
					 :name    ""
					 :address (:address e)
					 :total   (:total e)})
					(:estates m))])
				m-and-e))))

(def css-pay-brdr
	(g/css
		[:.tbl-brdr {:border-left [[(u/px 1) :grey :solid]]
			         :border-top [[(u/px 1) :grey :solid]]
			         :border-right [[(u/px 1) :grey :solid]]}]
		[:.tbl-brdr1 {:border-left [[(u/px 1) :grey :solid]]
			         :border-right [[(u/px 1) :grey :solid]]}]
		[:.tbl-co {:border-collapse :collapse}]
		[:.w400 {:width (u/px 400)}]
		[:.w150 {:width (u/px 150)}]
		))

(defn tbrdr
	[x]
	(if (seq (:name x)) {:class "tbl-brdr"} {:class "tbl-brdr1"}))

(defn enter-payments
	[year]
	(layout/common (str "Bokför inbetalningar " year) [css-pay-brdr]
		[:table
			[:tr
				[:td [:a.link-head {:href "/"} "Home"]]]
			[:tr [:td {:height 10}]]]
		(hf/form-to
			[:post "/enter-payments"]
        	[:table
				[:tr
					[:td (hf/label :xx "Inbetalningar för år")]
					[:td (hf/drop-down :newyear (range 2010 2020) year)]
					[:td (hf/submit-button {:class "button1 button"} "Updatera")]]
				[:tr [:td {:height 10}]]])
		(hf/form-to
			[:post "/update-payments"]
			(hf/hidden-field :current-year year)
			[:table
				[:tr
					[:td (hf/submit-button {:class "button1 button"} "Uppdatera betalningar")]]
				[:tr [:td {:height 10}]]]
			[:table.tbl-co
				[:tr
					[:th (hf/label :xx "ID")]
					[:th (hf/label :xx "Namn")]
					[:th (hf/label :xx "Fastighet")]
					[:th (hf/label :xx "Skuld")]
					[:th (hf/label :xx "Inbetalt")]
				]
				(map (fn [x]
					[:tr
						[:td.tafield.rafield.rpad (tbrdr x) (hf/label :xx (:_id x))]
						[:td.tafield.udpad.w400   (tbrdr x) (hf/label :xx (:name x))]
						[:td.tafield.udpad.w400   (tbrdr x) (hf/label :xx (:address x))]
						[:td.tafield.rafield.w150 (tbrdr x) (hf/label :xx (:total x))]
						[:td.tafield.w150         (tbrdr x) (hf/text-field {:class "w150"} (:_id x) "")]
					])
					(mk-rows year))])))

(def member-regex #"member-(\d+)")
(def estate-regex #"member-(\d+)-(\d+)")

(defn key->m
	[params k]
	(let [match (re-matches member-regex (name k))]
		(when (some? match)
			{:memberid (Integer/valueOf (second match))
			 :amount (when (seq (get params k))
			 			(BigDecimal. (get params k)))})))

(defn key->e
	[params k]
	(let [match (re-matches estate-regex (name k))]
		(when (some? match)
			{:memberid (Integer/valueOf (nth match 1))
			 :estateid (Integer/valueOf (nth match 2))
			 :amount (when (seq (get params k))
			 			(BigDecimal. (get params k)))})))

(defn update-payment
	[{params :params}]
	(let [m-vals (->> params
		              (filter #(re-matches spec/memberid-regex (name (key %))))
		              (map (fn [me] {:_id (key me) :amount (when (seq (val me)) (BigDecimal. (val me)))}))
		              (remove #(nil? (:amount %))))
		  e-vals (->> params
		              (filter #(re-matches spec/estateid-regex (name (key %))))
		              (map (fn [me] {:_id (key me) :amount (when (seq (val me)) (BigDecimal. (val me)))}))
		              (remove #(nil? (:amount %))))]
		(doseq [m-entry m-vals]
			(db/add-member-payment (:_id m-entry)
								   (:amount m-entry)
								   (Integer/valueOf (:current-year params))))
		(doseq [e-entry e-vals]
			(db/add-estate-payment (:_id e-entry)
								   (:amount e-entry)
								   (Integer/valueOf (:current-year params))))
		))

;;-----------------------------------------------------------------------------

(def css-config
	(g/css
		[:.txtcol {:width (u/px 200) :text-align :right}]
		[:.valcol {:width (u/px 150) :text-align :right}]
		))

(defn config
	[]
	(layout/common "Konfiguration" [css-config]
		(hf/form-to
			[:post "/add-config"]
        	[:table
				[:tr [:td [:a.link-head {:href "/"} "Home"]]]
				[:tr [:td {:height 40}]]
				[:tr
					[:td.txtcol (hf/label :xx "")]
					[:td.valcol (hf/label :xx "Belopp")]
					[:td.valcol (hf/label :xx "Moms%")]]
				[:tr
					[:td.txtcol (hf/label :xx "Medlemsavgift")]
					[:td.valcol (hf/text-field :membership-fee "")]
					[:td.valcol (hf/text-field :membership-tax "")]]
				[:tr
					[:td.txtcol (hf/label :xx "Anslutningsavgift")]
					[:td.valcol (hf/text-field :connection-fee "")]
					[:td.valcol (hf/text-field :connection-tax "")]]
				[:tr
					[:td.txtcol (hf/label :xx "Operatörsavgift")]
					[:td.valcol (hf/text-field :operator-fee "")]
					[:td.valcol (hf/text-field :operator-tax "")]]
				[:tr [:td {:height 40}]]
				[:tr
					[:td.txtcol (hf/label :xx "Gäller från")]
					[:td.valcol (hf/drop-down :fromyear (range 2017 2031) (utils/current-year))]
					[:td.valcol (hf/drop-down :frommonth (range 1 13) (utils/current-month))]]
				[:tr [:td {:height 40}]]
				[:tr
					[:td {:colspan 3} (hf/submit-button {:class "button1 button"} "Lägg till konfiguration")]]])))

(defn add-config
	[{params :params}]
	(db/add-config {:membershipfee (BigDecimal.    (:membership-fee params))
		  			:membershiptax (/ (BigDecimal. (:membership-tax params)) 100M)
		  			:connectionfee (BigDecimal.    (:connection-fee params))
		  			:connectiontax (/ (BigDecimal. (:connection-tax params)) 100M)
		  			:operatorfee   (BigDecimal.    (:operator-fee params))
		  			:operatortax   (/ (BigDecimal. (:operator-tax params)) 100M)
		  			:fromyear      (utils/ym->f (:fromyear params) (:frommonth params))}))

;;-----------------------------------------------------------------------------

