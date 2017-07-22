(ns fiberweb.views.system
  	(:require 	(fiberweb 		[db         :as db]
  								[utils      :as utils]
  								[spec       :as spec]
  								[config     :as config])
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
            	(taoensso 		[timbre     :as log])
            	(clojure.java 	[io         :as io])
            	(clojure 		[string     :as str]
            					[set        :as set])
            	[clojure.spec.alpha :as s]))

;;-----------------------------------------------------------------------------

(s/def :sys/name string?)
(s/def :sys/contact string?)

(defn current-owner
	[estate]
	{:pre [(utils/q-valid? :fiber/estate estate)]
	 :post [(utils/q-valid? (s/nilable :member/_id) %)]}
	(some->> estate :owners (utils/find-first #(-> % :from-to :to nil?)) :_id))

(defn current-owner-info
	[estate]
	{:pre [(utils/q-valid? :fiber/estate estate)]
	 :post [(utils/q-valid? (s/keys :req-un [:sys/name :sys/contact]) %)]}
	(if-let [owner (some-> estate current-owner db/get-member)]
		{:name (:name owner) :contact (-> owner :contacts (common/nth-contact 0) :value)}
		{:name "" :contact ""}))

(defn calc-member-sum
	[m year]
	{:pre [(utils/q-valid? :fiber/member m) (utils/q-valid? :fiber/year year)]
	 :post [(utils/q-valid? :fiber/member-sum %)]}
	(let [dcs (filter #(= (:year %) year) (:dcs m))
		  fee (reduce (fn [f1 f2] {:amount (+ (:amount f1) (:amount f2))
		  						   :tax (+ (:tax f1) (:tax f2))}) {:amount 0 :tax 0}
		  		(filter #(= (:dc-type %) :membership-fee) dcs))
		  pay (reduce + 0.0 (map :amount (filter #(= (:dc-type %) :payment) dcs)))]
		{:fee-amount (:amount fee)
		 :fee-tax    (:tax fee)
		 :pay-amount (double pay)
		 :total      (+ (:amount fee) (:tax fee) pay)}))

(defn get-membership-data
	[year]
	{:pre [(utils/q-valid? :fiber/year year)]
	 :post [(utils/q-valid? (s/* (s/merge :fiber/member :fiber/member-sum)) %)]}
	(->> (db/get-members)
		 (filter #(utils/within (:from-to %) year 1 1))
		 (map #(merge % (calc-member-sum % year)))
		 (remove #(= (:total %) 0.0))))

;;-----------------------------------------------------------------------------

(defn estate-debt
	[estate year months]
	{:pre [(utils/q-valid? :fiber/estate estate) (utils/q-valid? :fiber/year year) (utils/q-valid? :estate/months months)]
	 :post [(utils/q-valid? :fiber/estate-sum %)]}
	(let [dcs (filter #(and (= (:year %) year)
							(not-empty (set/intersection months (:months %))))
				(:dcs estate))
		  con-amount (->> dcs
		  				 (filter #(= (:dc-type %) :connection-fee))
		  				 (map :amount)
		  				 (reduce +))
		  con-tax    (->> dcs
		  				 (filter #(= (:dc-type %) :connection-fee))
		  				 (map :tax)
		  				 (reduce +))
		  op-amount  (->> dcs
		  				 (filter #(= (:dc-type %) :operator-fee))
		  				 (map :amount)
		  				 (reduce +))
		  op-tax     (->> dcs
		  				 (filter #(= (:dc-type %) :operator-fee))
		  				 (map :tax)
		  				 (reduce +))
		  pay-amount (->> dcs
		  				 (filter #(= (:dc-type %) :payment))
		  				 (map :amount)
		  				 (reduce +))]
		{:con-amount (double con-amount)
		 :con-tax    (double con-tax)
		 :op-amount  (double op-amount)
		 :op-tax     (double op-tax)
		 :pay-amount (double pay-amount)
		 :total      (+ con-amount con-tax op-amount op-tax pay-amount)}))

(defn get-usage-data
	[year quarter y-or-q]
	{:pre [(utils/q-valid? :fiber/year year) (utils/q-valid? (s/int-in 1 5) quarter) (utils/q-valid? :estate/interval y-or-q)]
	 :post [(utils/q-valid? (s/* (s/merge :fiber/estate :fiber/estate-sum)) %)]}
	(let [months (set (range 1 (inc (* quarter 3))))]
		(->> (db/get-estates)
			 (filter #(utils/within (:from-to %) year))
			 (filter (fn [e] (= (get (utils/get-year year (:billing-intervals e)) :interval) y-or-q)))
			 (filter (fn [e] (not-empty (set/intersection months (get (utils/get-year year (:activities e)) :months)))))
			 (map #(assoc % :owner (current-owner-info %)))
			 (map #(merge % (estate-debt % year months)))
			 (remove #(= (:total %) 0)))))

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
				{:date    (l/local-now)
				 :amount  (- 0.0 (:membership-fee config))
				 :tax     (- 0.0 (:membership-tax config))
				 :dc-type :membership-fee
				 :year    year}))))

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
					[:td (hf/drop-down :newyear config/year-range year)]
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
					[:td.rafield.tafield {:width 100} (hf/label :xx (:fee-amount x))]
					[:td.rafield.tafield {:width 100} (hf/label :xx (:fee-tax x))]
					[:td.rafield.tafield {:width 100} (hf/label :xx (:total x))]
				])
			(get-membership-data year))]))

(defn months-upto
	[quarter]
	{:pre [(utils/q-valid? (s/int-in 1 5) quarter)]
	 :post [(utils/q-valid? :estate/months %)]}
	(set (range 1 (inc (* quarter 3)))))

(defn get-interval
	[estate year]
	{:pre [(utils/q-valid? :fiber/estate estate) (utils/q-valid? :fiber/year year)]
	 :post [(utils/q-valid? :estate/interval %)]}
	(->> estate
		 :billing-intervals
		 (utils/get-year year)
		 :interval))

(defn get-edcs
	[estate year qmonths dc-type]
	{:pre [(utils/q-valid? :fiber/estate estate)
		   (utils/q-valid? :fiber/year year)
		   (utils/q-valid? :estate/months qmonths)
		   (utils/q-valid? :estate/dc-type dc-type)]
	 :post [(utils/q-valid? :estate/months %)]}
	(->> estate
		 :dcs
		 (filter #(and (= (:year %) year)
					   (= (:dc-type %) dc-type)
				       (seq (set/intersection (:months %) qmonths))))
		 (map #(set/intersection (:months %) qmonths))
		 (reduce set/union #{})))

(defn update-estate-q-fixed
	[year quarter y-or-q]
	{:pre [(utils/q-valid? :fiber/year year) (utils/q-valid? (s/int-in 1 5) quarter) (utils/q-valid? :estate/interval y-or-q)]}
	(let [qmonths (months-upto quarter)
	      eowe    (->> (db/get-estates-at year)
					   (filter #(= (get-interval % year) y-or-q))
					   (map (fn [e] {:_id (:_id e) :dcs (get-edcs e year qmonths :connection-fee)}))
	    			   (remove #(= (count (:dcs %)) (count qmonths))))
	      config (db/get-config-at year)]
        (doseq [e eowe]
        	(doseq [m qmonths
        		:when (not (some #{m} (:dcs e)))
        		:let [new-dc {:date     (l/local-now)
        			 :amount   (- 0.0 (:connection-fee config))
        			 :tax      (- 0.0 (:connection-tax config))
        			 :dc-type  :connection-fee
        			 :year     year
        			 :months   #{m}}]]
        		(s/assert :estate/dc-entry new-dc)
        		(db/add-estatedc (:_id e)
        			new-dc)))))

(defn get-activities-for
	[estate year qmonths]
	{:pre [(utils/q-valid? :fiber/estate estate) (utils/q-valid? :fiber/year year) (utils/q-valid? :estate/months qmonths)]
	 :post [(utils/q-valid? :estate/months %)]}
	(->> estate
		 :activities
		 (utils/get-year year)
		 :months
		 (set/intersection qmonths)))

(defn update-estate-q-activity
	[year quarter y-or-q]
	{:pre [(utils/q-valid? :fiber/year year) (utils/q-valid? (s/int-in 1 5) quarter) (utils/q-valid? :estate/interval y-or-q)]}
	(let [qmonths (months-upto quarter)
	      eowe    (->> (db/get-estates-at year)
					   (filter #(= (get-interval % year) y-or-q))
					   (map (fn [e] {:id   (:_id e)
	      							 :dcs  (get-edcs e year qmonths :operator-fee)
	      							 :acts (get-activities-for e year qmonths)}))
	    			   (remove #(= (count (:dcs %)) (count (:acts %)))))
	      config (db/get-config-at year)]
        (doseq [e eowe]
        	(doseq [m qmonths
        		:when (and (not (some #{m} (set (:dcs e))))
        				   (some #{m} (set (:acts e))))
        		:let [new-dc {:date     (l/local-now)
        			 :amount  (- 0.0 (:operator-fee config))
        			 :tax     (- 0.0 (:operator-tax config))
        			 :dc-type :operator-fee
        			 :year    year
        			 :months  #{m}}]]
        		(s/assert :estate/dc-entry new-dc)
        		(db/add-estatedc (:_id e)
        			new-dc)))))

(defn invoice-quarter
	[year quarter]
	(update-estate-q-fixed year quarter :quarterly)
	(update-estate-q-activity year quarter :quarterly)
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
					[:td.rafield (hf/drop-down :newyear config/year-range year)]
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
					[:td.rafield.rpad.tafield         (hf/label :xx (:memberid x))]
					[:td.udpad.tafield {:width 400}   (hf/label :xx (-> x :owner :name))]
					[:td.udpad.tafield {:width 300}   (hf/label :xx (:address x))]
					[:td.udpad.tafield {:width 250}   (hf/label :xx (-> x :owner :contact))]
					[:td.rafield.tafield {:width 120} (hf/label :xx (:con-amount x))]
					[:td.rafield.tafield {:width 120} (hf/label :xx (str "(" (:con-tax x) ")"))]
					[:td.rafield.tafield {:width 120} (hf/label :xx (:op-amount x))]
					[:td.rafield.tafield {:width 120} (hf/label :xx (str "(" (:op-tax x) ")"))]
					[:td.rafield.tafield {:width 100} (hf/label :xx (:pay-amount x))]
					[:td.rafield.tafield {:width 100} (hf/label :xx (:total x))]
				])
				(get-usage-data year quarter :quarterly))]))

(defn invoice-yearly
	[year]
	(update-estate-q-fixed year 4 :yearly)
	(update-estate-q-activity year 4 :yearly)
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
					[:td (hf/drop-down :newyear config/year-range year)]
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
					[:td.rafield.rpad.tafield         (hf/label :xx (:_id x))]
					[:td.udpad.tafield {:width 400}   (hf/label :xx (-> x :owner :name))]
					[:td.udpad.tafield {:width 300}   (hf/label :xx (:address x))]
					[:td.udpad.tafield {:width 250}   (hf/label :xx (-> x :owner :contact))]
					[:td.rafield.tafield {:width 120} (hf/label :xx (:con-amount x))]
					[:td.rafield.tafield {:width 120} (hf/label :xx (str "(" (:con-tax x) ")"))]
					[:td.rafield.tafield {:width 120} (hf/label :xx (:op-amount x))]
					[:td.rafield.tafield {:width 120} (hf/label :xx (str "(" (:op-tax x) ")"))]
					[:td.rafield.tafield {:width 100} (hf/label :xx (:pay-amount x))]
					[:td.rafield.tafield {:width 100} (hf/label :xx (:total x))]
				])
				(get-usage-data year 4 :yearly))]))

;;-----------------------------------------------------------------------------

(defn swap-estate
	[m-estates estates]
	(->> m-estates
		 (map (fn [e] (utils/find-first #(= (:_id %) (:_id e)) estates)))
		 (remove (fn [e] (or (nil? e) (= (:total e) 0))))))

(defn mk-rows
	[year]
	(let [estates  (merge (get-usage-data year 4 :yearly)
		  				  (get-usage-data year 4 :quarterly))
		  m-and-e  (->> (get-membership-data year)
		  				(map (fn [m] (update m :estates (fn [est] (swap-estate est estates)))))
		  			    (remove (fn [m] (and (= (:total m) 0)
		  									 (= (reduce + 0 (map :total (:estates m))) 0)))))]
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
					[:td (hf/drop-down :newyear config/year-range year)]
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
	(let [values (fn [id-rx] (->> params
		              (filter #(re-matches id-rx (name (key %))))
		              (map (fn [me] {:_id (key me) :amount (when (seq (val me)) (Double/valueOf (val me)))}))
		              (remove #(nil? (:amount %)))))]
		(doseq [m-entry (values spec/memberid-regex)]
			(db/add-member-payment (:_id m-entry)
								   (:amount m-entry)
								   (utils/param->int params :current-year)))
		(doseq [e-entry (values spec/estateid-regex)]
			(db/add-estate-payment (:_id e-entry)
								   (:amount e-entry)
								   (utils/param->int params :current-year)))
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
					[:td.valcol (hf/label :xx "Belopp SEK")]
					[:td.valcol (hf/label :xx "Moms SEK")]]
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
				[:tr
					[:td.txtcol (hf/label :xx "Insats")]
					[:td.valcol (hf/text-field :entry-fee "")]
					[:td.valcol (hf/text-field :entry-tax "")]]
				[:tr [:td {:height 40}]]
				[:tr
					[:td.txtcol (hf/label :xx "Gäller från")]
					[:td.valcol (hf/drop-down :fromyear (utils/curr-year-range) (utils/current-year))]
					[:td.valcol (hf/drop-down :frommonth config/month-range (utils/current-month))]]
				[:tr [:td {:height 40}]]
				[:tr
					[:td {:colspan 3} (hf/submit-button {:class "button1 button"} "Lägg till konfiguration")]]])))

(defn add-config
	[{params :params}]
	(db/add-config {:_id            (l/local-now)
					:membership-fee (Double/valueOf (:membership-fee params))
		  			:membership-tax (Double/valueOf (:membership-tax params))
		  			:connection-fee (Double/valueOf (:connection-fee params))
		  			:connection-tax (Double/valueOf (:connection-tax params))
		  			:operator-fee   (Double/valueOf (:operator-fee params))
		  			:operator-tax   (Double/valueOf (:operator-tax params))
		  			:entry-fee      (Double/valueOf (:entry-fee params))
		  			:entry-tax      (Double/valueOf (:entry-tax params))
		  			:from           (t/date-time (:fromyear params) (:frommonth params) 1)}))

;;-----------------------------------------------------------------------------

