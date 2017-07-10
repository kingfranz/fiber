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

(defn get-mf-dc
	[mid year]
	(some->> (db/get-memberdcs mid)
			 (filter #(= (:type %) "membership-fee"))
			 (filter #(= (:year %) year))))

(defn update-membership-fees
	[year]
	(let [members     (db/get-members year)
		  members-dcs (map #(assoc % :dc (get-mf-dc (:memberid %) year)) members)
		  members-owe (filter #(empty? (:dc %)) members-dcs)
		  config      (db/get-config-at year)]
		(doseq [member members-owe]
			(db/add-memberdc {:amount (:membershipfee config)
							  :tax    (:membershiptax config)
							  :type   "membership-fee"
							  :year   year
							  :memberid (:memberid member)}))))

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
					[:td (hf/drop-down :newyear (range 2010 2020) year)]
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
				[:td.rpad.rafield.tafield (hf/label :xx (:memberid x))]
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
				]) (db/get-usage-data year (utils/q->b 0) 12))]))

(defn mk-rows
	[year]
	(flatten (map (fn [m]
		[{:pos (:memberid m) :id (:memberid m) :name (:name m)
		 :address ""
		 :total (:total m)
		 :key (keyword (str "member-" (:memberid m)))}
		(map-indexed (fn [i e]
			{:pos (+ (:memberid m) (/ i 10))
			 :id ""
			 :name ""
			 :address (:address e)
			 :total (:total e)
			 :key (keyword (str "member-" (:memberid m) "-" (:estateid e)))})
			(:estates m))])
		(db/get-full year))))

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
						[:td.tafield.rafield.rpad (tbrdr x) (hf/label :xx (:id x))]
						[:td.tafield.udpad.w400 (tbrdr x) (hf/label :xx (:name x))]
						[:td.tafield.udpad.w400 (tbrdr x) (hf/label :xx (:address x))]
						[:td.tafield.rafield.w150 (tbrdr x) (hf/label :xx (:total x))]
						[:td.tafield.w150 (tbrdr x)
							(when (< (:total x) 0M)
								(hf/text-field {:class "w150"} (:key x) ""))]
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
	(let [m-keys (->> params
		              keys
		              (filter #(re-matches member-regex (name %)))
		              (map #(key->m params %))
		              (remove #(nil? (:amount %))))
		  e-keys (->> params
		              keys
		              (filter #(re-matches estate-regex (name %)))
		              (map #(key->e params %))
		              (remove #(nil? (:amount %))))]
		(doseq [m-entry m-keys]
			(db/add-member-payment (:memberid m-entry)
								   (:amount m-entry)
								   (Integer/valueOf (:current-year params))))
		(doseq [e-entry e-keys]
			(db/add-estate-payment (:estateid e-entry)
								   (:amount e-entry)
								   (Integer/valueOf (:current-year params))))
		))

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

<<<<<<< 35b96e22334c1f5fb988d6dfba4021266c45d5e8
(defn get-c
	[x i]
	(if (>= i (count (:contacts x)))
		""
		(:value (nth (:contacts x) i))))

(def css-lists
	(g/css
		[:.tbl-brdr {:border-left [[(u/px 1) :grey :solid]]
			         :border-top [[(u/px 1) :grey :solid]]
			         :border-right [[(u/px 1) :grey :solid]]}]
		[:.tbl-brdr1 {:border-left [[(u/px 1) :grey :solid]]
			         :border-right [[(u/px 1) :grey :solid]]}]
		[:.tbl-co {:border-collapse :collapse}]
		[:.txtcol {:width (u/px 400)}]
		[:.ccol {:word-wrap :break-word :width (u/px 250)}]
		[:.dcol {:width (u/px 250)}]
		[:.brdr {:border [[(u/px 1) :grey :solid]]}]
		[:.brdrcol {:border-collapse :collapse}]
		))

(defn list-members
	[]
	(layout/common "Medlemslista" [css-lists]
		[:table
			[:tr
				[:td [:a.link-head {:href "/"} "Home"]]
				[:td [:a.link-head {:href "/export-members-csv"} "Exportera CSV"]]
				[:td [:a.link-head {:href "/export-members-pdf"} "Exportera PDF"]]]
			[:tr [:td {:height 40}]]]
		[:table.brdrcol
			[:tr
				[:th (hf/label :xx "ID")]
				[:th.txtcol (hf/label :xx "Namn")]
				[:th (hf/label :xx "Från")]
				[:th (hf/label :xx "Kontakt")]
				[:th (hf/label :xx "Kontakt")]
				[:th (hf/label :xx "Note")]
			]
			(map (fn [x]
				[:tr
				[:td.rafield.rpad.brdr (hf/label :xx (:memberid x))]
				[:td.txtcol.brdr (hf/label {:class "txtcol"} :xx (:name x))]
				[:td.dcol.brdr (hf/label :xx (str (utils/get-year (:fromyear x)) "-"
					                    (utils/get-month (:fromyear x))))]
				[:td.brdr.ccol (hf/label :xx (get-c x 0))]
				[:td.brdr.ccol (hf/label :xx (get-c x 1))]
				[:td.brdr (hf/label :xx (:note x))]]) (db/get-member-cont))
				]))


(defn mk-all-lst
	[]
	(vec (map (fn [m]
		[(str (:memberid m))
		 (str (utils/get-year (:m-fromyear m)) "-" (utils/get-month (:m-fromyear m)))
		 (:name m)
		 (:contact m)
		 (str (:estateid m))
		 (:location m)
		 (:address m)
		 (str (:bimonths m))
		 (str (utils/get-year (:e-fromyear m)) "-" (utils/get-month (:e-fromyear m)))
		 ]) (db/get-all))))

(defn list-all
	[]
	(layout/common "Hela listan" [css-lists]
		[:table
			[:tr
				[:td [:a.link-head {:href "/"} "Home"]]
				[:td [:a.link-head {:href "/export-all-csv"} "Exportera CSV"]]
				[:td [:a.link-head {:href "/export-all-pdf"} "Exportera PDF"]]]
			[:tr [:td {:height 50} ""]]]
		[:table.brdrcol
			[:tr
				[:th (hf/label :xx "M-ID")]
				[:th (hf/label :xx "Från")]
				[:th (hf/label :xx "Namn")]
				[:th (hf/label :xx "Kontakt")]
				[:th (hf/label :xx "E-ID")]
				[:th (hf/label :xx "Benämning")]
				[:th (hf/label :xx "Adress")]
				[:th (hf/label :xx "Fak")]
				[:th (hf/label :xx "Från")]
			]
			(map (fn [x]
				[:tr
				[:td.rafield.rpad.brdr (hf/label :xx (:memberid x))]
				[:td.dcol.brdr (hf/label :xx (str (utils/get-year (:m_fromyear x)) "-"
					                    (utils/get-month (:m_fromyear x))))]
				[:td.txtcol.brdr (hf/label :xx (:name x))]
				[:td.brdr.ccol (hf/label :xx (:contact x))]
				[:td.rafield.rpad.brdr (hf/label :xx (:estateid x))]
				[:td.txtcol.brdr (hf/label :xx (:location x))]
				[:td.txtcol.brdr (hf/label :xx (:address x))]
				[:td.rafield.rpad.brdr (hf/label :xx (:bimonths x))]
				[:td.dcol.brdr (hf/label :xx (str (utils/get-year (:e_fromyear x)) "-"
					                    (utils/get-month (:e_fromyear x))))]
				]) (db/get-all))
				]))

(defn export-all-csv
	[]
	(with-open [out-file (io/writer "fulllista.csv")]
		(csv/write-csv out-file (mk-all-lst))))

(defn export-all-pdf
	[]
	(let [id-width 3
		  from-width 5
		  loc-width 12
		  fsize 10]
		(pdf/pdf [
		  	{:title (str "Full lista. Utskriven " (utils/now-str))
		     :header (str "Full lista. Utskriven " (utils/now-str))
		     :pages true
		    }
		    (into [:table
		     	{:header [
		     		{:backdrop-color [220 220 220]}
	     	         [:paragraph {:style :bold :size fsize :align :center} "ID"]
	     	         [:paragraph {:style :bold :size fsize} "Från"]
	     	         [:paragraph {:style :bold :size fsize} "Namn"]
	     	         [:paragraph {:style :bold :size fsize} "Kontakt"]
	     	         [:paragraph {:style :bold :size fsize :align :center} "ID"]
	     	         [:paragraph {:style :bold :size fsize} "Benämning"]
	     	         [:paragraph {:style :bold :size fsize} "Adress"]
	     	         [:paragraph {:style :bold :size fsize} "Fakturering"]
	     	         [:paragraph {:style :bold :size fsize} "Från"]
		     		]
			     	 :widths [id-width from-width 20
			     	 		  22
			     	 		  id-width loc-width 23 7 from-width]
			     	 :num-cols 9}]
		   		(mk-all-lst))
		   	]
		  	"fulllista.pdf")))

(defn mk-member-lst
	[]
	(vec (map (fn [m]
		[(str (:memberid m)) (:name m)
		 (str (utils/get-year (:fromyear m)) "-" (utils/get-month (:fromyear m)))
		 (get-c m 0) (get-c m 1) (get-c m 2) (get-c m 3)
		 (:note m)]) (db/get-member-cont))))

(defn export-members-csv
	[]
	(with-open [out-file (io/writer "medlemslista.csv")]
		(csv/write-csv out-file (mk-member-lst))))

(defn export-members-pdf
	[]
	(pdf/pdf [
	  	{:title (format "Medlemslista. Utskriven %s" (utils/now-str))
	     :header (format "Medlemslista. Utskriven %s" (utils/now-str))
	     :pages true
	    }
	    (into [:table
	     	{:header [
	     		{:backdrop-color [220 220 220]}
	     	    [:paragraph {:style :bold :size 15 :align :center} "ID"]
	     	    [:paragraph {:style :bold :size 15} "Namn"]
	     	    [:paragraph {:style :bold :size 15} "Från"]
	     	    [:paragraph {:style :bold :size 15} "Kontakt"]
	     		[:paragraph {:style :bold :size 15} "Kontakt"]
	     		[:paragraph {:style :bold :size 15} "Kontakt"]
	     		[:paragraph {:style :bold :size 15} "Kontakt"]
	     		[:paragraph {:style :bold :size 15} "Note"]
	     		]
	     	 	:widths [10 45 45 45 45 45 45 45]}]
	   		(mk-member-lst))
	   	]
	  	"medlemslista.pdf"))

;;-----------------------------------------------------------------------------

(defn months->arr
	[m]
	(bits->months m (q->b 0)))

(defn date-frmtr
	[k v]
	(if (or (= k :date) (= k :from) (and (= k :to) (some? v)) (and (= k :_id) (= (type v) org.joda.time.DateTime)))
		(str "ISODate('" (f/unparse (f/with-zone (f/formatters :date-time-no-ms) (t/default-time-zone)) v) "')")
		v))

(defn trans-dc
	[dc]
	(let [dc* {
		  :date    (c/from-sql-time (:date dc))
		  :amount  (double (:amount dc))
		  :tax     (double (:tax dc))
		  :dc-type (keyword (:type dc))
		  :year    (:year dc)}
		  dc+ (if (some? (:months dc))
				(assoc dc* :months (months->arr (:months dc)))
				dc*)]
		(if (some nil? (vals dc+))
			(throw (ex-info (str "trans-dc:" dc "\n\n" dc+) {:cause :wrong}))
			dc+)))

(defn trans-ft
	[f t]
	{:from (t/date-time (utils/get-year f) (utils/get-month f) 1)
	 :to   (when (some? t)
	 			(t/date-time (utils/get-year t) (utils/get-month t) 1))})

(defn export-mongo-estate
	[]
	(let [estates    (db/get-estates)
		  estatebis  (db/get-estatebi-all)
		  estateacts (db/get-activities-all)
		  estatedcs  (db/get-estatedcs-all)
		  mes        (db/get-membersestates)
		  members    (db/get-members)]
		(spit "estates.json" 
			(str/replace (json/write-str 
				(vec 
					(for [estate estates]
						{:_id               (str "estate-" (:estateid estate))
						 :location          (:location estate)
						 :address           (:address estate)
						 :from-to           (trans-ft (:fromyear estate) (:toyear estate))
						 :activities        (->> estateacts
						 						 (filter #(= (:estateid %) (:estateid estate)))
						 						 (mapv (fn [act] {:year (:year act)
						 						 				  :months (months->arr (:actmonths act))})))
						 :billing-intervals (->> estatebis
						 						 (filter #(= (:estateid %) (:estateid estate)))
						 						 (mapv (fn [bi] {:year (:year bi)
						 						 				 :interval (if (= (:bimonths bi) 12) :yearly :quarterly)})))
						 :dcs               (->> estatedcs
						 						 (filter #(= (:estateid %) (:estateid estate)))
						 						 (mapv trans-dc))
						 :owners            (->> mes
						 						 (filter #(= (:estateid %) (:estateid estate)))
						 						 (mapv (fn [me] {:_id (str "member-" (:memberid me))
						 						 				 :name (->> members
						 						 				 			(filter #(= (:memberid %) (:memberid me)))
						 						 				 			first
						 						 				 			:name)
						 						 				 :from-to (trans-ft (:fromyear me) (:toyear me))})))
						 :note              (:note estate)}))
				:value-fn date-frmtr) #"\"ISODate\(([^)]+)\)\"" "ISODate($1)"))))

;"ISODate('2013-01-01T01:00:00+01:00')"

(defn export-mongo-member
	[]
	(let [members   (db/get-members)
		  mes       (db/get-membersestates)
		  estates   (db/get-estates)
		  contacts* (db/get-contacts-all)
		  memberdcs (db/get-memberdcs-all)]
		(spit "members.json" 
			(str/replace (json/write-str 
				(vec 
					(for [member members]
			   			{:_id      (str "member-" (:memberid member))
						 :name     (:name member)
						 :from-to  (trans-ft (:fromyear member) (:toyear member))
						 :contacts (->> contacts*
						 				(filter #(= (:memberid %) (:memberid member)))
						 				(map #(select-keys % [:type :value :preferred]))
						 				(map #(update % :type keyword))
						 				(mapv (fn [c] (update c :preferred #(= % 1)))))
						 :estates  (->> mes
									   (filter #(= (:memberid %) (:memberid member)))
									   (mapv (fn [me] {:_id (str "estate-" (:estateid me))
									   			       :address (->> estates
						 						 					 (filter #(= (:estateid %) (:estateid me)))
						 						 					 first
						 						 					 :address)
						 						 	   :from-to (trans-ft (:fromyear me) (:toyear me))})))
						 :dcs      (->> memberdcs
					 				   (filter #(= (:memberid %) (:memberid member)))
					 				   (mapv trans-dc))
						 :note     (:note member)}))
				:value-fn date-frmtr) #"\"ISODate\(([^)]+)\)\"" "ISODate($1)"))))

(defn export-mongo-config
	[]
	(spit "configs.json" 
		(str/replace (json/write-str 
			(vec 
				(for [config (db/get-configs)]
					{:_id            (c/from-sql-time (:entered config))
					 :from           (t/date-time (utils/get-year (:fromyear config)) 
			 			   		                  (utils/get-month (:fromyear config)) 1)
					 :membership-fee (:membershipfee config)
					 :membership-tax (* (:membershiptax config) (:membershipfee config))
					 :connection-fee (:connectionfee config)
					 :connection-tax (* (:connectiontax config) (:connectionfee config))
					 :operator-fee   (:operatorfee config)
					 :operator-tax   (* (:operatortax config) (:operatorfee config))
					 :entry-fee      25000.0
					 :entry-tax      0.0}))
			:value-fn date-frmtr) #"\"ISODate\(([^)]+)\)\"" "ISODate($1)")))

(defn export-json
	[]
	(export-mongo-config)
	(export-mongo-member)
	(export-mongo-estate)
	)

;;-----------------------------------------------------------------------------
=======
>>>>>>> work in progress
