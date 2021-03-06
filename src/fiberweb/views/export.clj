(ns fiberweb.views.export
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
            					[coerce     :as c]
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

; mongoimport -u fiberuser -p "kAllE.kUlA399" --authenticationDatabase fiberdb --db fiberdb --file estates.json --drop --jsonArray

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
				[:th        (hf/label :xx "ID")]
				[:th.txtcol (hf/label :xx "Namn")]
				[:th        (hf/label :xx "Från")]
				[:th        (hf/label :xx "Kontakt")]
				[:th        (hf/label :xx "Kontakt")]
				[:th        (hf/label :xx "Note")]
			]
			(map (fn [x]
				[:tr
					[:td.rafield.rpad.brdr (hf/label :xx (some-> x :_id utils/scrub-id))]
					[:td.txtcol.brdr       (hf/label {:class "txtcol"} :xx (:name x))]
					[:td.dcol.brdr         (hf/label :xx (utils/year-month (:from (:from-to x))))]
					[:td.brdr.ccol         (hf/label :xx (some-> x :contacts (common/nth-contact 0) :value))]
					[:td.brdr.ccol         (hf/label :xx (some-> x :contacts (common/nth-contact 1) :value))]
					[:td.brdr              (hf/label :xx (:note x))]])
			(sort-by #(Integer/valueOf (utils/scrub-id (:_id %))) (db/get-members)))]))

(s/def :exp/estate-ga* (s/keys :req-un [:estate/_id :estate/location :estate/address :fiber/from-to :estate/interval]))

(defn mk-ga-estate
	[m-estate]
	{:pre [(utils/q-valid? :member/estate m-estate)]
	 :post [(utils/q-valid? :exp/estate-ga* %)]}
	(let [estate   (db/get-estate (:_id m-estate))
		  estate*  (select-keys estate [:_id :location :address :from-to])
		  bis      (:billing-intervals estate)
		  bi-entry (utils/get-year (utils/current-year) bis)]
		(assoc estate* :interval (:interval bi-entry))))

(s/def :exp/estate :exp/estate-ga*)
(s/def :exp/estate-ga (s/merge :fiber/member (s/keys :req-un [:exp/estate])))

(defn get-all
	[]
	{:post [(utils/q-valid? (s/* :exp/estate-ga) %)]}
	(->> (for [member (db/get-current-members)]
			(for [m-estate (:estates member)
				:when (-> m-estate :from-to :to nil?)]
				(assoc member :estate (mk-ga-estate m-estate))))
		flatten
		(sort-by #(Integer/valueOf (utils/scrub-id (:_id %))))
		))

(defn mk-all-lst
	[]
	(vec (map (fn [m]
		[(some-> m :_id utils/scrub-id)
		 (some-> m :from-to :from utils/year-month)
		 (some-> m :name)
		 (some-> m :contacts (common/nth-contact 0) :value)
		 (some-> m :estate :_id utils/scrub-id)
		 (some-> m :estate :location)
		 (some-> m :estate :address)
		 (some-> m :estate :interval name)
		 (some-> m :estate :from-to :from utils/year-month)
		 ]) (get-all))))

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
			(map (fn [m]
				[:tr
					[:td.rafield.rpad.brdr (hf/label :xx (some-> m :_id utils/scrub-id))]
					[:td.dcol.brdr         (hf/label :xx (some-> m :from-to :from utils/year-month))]
					[:td.txtcol.brdr       (hf/label :xx (some-> m :name))]
					[:td.brdr.ccol         (hf/label :xx (some-> m :contacts (common/nth-contact 0) :value))]
					[:td.rafield.rpad.brdr (hf/label :xx (some-> m :estate :_id utils/scrub-id))]
					[:td.txtcol.brdr       (hf/label :xx (some-> m :estate :location))]
					[:td.txtcol.brdr       (hf/label :xx (some-> m :estate :address))]
					[:td.rafield.rpad.brdr (hf/label :xx (some-> m :estate :interval name))]
					[:td.dcol.brdr         (hf/label :xx (some-> m :estate :from-to :from utils/year-month))]
				])
			(get-all))
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
	(vec
		(map (fn [m]
			[(:_id m)
			 (:name m)
		     (-> m :from-to :from utils/year-month)
		     (some-> m :contacts (common/nth-contact 0) :value)
		     (some-> m :contacts (common/nth-contact 1) :value)
		     (some-> m :contacts (common/nth-contact 2) :value)
		     (some-> m :contacts (common/nth-contact 3) :value)
		     (:note m)])
		(db/get-current-members))))

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
