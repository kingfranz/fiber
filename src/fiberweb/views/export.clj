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
            	(taoensso 		[timbre     :as timbre])
            	(clojure.java 	[io         :as io])
            	(clojure 		[string     :as str]
            					[spec       :as s]
            					[set        :as set])))

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
					[:td.rafield.rpad.brdr (hf/label :xx (:_id x))]
					[:td.txtcol.brdr       (hf/label {:class "txtcol"} :xx (:name x))]
					[:td.dcol.brdr         (hf/label :xx (utils/year-month (:from (:from-to x))))]
					[:td.brdr.ccol         (hf/label :xx (-> x :contacts :preferred :value))]
					[:td.brdr.ccol         (hf/label :xx (-> x :contacts :other first :value))]
					[:td.brdr              (hf/label :xx (:note x))]])
			(db/get-members))]))

(defn get-all
	[]
	(for [member (db/get-current-members)]
		(for [m-estate (:estates member)
			:let [estate (db/get-estate (:_id m-estate))
				  ee (assoc (select-keys estate [:_id :location :address :from-to])
				  			:bi-months (:bi-months (utils/get-year (utils/current-year) (:billing-intervals estate))))]
			:when (-> m-estate :from-to :to nil?)]
			(assoc (dissoc member :estates) :estate ee))))

(defn mk-all-lst
	[]
	(vec (map (fn [m]
		[(-> m :_id)
		 (-> m :from-to :from utils/year-month)
		 (-> m :name)
		 (-> m :contacts :preferred :value)
		 (-> m :estate :_id)
		 (-> m :estate :location)
		 (-> m :estate :address)
		 (-> m :estate :bi-months name)
		 (-> m :estate :from-to :from utils/year-month)
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
					[:td.rafield.rpad.brdr (hf/label :xx (-> m :_id))]
					[:td.dcol.brdr         (hf/label :xx (-> m :from-to :from utils/year-month))]
					[:td.txtcol.brdr       (hf/label :xx (-> m :name))]
					[:td.brdr.ccol         (hf/label :xx (-> m :contacts :preferred :value))]
					[:td.rafield.rpad.brdr (hf/label :xx (-> m :estate :_id))]
					[:td.txtcol.brdr       (hf/label :xx (-> m :estate :location))]
					[:td.txtcol.brdr       (hf/label :xx (-> m :estate :address))]
					[:td.rafield.rpad.brdr (hf/label :xx (-> m :estate :bi-months name))]
					[:td.dcol.brdr         (hf/label :xx (-> m :estate :from-to :from utils/year-month))]
				]) (get-all))
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

(defn nth*
	[coll n]
	(if (< n (count coll))
		(get (nth coll n) :value)
		""))

(defn mk-member-lst
	[]
	(vec
		(map (fn [m]
			[(:_id m)
			 (:name m)
		     (-> m :from-to :from utils/year-month)
		     (-> m :contacts :preferred :value)
		     (-> m :contacts :other (nth* 0))
		     (-> m :contacts :other (nth* 1))
		     (-> m :contacts :other (nth* 2))
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
