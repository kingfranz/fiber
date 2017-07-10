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
            	(clojure.data 	[json       :as json])
            	(clojure 		[string     :as str]
            					[spec       :as s]
            					[set        :as set])))

;;-----------------------------------------------------------------------------



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
	(utils/bits->months m (utils/q->b 0)))

(defn date-frmtr
	[k v]
	(if (or (= k :date) (= k :from) (and (= k :to) (some? v)))
		(f/unparse (f/with-zone (f/formatters :date-time) (t/default-time-zone)) v)
		v))

(defn trans-dc
	[dc]
	(let [dc* {
		 :date    (c/from-sql-time (:date dc))
		 :amount  (bigdec (:amount dc))
		 :tax     (bigdec (:tax dc))
		 :dc-type (keyword (str/replace (:type dc) "-" ""))
		 :year    (:year dc)}]
		(if (some? (:months dc))
			(assoc dc* :months (months->arr (:months dc)))
			dc*)))

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
		  mes        (db/get-membersestates)]
		(spit "estates.json" 
			(json/write-str 
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
						 						 				 :bi-months (if (= (:bimonths bi) 12) :yearly :quarterly)})))
						 :dcs               (->> estatedcs
						 						 (filter #(= (:estateid %) (:estateid estate)))
						 						 (mapv trans-dc))
						 :owners            (->> mes
						 						 (filter #(= (:estateid %) (:estateid estate)))
						 						 (mapv (fn [me] {:_id (str "member-" (:memberid me))
						 						 				 :from-to (trans-ft (:fromyear me) (:toyear me))})))
						 :note              (:note estate)}))
				:value-fn date-frmtr))))

(defn trans-contact
	[c]
	{:type (keyword (:type c)) :value (:value c)})

(defn export-mongo-member
	[]
	(let [members   (db/get-members)
		  mes       (db/get-membersestates)
		  contacts  (db/get-contacts-all)
		  memberdcs (db/get-memberdcs-all)]
		(spit "members.json" 
			(json/write-str 
				(vec 
					(for [member members
						:let [conts (->> contacts (filter #(= (:memberid %) (:memberid member))))]]
			   			{:_id      (str "member-" (:memberid member))
						 :name     (:name member)
						 :from-to  (trans-ft (:fromyear member) (:toyear member))
						 :contacts {:preferred (->> conts
												   (filter #(= (:preferred %) 1))
												   first
												   trans-contact)
								   :other (->> conts
								   			   (remove #(= (:preferred %) 1))
								   			   (mapv trans-contact))}
						 :estates  (->> mes
									   (filter #(= (:memberid %) (:memberid member)))
									   (mapv (fn [me] {:_id (str "estate-" (:estateid me))
									   			      :from-to (trans-ft (:fromyear me) (:toyear me))})))
						 :dcs      (->> memberdcs
					 				   (filter #(= (:memberid %) (:memberid member)))
					 				   (mapv trans-dc))
						 :note     (:note member)}))
				:value-fn date-frmtr))))

(defn export-mongo-config
	[]
	(spit "configs.json" 
		(json/write-str 
			(vec 
				(for [config (db/get-configs)
					:let [nc (select-keys config [:membershipfee :membershiptax
		  										   :connectionfee :connectiontax
		  										   :operatorfee   :operatortax])]]
					(assoc nc :_id (str (:entered config))
			 			   	  :from (t/date-time (utils/get-year (:fromyear config)) 
			 			   		                 (utils/get-month (:fromyear config)) 1))))
			:value-fn date-frmtr)))

(defn export-mongo
	[]
	(export-mongo-config)
	(export-mongo-member)
	(export-mongo-estate)
	)

;;-----------------------------------------------------------------------------
