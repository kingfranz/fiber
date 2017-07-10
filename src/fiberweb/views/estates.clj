(ns fiberweb.views.estates
  	(:require 	(fiberweb 	[db         :as db]
  							[utils      :as utils])
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
            	(clojure.java [jdbc :as j])
            	(clojure 	[string     :as str]
            				[set        :as set]
            				[pprint     :as pp])
            	[clojure.tools.reader.edn :as edn]))

;;-----------------------------------------------------------------------------

(defn update-estate
	[{params :params}]
	(let [base (select-keys params [:_id :location :address :note])
		  bis  (edn/read-string (:billing-intervals params))
		  biy  (utils/param->int params :biyear)
		  bi   (if (some? (:yearly params)) :yearly :quarterly)
		  bisn (map #(if (>= (:year %) biy) (assoc % :bi-months bi) %) bis)
		  ft   (common/extract-ft params (:_id params))]
		(db/update-estate (assoc base :from-to ft
					  				  :billing-intervals bisn))))

;;-----------------------------------------------------------------------------

(defn edit-estate
	[estateid]
	(let [estate (db/get-estate estateid)
		  owner  (db/get-owner estate)]
		(layout/common "Ändra en Fastighet" []
		(hf/form-to
			[:post "/update-estate"]
			(hf/hidden-field :_id estateid)
			(hf/hidden-field :billing-intervals (pr-str (:billing-intervals estate)))
			[:table
				[:tr
					[:td [:a.link-head {:href "/"} "Home"]]
					[:td (hf/submit-button {:class "button1 button"} "Uppdatera")]]]
			[:table
				[:tr [:td {:height 30}]]
				[:tr [:td
					[:table
						[:tr
							[:td (hf/label :xx "ID")]
							[:td {:width 500} (hf/label :xx estateid)]]
						[:tr
							[:td (hf/label :xx "Betäckning:")]
							[:td (hf/text-field :location (:location estate))]]
						[:tr
							[:td (hf/label :xx "Adress:")]
							[:td (hf/text-field :address (:address estate))]]
						[:tr
							[:td (hf/label :xx "Ägare:")]
							[:td (hf/label :xx (:name owner))]]
						[:tr
							[:td (hf/label :xx "Notering:")]
							[:td (hf/text-field :note (:note estate))]]]]]
				[:tr [:td {:height 50}]]
				[:tr [:td
					(common/mk-ft "Medlemskapet" estateid estate true)]]
				[:tr [:td {:height 50}]]
				[:tr [:td
					(let [current (->> (:billing-intervals estate)
									   (filter #(= (:year %) (utils/current-year)))
									   first
									   :bi-months)
						  bi-s (fn [bi] (str (:year bi) " " (name (:bi-months bi))))]
						[:table
							[:tr
								[:th (hf/label :xx "Betalningsinterval")]]
							(map (fn [bi] [:tr [:td (hf/label :xx (bi-s bi))]])
								(->> (:billing-intervals estate)
									 (filter #(< (:year %) (utils/current-year)))
									 (sort-by :year)))
							[:tr
								[:td (hf/drop-down :biyear (range (utils/current-year) 2030) (utils/current-year))]
								[:td (hf/label :xx " och framåt, Helår")]
								[:td {:width 50} (hf/check-box {:class "cb"} :yearly (= current :yearly))]]])]]
				[:tr [:td {:height 40}]]
				[:tr [:td
					(common/mk-dc-section estate)]]]))))

;;-----------------------------------------------------------------------------

(defn new-estate
	[]
	(layout/common "Ny Fastighet" []
		(hf/form-to
			[:post "/new-estate"]
			[:table
				[:tr
					[:td [:a.link-head {:href "/"} "Home"]]
					[:td (hf/submit-button {:class "button1 button"} "Skapa fastighet")]]
				[:tr [:td {:height 40}]]]
			[:table
			[:tr [:td
				[:table
					[:tr
						[:td.rafield.rpad (hf/label :xx "ID")]
						[:td.dcol (hf/text-field {:class "dcol"} :estateid "")]]
					[:tr
						[:td.rafield.rpad (hf/label :xx "Betäckning")]
						[:td.txtcol (hf/text-field :location "")]]
					[:tr
						[:td.rafield.rpad (hf/label :xx "Adress")]
						[:td.txtcol (hf/text-field :address "")]]
					[:tr
						[:td.rafield.rpad (hf/label :xx "Notering")]
						[:td.txtcol (hf/text-field :note "")]]
					[:tr [:td {:height 40}]]]]]
			[:tr [:td
				[:table
					[:tr
						[:th {:colspan 2} (hf/label :xx "Medlemskapet började")]]
					[:tr
						[:td (hf/drop-down :fromyear (range 2017 2031) (utils/current-year))]
						[:td (hf/drop-down :frommonth (range 1 13) (utils/current-month))]]
					[:tr [:td {:height 40}]]]]]
			[:tr [:td
				[:table
					[:tr
						[:th {:colspan 2} (hf/label :xx "Betalningsinterval")]]
					[:tr
						[:td.spad (hf/label :xx (str (utils/current-year) " och framåt, Helår:"))]
						[:td (hf/check-box {:class "cb"} :yearly)]]]]]])))

(defn create-estate
	[{params :params}]
	(db/add-estate {
		:_id               (str "estate-" (:estateid params))
		:location          (:location params)
		:address           (:address params)
		:from-to           {:from (t/date-time (utils/param->int params :fromyear)
											   (utils/param->int params :frommonth))
							:to nil}
		:activities        nil
		:billing-intervals (for [year (range (utils/current-year) (inc config/max-year))]
								{:year year :bi-months (if (some? (:yearly params)) :yearly :quarterly)})
		:dcs               []
		:owners            []
		:note              (:note params)}))

;;-----------------------------------------------------------------------------

(defn choose-estate
	[]
	(layout/common "Välj fastighet" []
    	[:table
			[:tr [:td [:a.link-head {:href "/"} "Home"]]]
			[:tr [:td {:height 40}]]]
		[:table
			[:tr
				[:th (hf/label :xx "ID")]
				[:th (hf/label :xx "Namn")]]
			(map (fn [x]
				[:tr
					[:td.rafield.rpad (hf/label :xx (:_id x))]
					[:td [:a.link-thin {:href (str "/edit/" (:_id x))} (hf/label :xx (:address x))]]])
				(db/get-estates))]))

;;-----------------------------------------------------------------------------

(defn enter-activities
	[year]
	(layout/common "Bokför aktiviteter" []
		(hf/form-to
			[:post "/enter-activities"]
			(hf/hidden-field :year year)
        	[:table
				[:tr
					[:td [:a.link-head {:href "/"} "Home"]]]
				[:tr [:td {:height 30}]]
				[:tr
					[:td (hf/label :xx "Aktiviteter för år")]
					[:td (hf/drop-down :newyear (range 2013 config/max-year) year)]]
				[:tr
					[:td (hf/submit-button {:class "button1 button"} "Updatera Året")]]
				[:tr [:td {:height 30}]]])
		(hf/form-to
			[:post "/update-activities"]
			(hf/hidden-field :year year)
			[:table.tbl-co
				[:tr
					[:th (hf/label :xx "MID")]
					[:th (hf/label :xx "Namn")]
					[:th (hf/label :xx "FID")]
					[:th (hf/label :xx "Fastighet")]
					[:th (hf/label :xx "Helår")]
					[:th (hf/label :xx "Alla")]
					[:th (hf/label :xx "Månader")]
					]
				(map (fn [x]
					[:tr.brdr 
						[:td.rafield.rpad (hf/label :xx (:_id x))]
						[:td.ncol         (hf/label :xx (:name x))]
						[:td.rafield.rpad (hf/label :xx (:_id x))]
						[:td.acol         (hf/label :xx (:address x))]
						[:td.rafield      (hf/label :xx (:bimonths x))]
						[:td.dcol         (hf/check-box {:class "cb"}
							(utils/mk-tag (:estateid x) "A") (= (:actmonths x) 4095))]
						[:td (common/mk-acts (:estateid x) (:actmonths x))]])
					(db/get-activities year))]
			[:table
				[:tr
					[:td (hf/submit-button {:class "button1 button"} "Updatera aktiviteter")]]])))

;;-----------------------------------------------------------------------------

(defn update-activities
	[{params :params}]
	(let [year (:year params)
		  acts (db/get-acts year)
		  f-acts (into {} (map (fn [e]
		  	(clojure.lang.MapEntry. (:estateid e)
		  	    (if (get params (utils/mk-tag (:estateid e) "A"))
		  	    	4095
		  	        (common/get-months (:estateid e) params))))
		  		acts))
		  new-acts* (remove (fn [a] (= (:actmonths a) (get f-acts (:estateid a)))) acts)
		  new-acts (map (fn [a] (assoc a :actmonths (get f-acts (:estateid a)))) new-acts*)]
		(doseq [a new-acts]
			(db/update-activity (:estateid a) year (:actmonths a)))
		))

;;-----------------------------------------------------------------------------

