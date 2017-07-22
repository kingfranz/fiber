(ns fiberweb.views.estates
  	(:require 	(fiberweb 		[db         :as db]
  								[utils      :as utils]
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
            	(hiccup 		[core       :as h]
            					[def        :as hd]
            					[element    :as he]
            					[form       :as hf]
            					[page       :as hp]
            					[util       :as hu])
            	(clojure 		[string     :as str]
            					[set        :as set]
            					[pprint     :as pp])
            	[clojure.tools.reader.edn :as edn]
            	[clojure.spec.alpha :as s]))

;;-----------------------------------------------------------------------------

(defn update-estate
	[{params :params}]
	(let [biy (utils/param->int params :biyear)
		  bi  (if (some? (:yearly params)) :yearly :quarterly)
		  ubi (fn [x] (map #(if (>= (:year %) biy) (assoc % :interval bi) %) x))]
		(db/update-estate (-> (db/get-estate (:_id params))
							  (assoc :location (:location params)
									 :address  (:address params)
									 :note     (:note params)
									 :from-to  (common/extract-ft params (:_id params)))
							  (update :billing-intervals ubi)))))

;;-----------------------------------------------------------------------------

(defn edit-estate
	[estateid]
	(let [estate (db/get-estate estateid)]
		(layout/common "Ändra en Fastighet" []
		(hf/form-to
			[:post "/update-estate"]
			(hf/hidden-field :_id estateid)
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
							[:td {:width 500} (hf/label :xx (utils/scrub-id estateid))]]
						[:tr
							[:td (hf/label :xx "Betäckning:")]
							[:td (hf/text-field :location (:location estate))]]
						[:tr
							[:td (hf/label :xx "Adress:")]
							[:td (hf/text-field :address (:address estate))]]
						[:tr
							[:td (hf/label :xx "Ägare:")]
							[:td (hf/label :xx (some-> (utils/find-first #(-> % :from-to :to nil?) (:owners estate))
													   (get :name)))]]
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
									   :interval)
						  bi-s (fn [bi] (str (:year bi) " " (name (:interval bi))))]
						[:table
							[:tr
								[:th (hf/label :xx "Betalningsinterval")]]
							(map (fn [bi] [:tr [:td (hf/label :xx (bi-s bi))]])
								(->> (:billing-intervals estate)
									 (filter #(< (:year %) (utils/current-year)))
									 (sort-by :year)))
							[:tr
								[:td (hf/drop-down :biyear (utils/curr-year-range) (utils/current-year))]
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
						[:td (hf/drop-down :fromyear (utils/curr-year-range) (utils/current-year))]
						[:td (hf/drop-down :frommonth config/month-range (utils/current-month))]]
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
		:billing-intervals (for [year (utils/curr-year-range)]
								{:year year :interval (if (some? (:yearly params)) :yearly :quarterly)})
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
					[:td.rafield.rpad (hf/label :xx (utils/scrub-id (:_id x)))]
					[:td [:a.link-thin {:href (str "/edit/" (:_id x))} (hf/label :xx (:address x))]]])
				(sort-by #(Integer/valueOf (utils/scrub-id (:_id %))) (db/get-estates)))]))

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
					[:td (hf/drop-down :newyear config/year-range year)]]
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
						[:td.rafield.rpad (hf/label :xx (some-> (utils/find-first #(-> % :from-to :to nil?) (:owners x))
													   			(get :_id)
													   			utils/scrub-id))]
						[:td.ncol         (hf/label :xx (some-> (utils/find-first #(-> % :from-to :to nil?) (:owners x))
													   			(get :name)))]
						[:td.rafield.rpad (hf/label :xx (utils/scrub-id (:_id x)))]
						[:td.acol         (hf/label :xx (:address x))]
						[:td.rafield      (hf/label :xx (get (utils/get-year year (:billing-intervals x)) :interval))]
						[:td.dcol         (hf/check-box {:class "cb"}
							(utils/mk-tag (:_id x) "A") (utils/all-months? (get (utils/get-year year (:activities x)) :months)))]
						[:td (common/mk-acts (:_id x) (get (utils/get-year year (:activities x)) :months))]])
					(db/get-estates-at year))]
			[:table
				[:tr
					[:td (hf/submit-button {:class "button1 button"} "Updatera aktiviteter")]]])))

;;-----------------------------------------------------------------------------

(defn update-activities
	[{params :params}]
	(let [year (:year params)
		  estates (db/get-estates-at year)
		  amonths (fn [id] (if (some? (get params (utils/mk-tag id "A")))
		  					   #{1 2 3 4 5 6 7 8 9 10 11 12}
		  					   (common/get-months id params)))
		  merge-acts (fn [months activities] map #(if (= (:year %) year) {:year year :months months} %) activities)
		  acts (map (fn [{id :_id activities :activities}]
		  			{:_id id :activities (merge-acts (amonths id) activities)})
		  		estates)]
		(doseq [a acts]
			(db/update-activity (:_id a) (:activities a)))
		))

;;-----------------------------------------------------------------------------

