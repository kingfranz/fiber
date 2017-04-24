(ns fiberweb.views.estates
  	(:require 	(fiberweb 	[db         :as db])
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
            				[pprint     :as pp])))

;;-----------------------------------------------------------------------------


;;-----------------------------------------------------------------------------

(defn extract-dcs
	[params]
	(let [eid  (:estateid params)
		  fdcs (map #(assoc (common/extract-dc params false (:estatedcid %))
		  	        :dc % :estateid eid :estatedcid (:estatedcid %)) (db/get-estatedcs eid))
		  ndc  (common/extract-dc params false "X")]
		(doseq [dc fdcs
				:when (:del dc)]
			(db/delete-estatedc eid (:estatedcid dc)))
		(doseq [dc fdcs
				:when (not (common/same-submap? dc (:dc dc)))]
			(db/update-estatedc eid (dissoc dc :del :dc)))
		(when (seq (:date ndc))
			(db/add-estate-payment eid (:amount ndc) (:year ndc)))))

(defn update-estate
	[{params :params}]
	(j/with-db-transaction [db-conn db/fiberdb]
		(db/update-estate {:estateid (:estateid params)
						   :location (:location params)
						   :address (:address params)
						   :note (:note params)
			               :fromyear (common/ym->f (:fromyear params) (:frommonth params))
			               :toyear (when (some? (:endestate params))
			               		(common/ym->f (:toyear params) (:tomonth params)))})
		(db/update-estatebi (:estateid params) (:biyear params) (:yearly params))
		(extract-dcs params)))

;;-----------------------------------------------------------------------------

(defn edit-estate
	[estateid]
	(let [estate (db/get-estate estateid)
		  owner (db/get-owner estateid)]
		(layout/common "Ändra en Fastighet" []
		(hf/form-to
			[:post "/update-estate"]
			(hf/hidden-field :estateid estateid)
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
					[:table
						[:tr
							[:td {:colspan 2} (hf/label :xx "Medlemskapet började")]
							[:td (hf/label :xx "Avsluta?")]
							[:td {:colspan 2} (hf/label :xx "Medlemskapet slutade")]]
						[:tr
							[:td {:width 100} (hf/text-field :fromyear (common/get-year (:fromyear estate)))]
							[:td {:width 100} (hf/text-field :frommonth (common/get-month (:fromyear estate)))]
							[:td (hf/check-box {:class "cb"} :endestate (some? (:toyear estate)))]
							[:td {:width 100} (hf/text-field :toyear (common/get-year (:toyear estate)))]
							[:td {:width 100} (hf/text-field :tomonth (common/get-month (:toyear estate)))]]]]]
				[:tr [:td {:height 50}]]
				[:tr [:td
					(let [bis (db/get-estatebi estateid)
						  current (:bimonths (first (filter #(= (:year %) (common/current-year)) bis)))
						  bi-s (fn [bi] (str (:year bi) " " (if (= (:bimonths bi) 12) "Helår" "Kvartal")))]
						[:table
							[:tr
								[:th (hf/label :xx "Betalningsinterval")]]
							(map (fn [bi]
								[:tr [:td (hf/label :xx (bi-s bi))]])
								(->> bis (filter #(< (:year %) (common/current-year))) (sort-by :year)))
							[:tr
								[:td (hf/drop-down :biyear (range 2013 2030) (common/current-year))]
								[:td (hf/label :xx " och framåt, Helår")]
								[:td {:width 50} (hf/check-box {:class "cb"} :yearly (= current 12))]]])]]
				[:tr [:td {:height 40}]]
				[:tr [:td
					(common/mk-dc-section (db/get-estatedcs estateid) false)]]]))))

;;-----------------------------------------------------------------------------

(defn new-estate
	[]
	(layout/common "Ändra en Fastighet" []
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
						[:td (hf/drop-down :fromyear (range 2017 2031) (common/current-year))]
						[:td (hf/drop-down :frommonth (range 1 13) (common/current-month))]]
					[:tr [:td {:height 40}]]]]]
			[:tr [:td
				[:table
					[:tr
						[:th {:colspan 2} (hf/label :xx "Betalningsinterval")]]
					[:tr
						[:td.spad (hf/label :xx (str (common/current-year) " och framåt, Helår:"))]
						[:td (hf/check-box {:class "cb"} :yearly)]]]]]])))

(defn create-estate
	[{params :params}]
	(db/add-estate (:estateid params) (:location params) (:address params)
		           (:note params) (common/ym->f (:fromyear params) (:frommonth params))
		           (if (some? (:yearly params)) 12 3)))

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
					[:td.rafield.rpad (hf/label :xx (:estateid x))]
					[:td [:a.link-thin {:href (str "/edit-estate/" (:estateid x))} (hf/label :xx (:address x))]]])
				(db/get-estates))]))

;;-----------------------------------------------------------------------------

(defn mk-tag
	[id idx]
	(keyword (str "tag-" id "-" idx)))

(defn mk-acts
	[eid bits]
	;(println (type bits) (int bits))
	[:table
		[:tr
			(map (fn [i]
				[:td {:width 30} (hf/check-box {:class "cb"} (mk-tag eid (inc i)) (bit-test bits i))])
				(range 12))]])

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
					[:td (hf/drop-down :newyear (range 2010 2020) year)]]
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
						[:td.rafield.rpad (hf/label :xx (:memberid x))]
						[:td.ncol (hf/label :xx (:name x))]
						[:td.rafield.rpad (hf/label :xx (:estateid x))]
						[:td.acol (hf/label :xx (:address x))]
						[:td.rafield (hf/label :xx (:bimonths x))]
						[:td.dcol (hf/check-box {:class "cb"} (mk-tag (:estateid x) "A") (= (:actmonths x) 4095))]
						[:td (mk-acts (:estateid x) (:actmonths x))]])
					(db/get-activities year))]
			[:table
				[:tr
					[:td (hf/submit-button {:class "button1 button"} "Updatera aktiviteter")]]])))

;;-----------------------------------------------------------------------------

(defn get-months
	[id request]
	(reduce + (map (fn [i] (if (get request (mk-tag id i))
					(nth [0 1 2 4 8 16 32 64 128 256 512 1024 2048 4096] i)
					0)) (range 1 13))))

(defn update-activities
	[{request :params}]
	;(pp/pprint request)
	(let [year (:year request)
		  acts (db/get-acts year)
		  f-acts (into {} (map (fn [e]
		  	(clojure.lang.MapEntry. (:estateid e)
		  	    (if (get request (mk-tag (:estateid e) "A"))
		  	    	4095
		  	        (get-months (:estateid e) request))))
		  		acts))
		  new-acts* (remove (fn [a] (= (:actmonths a) (get f-acts (:estateid a)))) acts)
		  new-acts (map (fn [a] (assoc a :actmonths (get f-acts (:estateid a)))) new-acts*)]
		(doseq [a new-acts]
			(db/update-activity (:estateid a) year (:actmonths a)))
		))

;;-----------------------------------------------------------------------------

