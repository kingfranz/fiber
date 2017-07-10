(ns fiberweb.views.members
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
            				[spec       :as s]
            				[set        :as set])))

;;-----------------------------------------------------------------------------

(defn mk-tag
	[id idx]
	(keyword (str "tag-" id "-" idx)))

(defn edit-member
	[memberid]
	(let [member (db/get-member memberid)
		  dcs    (db/get-memberdcs memberid)]
		(layout/common "Ändra en medlem" []
		(hf/form-to
			[:post "/update-member"]
			(hf/hidden-field :memberid memberid)
			(hf/hidden-field :member (pr-str member))
			[:table
				[:tr
					[:td [:a.link-head {:href "/"} "Home"]]
					[:td (hf/submit-button {:class "button1 button"} "Uppdatera medlem")]
					[:tr [:td {:height 40}]]]]
			[:table.brdrcol
			[:tr [:td
				[:table
					[:tr
						[:td.rafield.rpad (hf/label :xx "Medlemsnummer")]
						[:td (hf/label :memberid (:memberid member))]]
					[:tr
						[:td.rafield.rpad (hf/label :xx "Namn")]
						[:td.txtcol (hf/text-field :name (:name member))]]
					[:tr
						[:td.rafield.rpad (hf/label :xx "Notering")]
						[:td.txtcol (hf/text-field :note (:note member))]]]]]
			[:tr [:td {:height 40}]]
			[:tr [:td
				(common/mk-ft "Medlemskapet" memberid member true)]]
			[:tr [:td {:height 40}]]
			[:tr [:td
				(map-indexed (fn [i x]
					(common/mk-ft "Fastigheter"
								  (:estateid x) 
								  (set/rename-keys x {:me_fromyear :fromyear :me_toyear :toyear})
								  (zero? i))) 
					(db/get-estates memberid))]]
			[:tr [:td {:colspan 4}
				[:a.link-head {:href (str "/add-estate/" memberid)} "Lägg till"]]]
			[:tr [:td {:height 40}]]
			[:tr [:td
				[:table
					[:tr
						[:th {:colspan 2} (hf/label :xx "Kontaktinformation")]]
					[:tr
						[:td (hf/label :xx "Typ")]
						[:td (hf/label :xx "Text")]]
					(let [contacts (db/get-contacts memberid)]
						(list
							(map-indexed (fn [i x]
								[:tr
									[:td (hf/drop-down (mk-tag "ctype" i)
										               (vals common/contact-map)
										               (get common/contact-map (:type x)))]
									[:td.txtcol (hf/text-field (mk-tag "cvalue" i) (:value x))
									    (hf/hidden-field (mk-tag "cid" i) (:contactid x))
									    (hf/hidden-field (mk-tag "chtype" i) (:type x))
									    (hf/hidden-field (mk-tag "chvalue" i) (:value x))]])
								contacts)
							(for [i (range (count contacts) 7)]
								[:tr
									[:td (hf/drop-down (mk-tag "ctype" i)
										               (vals common/contact-map)
										               (val (first common/contact-map)))]
									[:td.txtcol (hf/text-field (mk-tag "cvalue" i) nil)
									    (hf/hidden-field (mk-tag "cid" i) nil)
									    (hf/hidden-field (mk-tag "chtype" i) nil)
									    (hf/hidden-field (mk-tag "chvalue" i) nil)]])))]]]
			[:tr [:td {:height 40}]]
			[:tr [:td
				(common/mk-dc-section memberid dcs true)]]]))))

(defn new-member
	[eid]
	(layout/common "Ny medlem" []
		(hf/form-to
			[:post "/new-member"]
			(hf/hidden-field :estateid eid)
			[:table
				[:tr
					[:td [:a.link-head {:href "/"} "Home"]]
					[:td (hf/submit-button {:class "button1 button"} "Skapa medlem")]]
				[:tr [:td {:height 40}]]]
			[:table
			[:tr [:td
				[:table
					[:tr
						[:td.rafield (hf/label :xx "Medlemsnummer")]
						[:td.dcol (hf/text-field {:class "dcol"} :memberid "")]]
					[:tr
						[:td.rafield (hf/label :xx "Namn")]
						[:td.txtcol (hf/text-field :name "")]]
					[:tr
						[:td.rafield (hf/label :xx "Notering")]
						[:td.txtcol (hf/text-field :note "")]]]]]
			[:tr [:td {:height 40}]]
			[:tr [:td
				[:table
					[:tr
						[:th {:colspan 2} (hf/label :xx "Medlemskapet börjar")]]
					[:tr
						[:td (hf/drop-down :fromyear (range 2017 2031) (utils/current-year))]
						[:td (hf/drop-down :frommonth (range 1 13) (utils/current-month))]]]]]
			[:tr [:td {:height 40}]]
			[:tr [:td
				[:table
					[:tr
						[:th (hf/label :xx "Fastighet")]]
					[:tr
						(if (some? eid)
							(list
								[:td.dcol (hf/label :x (:address (db/get-estate eid)))]
								[:td [:a.link-thin {:href "/new-member"} "X"]])
							[:td [:a.link-head {:href "/add-estate/new/"} "Lägg till"]])
						]]]]
			[:tr [:td {:height 40}]]
			[:tr [:td
					[:table
						[:tr
							[:th {:colspan 2} (hf/label :xx "Kontaktinformation")]]
						[:tr
							[:td (hf/label :xx "Typ")]
							[:td (hf/label :xx "Text")]]
						(for [idx (range 6)]
							[:tr
								[:td (hf/drop-down (utils/mk-tag "tag-ct-" idx)
												   (vals common/contact-map)
												   (val (first common/contact-map)))]
								[:td.txtcol (hf/text-field (utils/mk-tag "tag-cv-" idx) "")]])
						]]]])))

(defn b->n
	[x]
	(when (seq x) x))

(defn get-me
	[params eid mid]
	{:memberid (Integer/valueOf (:memberid params))
	 :estateid eid
	 :fromyear (utils/ymd->f (utils/param->double params (utils/mk-tag "fromyear" mid))
		                     (utils/param->double params (utils/mk-tag "frommonth" mid)))
	 :toyear (when (some? (get params (utils/mk-tag "endtag" mid)))
	        	(utils/ymd->f (utils/param->double params (utils/mk-tag "toyear" mid))
		                      (utils/param->double params (utils/mk-tag "tomonth" mid))))})

(defn extract-estates
	[params]
	(doseq [estate (db/get-estates (:memberid params))
			:let [id (:estateid estate)
			      me (get-me params id id)]
		    :when (not= me (select-keys estate [:memberid :estateid :fromyear :toyear]))]
	    (db/update-member-estate me))
	(if (seq (get params (mk-tag "estateid" "X")))
		(let [me (get-me params (utils/param->int params "estateid" "X") "X")]
	        (db/insert-member-estate me))))
	     
(defn extract-contacts
	[params]
	(let [memberid (some->> (:memberid params) Integer/valueOf)
		  contacts* (map (fn [i] {
			:type      (some->> (mk-tag "ctype" i) (get params) (get common/icontact-map) keyword)
		    :value     (some->> (mk-tag "cvalue" i) (get params))
		    :preferred (zero? i)
		    :memberid  memberid
		    :contactid (some->> (get params (mk-tag "cid" i)) b->n Integer/valueOf)
		    :htype     (->> (mk-tag "chtype" i) (get params) keyword)
		    :hvalue    (->> (mk-tag "chvalue" i) (get params))
		    })
			(range 6))
		  contacts (filter #(s/valid? :fiber/contact %) contacts*)]
		(if-not (true? (:preferred (first contacts)))
			(throw (Exception. "no preferred contact"))
			(doseq [contact* contacts
			  :let [contact (update contact* :type name)]]
				(cond
					; blank value means delete
					(and (some? (:contactid contact)) (str/blank? (:value contact)))
						(db/delete-contact contact)
					; type or value changed means update
					(and (some? (:contactid contact))
					     (or (not= (:type contact) (:htype contact))
					    	 (not= (:value contact) (:hvalue contact))))
						(db/update-contact (dissoc contact :htype :hvalue))
					; no id and not blank value means new
					(and (nil? (:contactid contact)) (seq (:value contact)))
						(db/add-contact (dissoc contact :htype :hvalue))
				)))))

(defn create-member
	[{params :params}]
	(db/add-member
		(assoc (select-keys params [:memberid :name :note])
		       :fromyear (utils/ym->f (:fromyear params) (:frommonth params))
		       :toyear   nil)
		(:estateid params)
		(extract-contacts params (:memberid params))))

(defn choose-estate
	[memberid]
	(layout/common "Välj fastighet" []
		[:table
			[:tr [:td [:a.link-head {:href (if (some? memberid)
											   (str "/edit-member/" memberid)
											   "/new-member")} "Tillbaka"]]]
			[:tr [:td {:height 40}]]]
		[:table
			[:tr
				[:th (hf/label :xx "ID")]
				[:th (hf/label :xx "Namn")]]
			(map (fn [x]
				[:tr
					[:td.rafield.rpad (hf/label :xx (:estateid x))]
					[:td
						[:a.link-thin {:href (if (some? memberid)
												 (str "/add-estate/" memberid "/" (:estateid x))
												 (str "/new-member/" (:estateid x)))}
									  (hf/label :xx (:address x))]]])
				(db/get-avail-estates))]))

(defn choose-member
	[]
	(layout/common "Välj medlem" []
		[:table
			[:tr [:td [:a.link-head {:href "/"} "Home"]]]
			[:tr [:td {:height 40}]]]
		[:table
			[:tr
				[:th (hf/label :xx "ID")]
				[:th (hf/label :xx "Namn")]]
			(map (fn [x]
				[:tr
					[:td.rafield.rpad (hf/label :xx (:memberid x))]
					[:td [:a.link-thin {:href (str "/edit-member/" (:memberid x))} (hf/label :xx (str (:name x)))]]])
				(db/get-members))]))

(defn update-member
	[{params :params}]
	(j/with-db-transaction [db-conn db/fiberdb]
		(db/update-member {:memberid (:memberid params)
						   :name (:name params)
						   :note (:note params)
			               :fromyear (utils/ym->f (get params (utils/mk-tag "fromyear" (:memberid params)))
			               						  (get params (utils/mk-tag "frommonth" (:memberid params))))
			               :toyear (when (some? (get params (utils/mk-tag "endtag" (:memberid params))))
			               		(utils/ym->f (get params (utils/mk-tag "toyear" (:memberid params)))
			               		             (get params (utils/mk-tag "tomonth" (:memberid params)))))})
		(extract-estates params)
		(extract-contacts params)))



