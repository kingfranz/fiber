(ns fiberweb.views.members
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
            				[set        :as set])))

;;-----------------------------------------------------------------------------

(defn mk-tag
	[id idx]
	(keyword (str "tag-" id "-" idx)))

(defn mk-ft
	[fytag fyval
	 fmtag fmval
	 tytag tyval
	 tmtag tmval
	 endtag]
	(list
		[:td (hf/drop-down fytag (range 2013 2031) (common/get-year fyval))]
		[:td (hf/drop-down fmtag (range 1 13) (common/get-month fmval))]
		[:td (hf/check-box {:class "cb"} endtag (some? tyval))]
		[:td (hf/drop-down tytag (range 2013 2031) (common/get-year tyval))]
		[:td (hf/drop-down tmtag (range 1 13) (common/get-month tmval))]))

(defn edit-member
	[memberid]
	(let [member (db/get-member memberid)]
		(layout/common "Ändra en medlem" [css-member]
		(hf/form-to
			[:post "/edit-member"]
			(hf/hidden-field :memberid memberid)
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
				[:table
					[:tr
						[:th {:colspan 5} (hf/label :xx "Medlemskapet")]]
					[:tr
						[:td {:colspan 2} (hf/label :xx "Började")]
						[:td (hf/label :xx "Avsluta?")]
						[:td {:colspan 2} (hf/label :xx "Slutade")]]
					[:tr
						(mk-ft :fromyear (:fromyear member) :frommonth (:fromyear member)
						       :toyear (:toyear member) :tomonth (:tomonth member)
						       :endmember)]]]]
			[:tr [:td {:height 40}]]
			[:tr [:td
				[:table
					[:tr
						[:th {:colspan 6} (hf/label :xx "Fastigheter")]]
					[:tr
						[:th (hf/label :xx "ID")]
						[:th {:colspan 2} (hf/label :xx "Började")]
						[:th (hf/label :xx "Avsluta?")]
						[:th {:colspan 2} (hf/label :xx "Slutade")]]
					(let [estates (db/get-estates memberid)]
						(list
							(map (fn [x]
								[:tr
									[:td.dcol.rafield.rpad (hf/label :xx (:estateid x))]
									(mk-ft (mk-tag "fromyear" (:estateid x)) (:me_fromyear x)
										   (mk-tag "frommonth" (:estateid x)) (:me_fromyear x)
						       			   (mk-tag "toyear" (:estateid x)) (:me_toyear x)
						       			   (mk-tag "tomonth" (:estateid x)) (:me_toyear x)
						       			   (mk-tag "end" (:estateid x)))])
								estates)
								[:tr
									[:td.dcol.rafield.rpad (hf/text-field (mk-tag "estateid" "X") "")]
									(mk-ft (mk-tag "fromyear" "X") nil
										   (mk-tag "frommonth" "X") nil
						       			   (mk-tag "toyear" "X") nil
						       			   (mk-tag "tomonth" "X") nil
						       			   (mk-tag "end" "X"))]))]]]
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
				(common/mk-dc-section (db/get-memberdcs memberid) true)]]]))))

(defn new-member
	[]
	(layout/common "Ny medlem" [css-member]
		(hf/form-to
			[:post "/new-member"]
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
						[:td (hf/drop-down :fromyear (range 2017 2031) (common/current-year))]
						[:td (hf/drop-down :frommonth (range 1 13) (common/current-month))]]]]]
			[:tr [:td {:height 40}]]
			[:tr [:td
				[:table
					[:tr
						[:td (hf/label :xx "Fastighets ID")]
						[:td.dcol (hf/text-field :estateid "")]]]]]
			[:tr [:td {:height 40}]]
			[:tr [:td
					[:table
						[:tr
							[:th {:colspan 2} (hf/label :xx "Kontaktinformation")]]
						[:tr
							[:td (hf/label :xx "Typ")]
							[:td (hf/label :xx "Text")]]
						[:tr
							[:td (hf/drop-down :tag-ct-0 (vals common/contact-map) (val (first common/contact-map)))]
							[:td.txtcol (hf/text-field :tag-cv-0 "")]]
						[:tr
							[:td (hf/drop-down :tag-ct-1 (vals common/contact-map) (val (first common/contact-map)))]
							[:td.txtcol (hf/text-field :tag-cv-1 "")]]
						[:tr
							[:td (hf/drop-down :tag-ct-2 (vals common/contact-map) (val (first common/contact-map)))]
							[:td.txtcol (hf/text-field :tag-cv-2 "")]]
						[:tr
							[:td (hf/drop-down :tag-ct-3 (vals common/contact-map) (val (first common/contact-map)))]
							[:td.txtcol (hf/text-field :tag-cv-3 "")]]
						[:tr
							[:td (hf/drop-down :tag-ct-4 (vals common/contact-map) (val (first common/contact-map)))]
							[:td.txtcol (hf/text-field :tag-cv-4 "")]]
						[:tr
							[:td (hf/drop-down :ct-5 (vals common/contact-map) (val (first common/contact-map)))]
							[:td.txtcol (hf/text-field :cv-5 "")]]]]]])))

(defn b->n
	[x]
	(when (seq x) x))

(defn extract-dcs
	[params]
	(let [mid  (:memberid params)
		  fdcs (map #(assoc (common/extract-dc params true (:memberdcid %))
		  	        :dc % :memberid mid :memberdcid (:memberdcid %)) (db/get-memberdcs mid))
		  ndc  (common/extract-dc params true "X")]
		(doseq [dc fdcs
				:when (:del dc)]
			(db/delete-memberdc mid (:memberdcid dc)))
		(doseq [dc fdcs
				:when (not (common/same-submap? dc (:dc dc)))]
			(db/update-memberdc mid (dissoc dc :del :dc)))
		(when (seq (:date ndc))
			(db/add-member-payment mid (:amount ndc) (:year ndc)))))

(defn get-me
	[params eid id]
	{:memberid (Integer/valueOf (:memberid params))
	:estateid eid
	:fromyear (common/ymd->f (common/param->double params "fromyear" id)
		                     (common/param->double params "frommonth" id))
	:toyear (when (some? (get params (mk-tag "end" id)))
	        (common/ymd->f (common/param->double params "toyear" id)
		                   (common/param->double params "tomonth" id)))})

(defn extract-estates
	[params]
	(doseq [estate (db/get-estates (:memberid params))
			:let [id (:estateid estate)
			      me (get-me params id id)]
		    :when (not= me
		    	        (select-keys estate [:memberid :estateid :fromyear :toyear]))]
	    (db/update-member-estate me))
	(if (seq (get params (mk-tag "estateid" "X")))
		(let [me (get-me params (common/param->int params "estateid" "X") "X")]
	        (db/insert-member-estate me))))
	     
(defn extract-contacts
	[params]
	(let [contacts (map (fn [i] {
			:type (get common/icontact-map (get params (mk-tag "ctype" i)))
		    :value (get params (mk-tag "cvalue" i))
		    :preferred (zero? i)
		    :memberid (some->> (:memberid params) Integer/valueOf)
		    :contactid (some->> (get params (mk-tag "cid" i)) b->n Integer/valueOf)
		    :htype (get params (mk-tag "chtype" i))
		    :hvalue (get params (mk-tag "chvalue" i))
		    })
		(range 6))]
		(if-not (some :preferred contacts)
			(throw (Exception. "no preferred contact"))
			(do
				(doseq [contact contacts
					    :when (and (some? (:contactid contact))
					    	       (str/blank? (:value contact)))]
					(db/delete-contact contact))
				(doseq [contact contacts
					    :when (and (some? (:contactid contact))
					    	       (or (not= (:type contact) (:htype contact))
					    	       	   (not= (:value contact) (:hvalue contact))))]
					(db/update-contact (dissoc contact :htype :hvalue)))
				(doseq [contact contacts
					    :when (and (nil? (:contactid contact))
					    	       (seq (:value contact)))]
					(db/add-contact (dissoc contact :htype :hvalue)))
				))))

(defn create-member
	[{params :params}]
	(db/add-member (:memberid params) (:name params) (:note params)
		           (common/ym->f (:fromyear params) (:frommonth params))
		           (:estateid params)
		           (extract-contacts params (:memberid params))))

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
			               :fromyear (common/ym->f (:fromyear params) (:frommonth params))
			               :toyear (when (some? (:endmember params))
			               		(common/ym->f (:toyear params) (:tomonth params)))})
		(extract-estates params)
		(extract-contacts params)
		(extract-dcs params)))



