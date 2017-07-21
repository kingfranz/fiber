(ns fiberweb.views.members
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
            					[set        :as set])
            	[clojure.spec.alpha :as s]))

;;-----------------------------------------------------------------------------

(defn get-avail-estates
	[]
	{:post [(utils/q-valid? (s/* :fiber/estate) %)]}
	(->> (db/get-estates)
		 (remove #(some? (some (fn [{{from :from to :to} :from-to}] (nil? to)) (:owners %))))))

(defn contact-row
	[idx contacts]
	{:pre [(utils/q-valid? (s/int-in 0 6) idx) (utils/q-valid? :member/contacts contacts)]}
	[:tr
		[:td (hf/drop-down (utils/mk-tag "ctype" idx)
			               (vals common/contact-map)
			               (get common/contact-map (some-> contacts (common/nth-contact idx) :type)))]
		[:td.txtcol (hf/text-field (utils/mk-tag "cvalue" idx) (some-> contacts (common/nth-contact idx) :value))]])

(defn edit-member
	[memberid]
	(let [member (db/get-member memberid)]
		(layout/common "Ändra en medlem" []
		(hf/form-to
			[:post "/update-member"]
			(hf/hidden-field :_id memberid)
			(hf/hidden-field :estates (:estates member))
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
						[:td (hf/label :x (utils/scrub-id memberid))]]
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
					(common/mk-ft "Fastigheter" (:_id x) x (zero? i)))
					(:estates member))]]
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
					(for [i (range 6)] (contact-row i (:contacts member)))]]]
			[:tr [:td {:height 40}]]
			[:tr [:td
				(common/mk-dc-section member)]]]))))

(defn new-member
	[eid]
	(layout/common "Ny medlem" []
		(hf/form-to
			[:post "/new-member"]
			(hf/hidden-field :eids [eid])
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
						[:td.dcol (hf/text-field {:class "dcol"} :_id "")]]
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
						[:td (hf/drop-down (utils/mk-tag "fromyear" "X") (utils/curr-year-range) (utils/current-year))]
						[:td (hf/drop-down (utils/mk-tag "frommonth" "X") config/month-range (utils/current-month))]]]]]
			[:tr [:td {:height 40}]]
			[:tr [:td
				[:table
					[:tr
						[:th (hf/label :xx "Fastighet")]]
					[:tr
						(if (some? eid)
							(list
								(common/mk-ft "" eid {:from-to {:from (l/local-now) :to nil}} false)
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
							(contact-row idx []))
						]]]])))

(defn extract-estates
	[params]
	(map #(update % :from-to (common/extract-ft params (:_id %))) (:estates params)))
	     
(defn extract-contacts
	[params]
	{:post [(utils/q-valid? :member/contacts %)]}
	(->> (range 6)
		 (map (fn [i] {:type  (some->> i (utils/mk-tag "ctype") (get common/icontact-map) keyword)
				       :value (some->> i (utils/mk-tag "cvalue") str/trim)
					   :preferred (zero? i)}))
		(remove #(or (nil? (:type %)) (str/blank? (:value %))))))

(defn create-member
	[{params :params}]
	(db/add-member {
		:_id      (str "member-" (Integer/valueOf (:_id params)))
		:name     (str/trim (:name params))
		:from-to  (common/extract-ft params "X")
		:contacts (extract-contacts params)
		:estates  (extract-estates params)
		:dcs      []
		:note     (str/trim (:note params))}))

(defn update-member
	[{params :params}]
	(let [mid (:_id params)
		  member (db/get-member mid)]
		(db/update-member {:_id      mid
						   :name     (str/trim (:name params))
						   :from-to  (common/extract-ft params "X")
						   :contacts (extract-contacts params)
						   :estates  (extract-estates params)
						   :note     (str/trim (:note params))})))

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
					[:td.rafield.rpad (hf/label :xx (utils/scrub-id (:_id x)))]
					[:td
						[:a.link-thin {:href (if (some? memberid)
												 (str "/add-estate/" memberid "/" (:_id x))
												 (str "/new-member/" (:_id x)))}
									  (hf/label :xx (:address x))]]])
				(sort-by #(Integer/valueOf (utils/scrub-id (:_id %))) (get-avail-estates)))]))

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
					[:td.rafield.rpad (hf/label :xx (utils/scrub-id (:_id x)))]
					[:td [:a.link-thin {:href (str "/edit/" (:_id x))} (hf/label :xx (str (:name x)))]]])
				(sort-by #(Integer/valueOf (utils/scrub-id (:_id %))) (db/get-members)))]))



