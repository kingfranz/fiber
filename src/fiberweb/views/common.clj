(ns fiberweb.views.common
  	(:require 	(fiberweb.views [layout     :as layout])
 	          	(garden 	[core       :as g]
            				[units      :as u]
            				[selectors  :as sel]
            				[stylesheet :as ss]
            				[color      :as color])
             	(clj-time 	[core       :as t]
            				[local      :as l]
            				[coerce     :as c]
            				[format     :as f]
            				[periodic   :as p])
            	(hiccup 	[core       :as h]
            				[def        :as hd]
            				[element    :as he]
            				[form       :as hf]
            				[page       :as hp]
            				[util       :as hu])
            	(clojure 	[string     :as str]
            				[set        :as set]
            				[spec       :as s])))

;;-----------------------------------------------------------------------------

(def memberdc-map (hash-map "membership-fee" "Medlemsavgift"
	                        "payment"        "Inbetalning"))

(def imemberdc-map (set/map-invert memberdc-map))

(def estatedc-map (hash-map "connection-fee" "Anslutningsavgift"
	                        "operator-fee"   "Användningsavgift"
	                        "payment"        "Inbetalning"
	                        "entry-fee"      "Insats"))

(def iestatedc-map (set/map-invert estatedc-map))

(def contact-map (hash-map "address" "Adress"
						   "email"   "E-Post"
						   "phone"   "Telefon"))

(def icontact-map (set/map-invert contact-map))

;;-----------------------------------------------------------------------------

(defn spy
	([x]
	(prn "spy:" (type x) x)
	x)
	([t x]
	(prn "spy:" t x)
	x))

(defn ym->f
	[y m]
	(when (some? y)
		(+ (Double/valueOf y) (/ (Double/valueOf m) 13.0))))

(defn ymd->f
	[y m]
	(when (some? y)
		(+ y (/ m 13.0))))

(defn current-year
	[]
	(t/year (l/local-now)))

(defn current-month
	[]
	(t/month (l/local-now)))

(defn current-quarter
	[]
	(nth [0 1 1 1 2 2 2 3 3 3 4 4 4] (current-month)))

(defn get-year
	[ym]
	(if (nil? ym)
		(current-year)
		(int ym)))

(defn get-month
	[ym]
	(if (nil? ym)
		(current-month)
		(int (* (- ym (int ym)) 13))))

(defn now-str
    []
    (f/unparse (f/with-zone (f/formatters :mysql) (t/default-time-zone)) (l/local-now)))

(defn same-submap?
	[m1 m2]
	(let [inter (set/intersection (keys m1) (keys m2))
		  m1i   (apply dissoc m1 (set/difference (keys m1) inter))
		  m2i   (apply dissoc m2 (set/difference (keys m2) inter))]
		(= m1i m2i)))

(defn b->n
	[x]
	(when (seq x) x))

(defn mk-tag
	[id idx]
	(keyword (str "tag-" id "-" idx)))

(defn param->bigdec
	[params s id]
	(some->> (mk-tag s id) (get params) b->n BigDecimal.))

(defn param->int
	[params s id]
	(some->> (mk-tag s id) (get params) b->n Integer/valueOf))

(defn param->double
	[params s id]
	(some->> (mk-tag s id) (get params) b->n Double/valueOf))

(defn ts->d
	[ts]
	(f/unparse (f/formatters :date) (c/from-sql-time ts)))

(defn dc-row
	[dc id ddmap]
	[:tr
		[:td.ddcol.rpad (hf/text-field {:class "rafield"} (mk-tag "date" id) (some->> (:date dc) ts->d))]
		[:td.rpad (hf/drop-down {:class "rafield"} (mk-tag "type" id)
			               (vals ddmap)
			               (get ddmap (:type dc)))]
		[:td.dcol.rpad (hf/text-field {:class "rafield"} (mk-tag "amount" id) (:amount dc))]
		[:td.dcol.rpad (hf/text-field {:class "rafield"} (mk-tag "tax" id) (:tax dc))]
		[:td.dcol (hf/text-field {:class "rafield"} (mk-tag "year" id) (:year dc))]
		[:td (hf/check-box {:class "cb"} (mk-tag "del" id) false)]])

(defn mk-dc-section
	[dcs member?]
	[:table
		[:tr
			[:th {:colspan 6} (hf/label :xx "Debit - Kredit")]]
		[:tr
			[:td (hf/label :xx "Införd")]
			[:td (hf/label :xx "Typ")]
			[:td (hf/label :xx "Belopp")]
			[:td (hf/label :xx "Moms")]
			[:td (hf/label :xx "År")]
			[:td (hf/label :xx "Ta bort")]
		]
		(map (fn [dc] (dc-row dc
			                  (if member?
			                  	(:memberdcid dc)
			                  	(:estatedcid dc))
			                  (if member?
			                  	memberdc-map
			                  	estatedc-map))) dcs)
		(dc-row {} "X"
              (if member?
              	memberdc-map
              	estatedc-map))])

(defn extract-dc
	[params member? id]
	(let [valid? (if member? :fiber/member-dc-entry :fiber/estate-dc-entry)
		  dc {:date   (get params (mk-tag "date" id))
			 :type   (get (if member? imemberdc-map iestatedc-map) (get params (mk-tag "type" id)))
			 :amount (param->bigdec params "amount" id)
			 :tax    (param->bigdec params "tax" id)
			 :year   (param->int params "year" id)
			 :months 0
			 :del    (some? (get params (mk-tag "del" id)))}]
		(prn dc)
		(if-not (or (str/blank? (:date dc))
			        (s/valid? valid? dc))
			(throw (Exception. (s/explain-str valid? dc))))
		dc))


;;-----------------------------------------------------------------------------

