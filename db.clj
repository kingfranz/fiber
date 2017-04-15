(ns fiberweb.db
  	(:require 	[lobos.config :refer :all]
  				(garden 	[core       :as g]
            				[units      :as u]
            				[selectors  :as sel]
            				[stylesheet :as stylesheet]
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
            	(honeysql   [core       :as sql]
            				[helpers    :refer :all])
            	(clojure 	[string     :as str]
            				[set        :as set]
            				[spec       :as s])))

;;-----------------------------------------------------------------------------

(defdb fiberdb fiber-db)

(declare MEMBERS ESTATES MEMBERS_ESTATES CONTACTS CONFIGS MEMBER_DCS ESTATE_DCS ESTATE_BI ESTATE_ACT)

(defentity MEMBERS
	(entity-fields :ID :NAME :NOTE :FROM_YEAR :TO_YEAR)
	(has-many CONTACTS)
	(has-many MEMBER_DCS)
	(has-many MEMBERS_ESTATES))

(defentity estates
	(entity-fields :ID :LOCATION :ADDRESS :NOTE :FROM_YEAR :TO_YEAR)
	(has-many ESTATE_DCS)
	(has-many ESTATE_BI)
	(has-many ESTATE_ACT)
	(has-many MEMBERS_ESTATES))

(defentity MEMBERS_ESTATES
	(entity-fields :FROM_YEAR :TO_YEAR)
	(belongs-to MEMBERS {:fk :MEMBER_ID})
	(belongs-to ESTATES {:fk :ESTATE_ID}))

(defentity CONTACTS
	(entity-fields :TYPE :VALUE :PREFERRED)
	(belongs-to MEMBERS {:fk :MEMBER_ID}))

(defentity CONFIGS
	(entity-fields :ENTERED
				   :MEMBERSHIP_FEE :MEMBERSHIP_TAX
				   :CONNECTION_FEE :CONNECTION_TAX
				   :OPERATOR_FEE   :OPERATOR_TAX
				   :FROM_YEAR)
	(pk :ENTERED))

(defentity MEMBER_DCS
	(entity-fields :DATE
				   :AMOUNT
				   :TAX
				   :TYPE
				   :YEAR)
	(belongs-to MEMBERS {:fk :MEMBER_ID}))

(defentity ESTATE_DCS
	(entity-fields :DATE
				   :AMOUNT
				   :TAX
				   :TYPE
				   :YEAR
				   :MONTHS)
	(belongs-to ESTATES {:fk :ESTATE_ID}))

(defentity ESTATE_BI
	(entity-fields :YEAR
				   :BI_MONTHS)
	(belongs-to ESTATES {:fk :ESTATE_ID}))

(defentity ESTATE_ACT
	(entity-fields :YEAR
				   :ACT_MONTH)
	(belongs-to ESTATES {:fk :ESTATE_ID}))

;;-----------------------------------------------------------------------------

(defn within-ft
	[f t y]
	(and (<= f y)
		 (or (nil? t) (>= t y))))

(defn estate-id-exists?
	[id]
	(seq (select ESTATES
		(where (= :ID id)))))

(defn member-id-exists?
	[id]
	(seq (select MEMBERS
		(where (= :ID id)))))

(defn member-count
	[]
	(count (select MEMBERS
		(fields :ID))))

(defn estate-count
	[]
	(count (select ESTATES
		(fields :ID))))

(defn insert-estate
	[estate]
	(insert ESTATES
		(values estate)))

(defn insert-member
	[member]
	(insert MEMBERS
		(values member)))

(defn insert-config
	[config]
	(insert CONFIGS
		(values (assoc config :ENTERED [<= (sqlfn now)]))))

(defn update-estate
	[estate]
	(update ESTATES
		(set-fields (dissoc estate :ID))
		(where (= :ID (:id estate)))))

(defn update-member
	[member]
	(update MEMBERS
		(set-fields (dissoc member :id))
		(where (= :ID (:id member)))))

(defn get-estates
	[member]
	{:pre [(s/valid? :fiber/member member)]
	 :post [(coll? %)]}
	(select ESTATES
		(with MEMBERS_ESTATES
			(where (= :MEMBER_ID (:id member))))
		(where (= :ID :MEMBERS_ESTATES.ESTATE_ID))))

(defn get-contacts
	[member]
	{:pre [(s/valid? :fiber/member member)]
	 :post [(coll? %)]}
	(select CONTACTS
		(where (= :MEMBER_ID (:id member)))))

(defn get-member-dcs
	[member]
	{:pre [(s/valid? :fiber/member member)]
	 :post [(coll? %)]}
	(select MEMBER_DCS
		(where (= :MEMBER_ID (:id member)))))

(defn get-estate-dcs
	[estate]
	{:pre [(s/valid? :fiber/estate estate)]
	 :post [(coll? %)]}
	(select ESTATE_DCS
		(where (= :ESTATE_ID (:id estate)))))

(defn get-owners
	[estate year]
	{:pre [(s/valid? :fiber/valid-year year) (s/valid? :fiber/estate estate)]
	 :post [(coll? %)]}
	(select MEMBERS
		(where {:ID [in (subselect MEMBERS_ESTATES
							(fields :MEMBER_ID)
							(where (and (= :ESTATE_ID (:id estate))
										(and (<= :FROM_YEAR year) (or (nil? :TO_YEAR) (>= :TO_YEAR year))))))]})))

(defn get-owner
	([estate]
	{:pre [(s/valid? :fiber/estate estate)]
	 :post [(s/valid? :fiber/member %)]}
	(select MEMBERS
		(where {:ID [in (subselect MEMBERS_ESTATES
							(fields :MEMBER_ID)
							(where (and (= :ESTATE_ID (:id estate))
										(nil? :TO_YEAR))))]})))
	([estate year month]
	{:pre [(s/valid? :fiber/valid-year year)
		   (s/valid? :fiber/estate estate)
		   (s/valid? :fiber/valid-month month)]
	 :post [(s/valid? :fiber/member %)]}
	(select MEMBERS
		(where {:ID [in (subselect MEMBERS_ESTATES
							(fields :MEMBER_ID)
							(where (and (= :ESTATE_ID (:id estate))
										(and (<= :FROM_YEAR year) (or (nil? :TO_YEAR) (>= :TO_YEAR year))))))]}))))

(defn- get-member-debt
	[id year]
	(let [result (select MEMBER_DCS
					(fields :AMOUNT :TAX)
					(where (and (= :YEAR year) (= :MEMBER_ID id))))]
		(reduce
			(fn [{a1 :AMOUNT t1 :TAX tot1 :TO_YEARTAL}
				 {a2 :AMOUNT t2 :TAX tot2 :TO_YEARTAL}]
				{:amount (+ a1 a2) :tax (+ t1 t2) :total (+ tot1 tot2)})
			{:amount 0M :tax 0M :total 0M}
			(map
				#(assoc % :total (+ (:amount %) (:tax %)))
				result))))

(defn get-membership-data
	[year]
	(let [result (select MEMBERS
					(fields :ID :NAME [:CONTACTS.VALUE :CONTACT])
					(with CONTACTS
						(where (true? :PREFERRED)))
					(where (and (<= :FROM_YEAR year) (or (nil? :TO_YEAR) (>= :TO_YEAR year)))))]
		(remove
			#(>= (:total %) 0)
			(map
				#(merge % (get-member-debt (:ID %) year))
				result))))

(defn- get-estate-debt
	[id year monthmask]
	(let [con (select ESTATE_DCS
				(fields :CON-AMOUNT :CON-TAX)
				(aggregate (sum :AMOUNT) :CON-AMOUNT)
				(aggregate (sum :TAX) :CON-TAX)
				(where (and (= :YEAR year)
							(not= (bit-and :MONTHS monthmask) 0)
							(= :ESTATE_ID id)
							(= :TYPE "connection-fee"))))
		  op  (select ESTATE_DCS
				(fields :OP-AMOUNT :OP-TAX)
				(aggregate (sum :AMOUNT) :OP-AMOUNT)
				(aggregate (sum :TAX) :OP-TAX)
				(where (and (= :YEAR year)
							(not= (bit-and :MONTHS monthmask) 0)
							(= :ESTATE_ID id)
							(= :TYPE "operator-fee"))))
		  pay (select ESTATE_DCS
				(fields :PAY-AMOUNT)
				(aggregate (sum :AMOUNT) :PAY-AMOUNT)
				(where (and (= :YEAR year)
							(not= (bit-and :MONTHS monthmask) 0)
							(= :ESTATE_ID id)
							(= :TYPE "payment"))))
		  data (merge (first con) (first op) (first pay))]
		(assoc data :TO_YEARTAL (reduce + (vals data)))))

(defn get-usage-data
	[year monthmask]
	(->> (select ESTATES
			(fields :ADDRESS
					[:ID :ESTATE-ID]
					[:MEMBERS.NAME :NAME]
					[:MEMBERS.ID :ID]
					[:CONTACTS.VALUE :CONTACT])
			(with MEMBERS_ESTATES
				(where (and (<= :FROM_YEAR year) (or (nil? :TO_YEAR) (>= :TO_YEAR year))))
				(with MEMBERS
					(with CONTACTS
						(where (true? :PREFERRED)))))
			(where (and (<= :FROM_YEAR year) (or (nil? :TO_YEAR) (>= :TO_YEAR year))))
			(with ESTATE_BI
				(where (= :BI_MONTHS 12)))
			(with ESTATE_ACT
				(where (not= (bit-and :ACT_MONTHS monthmask) 0))))
		(map #(merge % (get-estate-debt (:ESTATE-ID %) year monthmask)))
		(remove #(>= (:total %) 0))))

;;------------------------------------------------------------------------------------

(defn add-member-payment
	[member date amount tax year]
	{:pre [(s/valid? :fiber/date date)
		   (decimal? amount) (not= amount 0M) (decimal? tax)
		   (s/valid? :fiber/valid-year year)]}
	(insert MEMBER_DCS
		(values {:DATE      date
				 :AMOUNT    amount
				 :TAX       tax
				 :TYPE      (name :payment)
				 :YEAR      year
				 :MEMBER_ID (:id member)})))

(defn add-estate-payment
	[estate date amount tax id year months]
	{:pre [(s/valid? :fiber/date date)
		   (decimal? amount) (not= amount 0M) (decimal? tax)
		   (s/valid? :fiber/valid-year year) (s/valid? :fiber/months months)]}
	(insert ESTATE_DCS
		(values {:DATE      date
				 :AMOUNT    amount
				 :TAX       tax
				 :TYPE      (name :payment)
				 :YEAR      year
				 :MONTHS    months
				 :ESTATE_ID id})))

;;------------------------------------------------------------------------------------

(defn get-estates-at
	[year]
	(select ESTATES
		(where (and (<= :FROM_YEAR year) (or (nil? :TO_YEAR) (>= :TO_YEAR year))))))

(defn get-members-with-estates
	[year]
	(select MEMBERS
		(order :NAME :ASC)
		(where {:ID [in (subselect MEMBERS_ESTATES
						(fields :ID)
						(and (<= :FROM_YEAR year) (or (nil? :TO_YEAR) (>= :TO_YEAR year))))]})))

;;------------------------------------------------------------------------------------

(defn get-members-not-charged-membership
	[year]
	{:pre [(s/valid? :fiber/valid-year year)]}
	(->> (select MEMBERS
			(where {:ID [not-in (subselect MEMBER_DCS
								(fields :MEMBER_ID)
								(where (and (= :YEAR year)
											(= :TYPE "membership-fee"))))]})
			(where (and (<= :FROM_YEAR year) (or (nil? :TO_YEAR) (>= :TO_YEAR year)))))))

;;------------------------------------------------------------------------------------

(defn get-config-at
	[yearmonth]
	(last (select CONFIGS
			(where (<= :FROM_YEAR yearmonth))
			(order :FROM_YEAR :ASC))))

