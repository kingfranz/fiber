(ns fiberweb.db
  	(:require 	(garden 	[core       :as g]
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
            	(clojure.java [jdbc :as j])
            	(clojure 	[string     :as str]
            				[set        :as set]
            				[spec       :as s])))

;;-----------------------------------------------------------------------------

(defonce fiberdb {
	:classname   "com.mysql.cj.jdbc.Driver"
	:host        "diskstation.soahojen"
    :port        3306
    :dbtype      "mysql"
    ;:subprotocol "mysql"
    ;:subname "//192.168.0.42:3306/fiberdb"
    :dbname      "fiberdb?serverTimezone=CAT"
    :user        "sa"
    :password    "wiggins"})

;;-----------------------------------------------------------------------------

(defn estateid-exists?
	[id]
	(seq (j/query fiberdb
  		["select *
  		  from fiberdb.estates
  		  where estateid = ?" id])))

(defn member-id-exists?
	[id]
	(seq (j/query fiberdb
  		["select *
  		  from fiberdb.members
  		  where memberid = ?" id])))

(defn member-count
	[]
	(j/query fiberdb
  		["select count(*)
  		  from fiberdb.members"]))

(defn estate-count
	[]
	(j/query fiberdb
  		["select count(*)
  		  from fiberdb.estates"]))

(defn insert-estate
	[estate]
	(j/insert! fiberdb :estates estate))

(defn insert-member
	[member]
	(j/insert! fiberdb :members member))

(defn insert-config
	[config]
	(j/insert! fiberdb :configs config))

(defn update-estate
	[estate]
	(j/update! fiberdb :estates
		(dissoc estate :estateid) ["estateid = ?" (:estateid estate)]))

(defn update-member
	[member]
	(j/update! fiberdb :members
		(dissoc member :memberid) ["memberid = ?" (:memberid member)]))

(defn get-member
	[id]
	(first (j/query fiberdb
		["select *
		  from members
		  where memberid = ?" id])))

(defn get-members
	[]
	(j/query fiberdb
		["select *
		  from members"]))

(defn get-estates
	[member]
	(j/query fiberdb
		["select e.*
		  from fiberdb.estates e
		  inner join fiberdb.membersestates me on me.estateid = e.estateid
		  where me.memberid = ?" (:memberid member)]))

(defn get-contacts
	[member]
	(j/query fiberdb
		["select *
		  from fiberdb.contacts
		  where memberid = ?" (:memberid member)]))

(defn get-memberdcs
	[member]
	(j/query fiberdb
		["select *
		  from fiberdb.memberdcs
		  where memberid = ?" (:memberid member)]))

(defn get-estatedcs
	[estate]
	(j/query fiberdb
		["select *
		  from fiberdb.estatedcs
		  where estateid = ?" (:estateid estate)]))

(defn get-owners
	[estate year]
	(j/query fiberdb
		["select m.*
		  from fiberdb.members m
		  inner join fiberdb.membersestates me on me.memberid = m.memberid
		  where me.estateid = ? and
		        me.fromyear <= ? and
		        (me.toyear is NULL or me.toyear >= ?)"
		  (:estateid estate) year year]))

(defn get-owner
	([estate]
	(j/query fiberdb
		["select m.*
		  from fiberdb.members m
		  inner join fiberdb.membersestates me on me.memberid = m.memberid
		  where me.estateid = ? and
		        me.toyear is NULL"
		  (:estateid estate)]))
	([estate year month]
	(j/query fiberdb
		["select m.*
		  from fiberdb.members m
		  inner join fiberdb.membersestates me on me.memberid = m.memberid
		  where me.estateid = ? and
		        me.fromyear <= ? and
		        (me.toyear is NULL or me.toyear >= ?)"
		  (:estateid estate) year year])))

(defn- get-member-debt
	[id year]
	(let [result (j/query fiberdb
  		["select dcs.amount, dcs.tax
  		  from fiberdb.memberdcs dcs
  		  where dcs.year = ? and dcs.memberid = ?" year id])]
		;(println "debt:" result)
		(reduce
			(fn [{a1 :amount t1 :tax tot1 :total}
				 {a2 :amount t2 :tax tot2 :total}]
				{:amount (+ a1 a2) :tax (+ t1 t2) :total (+ tot1 tot2)})
			{:amount 0M :tax 0M :total 0M}
			(map
				#(assoc % :total (+ (:amount %) (:tax %)))
				result))))



(defn get-membership-data
	[year]
	(let [result (j/query fiberdb
  		["select m.memberid as memberid, m.name as name, c.value as contact
  		  from fiberdb.members m
  		  inner join fiberdb.contacts c on c.memberid = m.memberid
  		  where c.preferred = TRUE and 
  		  m.fromyear <= ? and
  		  (m.toyear IS NULL or m.toyear = ?)" year year])]
		;(println "gmd:" year (type result) (count result) result)
		(remove
			#(>= (:total %) 0)
			(map
				#(merge % (get-member-debt (:memberid %) year))
				result))))

(defn- get-estate-debt
	[id year monthmask]
	(let [con (j/query fiberdb
				["select ifnull(sum(amount), 0) as conamount,
				         ifnull(sum(tax), 0) as contax
				  from fiberdb.estatedcs
				  where year = ? and type = 'connection-fee' and
				        estateid = ? and (months & ?) <> 0" year id monthmask])
		  op  (j/query fiberdb
				["select ifnull(sum(amount),0) as opamount,
				         ifnull(sum(tax), 0) as optax
				  from fiberdb.estatedcs
				  where year = ? and type = 'operator-fee' and
				        estateid = ? and (months & ?) <> 0" year id monthmask])
		  pay (j/query fiberdb
				["select ifnull(sum(amount), 0) as payamount,
				         ifnull(sum(tax), 0) as paytax
				  from fiberdb.estatedcs
				  where year = ? and type = 'payment' and
				        estateid = ? and (months & ?) <> 0" year id monthmask])
		  data (merge (first con) (first op) (first pay))]
		;(println "ged-con:" con)
		;(println "ged-op:" op)
		;(println "ged-pay:" pay)
		(assoc data :total (reduce + (vals data)))))

(defn get-usage-data
	[year monthmask bi-months]
	(->> (j/query fiberdb
		["select distinct m.memberid as memberid, m.name as name, e.id as estateid,
			              e.address as address, c.value as contact
		 from (((((fiberdb.estates e
		 inner join fiberdb.membersestates me on me.estateid = e.estateid)
		 inner join fiberdb.members m on me.memberid = m.memberid)
		 inner join fiberdb.contacts c on c.memberid = m.memberid)
		 inner join fiberdb.estatebi eb on eb.estateid = e.estateid)
		 inner join fiberdb.estateact ea on ea.estateid = e.estateid)
		 where me.fromyear <= ? and
		       (me.toyear is NULL or me.toyear >= ?) and
			   c.preferred = TRUE and eb.bimonths = ? and
			   (ea.actmonths & ?) <> 0" year year bi-months monthmask])
		(map #(merge % (get-estate-debt (:estateid %) year monthmask)))
		(remove #(>= (:total %) 0M))))

;;------------------------------------------------------------------------------------

(defn add-member-payment
	[member date amount tax year]
	{:pre [(s/valid? :fiber/date date)
		   (decimal? amount) (not= amount 0M) (decimal? tax)
		   (s/valid? :fiber/valid-year year)]}
	(j/insert! fiberdb :memberdcs
		{:date date
		 :amount amount
		 :tax tax
		 :type "payment"
		 :year year
		 :memberid (:memberid member)}))

(defn add-estate-payment
	[estate date amount tax id year months]
	{:pre [(s/valid? :fiber/date date)
		   (decimal? amount) (not= amount 0M) (decimal? tax)
		   (s/valid? :fiber/valid-year year) (s/valid? :fiber/months months)]}
	(j/insert! fiberdb :estatedcs
		{:date date
		 :amount amount
		 :tax tax
		 :type "payment"
		 :year year
		 :months months
		 :estateid (:estateid estate)}))

;;------------------------------------------------------------------------------------

(defn get-estates-at
	[year]
	(j/query fiberdb
		["select *
		  from fiberdb.estates
		  where fromyear <= ? and
		       (toyear is NULL or toyear >= ?)" year year]))

(defn get-members-with-estates
	[year]
	(j/query fiberdb
		["select m.*
		  from fiberdb.members m
		  inner join fiberdb.membersestates me on me.memberid = m.memberid
		  where me.fromyear <= ? and
		       (me.toyear is NULL or me.toyear >= ?)
		  oreder by m.name" year year]))

;;------------------------------------------------------------------------------------

(defn get-members-not-charged-membership
	[year]
	{:pre [(s/valid? :fiber/valid-year year)]}
	(j/query fiberdb
  		["select m.*
  		  from fiberdb.members m
  		  inner join fiberdb.memberdcs dcs on dcs.memberid = m.memberid
  		  where dcs.year <= ? and dcs.type = 'membership-fee'
  		  and m.fromyear <= ? and (m.toyear is NULL or m.toyear >= ?)" year year year]))

;;------------------------------------------------------------------------------------

(defn get-config-at
	[year]
	(last (j/query fiberdb
  		["select *
  		  from fiberdb.configs
  		  where fromyear <= ?
  		  order by fromyear" year])))


