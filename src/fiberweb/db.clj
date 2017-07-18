(ns fiberweb.db
  	(:require 	(garden 		[core       :as g]
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
            	(taoensso 		[timbre     :as timbre])
            	(fiberweb		[spec       :as spec]
            					[utils      :as utils])
            	(clojure.java 	[jdbc       :as j])
            	(clojure 		[string     :as str]
            					[set        :as set]
            					[spec       :as s])))

;;-----------------------------------------------------------------------------

(defonce fiberdb* {
	:classname   "com.mysql.cj.jdbc.Driver"
	:host        "192.168.0.42"
    :port        3306
    :dbtype      "mysql"
    :dbname      "fiberdb?serverTimezone=CAT"
    :user        "sa"
    :password    "wiggins"})

(defonce fiberdb {
	:classname   "com.mysql.cj.jdbc.Driver"
	:host        "127.0.0.1"
    :port        3306
    :dbtype      "mysql"
    :dbname      "fiberdb?serverTimezone=CAT"
    :user        "root"
    :password    "wiggins"})

;;-----------------------------------------------------------------------------

(defn estateid-exists?
	[id]
	(seq (j/query fiberdb
  		["select *
  		  from fiberdb.estates
  		  where estateid = ?" id])))

(defn memberid-exists?
	[id]
	(seq (j/query fiberdb
  		["select *
  		  from fiberdb.members
  		  where memberid = ?" id])))

(defn member-count
	[]
	(:count (first (j/query fiberdb
  		["select count(*) as count
  		  from fiberdb.members
  		  where toyear is NULL"]))))

(defn estate-count
	[]
	(:count (first (j/query fiberdb
  		["select count(*) as count
  		  from fiberdb.estates
  		  where toyear is NULL"]))))

(defn add-estate
	[estate bimonths]
	(when (estateid-exists? (:estateid estate))
		(throw (Exception. "duplicate estate ID")))
	(j/with-db-transaction [db-conn fiberdb]
		(j/insert! db-conn :estates estate)
		(doseq [year (range (utils/current-year) 2031)]
			(j/insert! db-conn :estatebi {
				:estateid (:estateid estate)
				:year     year
				:bimonths bimonths}))))
(s/fdef add-estate
	:args (s/cat :estate :fiber/estate :bimonths :estate/bi-months))

(defn insert-config
	[config]
	(j/insert! fiberdb :configs config))

(defn update-estate
	[estate]
	(j/update! fiberdb :estates
		estate ["estateid = ?" (:estateid estate)]))

(defn update-estatebi
	[eid from-year yearly?]
	(j/update! fiberdb :estatebi
		{:bimonths (if yearly? 12 3)} ["estateid = ? and year >= ?" eid from-year]))

(s/fdef update-estatedc
	:args :fiber/estate-dc-entry)

(defn update-estatedc
	[dc]
	(j/update! fiberdb :estatedcs
		dc ["estateid = ? and estatedcid = ?" (:estateid dc) (:estatedcid dc)]))

(s/fdef add-estatedc
	:args :fiber/estate-dc-entry)

(defn add-estatedc
	[dc]
	(j/insert! fiberdb :estatedcs dc))

(defn delete-estatedc
	[eid dcid]
	(j/delete! fiberdb :estatedcs ["estateid = ? and estatedcid = ?" eid dcid]))

(s/fdef update-memberdc
	:args :fiber/member-dc-entry)

(defn update-memberdc
	[dc]
	(j/update! fiberdb :memberdcs
		dc ["memberid = ? and memberdcid = ?" (:memberid dc) (:memberdcid dc)]))

(s/fdef add-memberdc
	:args :fiber/member-dc-entry)

(defn add-memberdc
	[dc]
	(j/insert! fiberdb :memberdcs dc))

(defn delete-memberdc
	[mid dcid]
	(j/delete! fiberdb :memberdcs ["memberid = ? and memberdcid = ?" mid dcid]))

(defn update-member
	[member]
	(j/update! fiberdb :members
		member ["memberid = ?" (:memberid member)]))

(defn get-member
	[id]
	(first (j/query fiberdb
		["select *
		  from members
		  where memberid = ?" id])))

(defn get-members
	([]
	(j/query fiberdb
		["select *
		  from members"]))
	([year]
	(j/query fiberdb
		["select *
		  from members
		  where fromyear <= ? and
		        (toyear is NULL or toyear >= ?)" year year])))

(defn get-estate
	[id]
	(first (j/query fiberdb
		["select *
		  from estates
		  where estateid = ?" id])))

(defn get-membersestates
	[]
	(j/query fiberdb
		["select *
		  from fiberdb.membersestates"]))

(defn get-estates
	([]
	(j/query fiberdb
		["select *
		  from fiberdb.estates"]))
	([memberid]
	(j/query fiberdb
		["select *
		  from fiberdb.member_estates
		  where memberid = ?" memberid]))
	([memberid year]
	(j/query fiberdb
		["select *
		  from fiberdb.member_estates
		  where memberid = ? and
		        me_fromyear <= ? and
		        (me_toyear is NULL or me_toyear >= ?)" memberid year year])))

(defn get-avail-estates
	[]
	(let [estates (get-estates)
		  mes*    (get-membersestates)
		  mes     (->> mes*
		  			   (group-by :estateid)
		  			   (map (fn [[k v]] [k (every? #(some? (:toyear %)) v)]))
		  			   (filter #(true? (second %)))
		  			   (map first)
		  			   set)]
		(concat (filter #(some #{(:estateid %)} mes) estates)
				(remove #(some #{(:estateid %)} (set (map :estateid mes*))) estates))))

(defn get-contacts-all
	[]
	(j/query fiberdb
		["select *
		  from fiberdb.contacts"]))

(defn get-contacts
	[memberid]
	(j/query fiberdb
		["select *
		  from fiberdb.contacts
		  where memberid = ?
		  order by preferred DESC" memberid]))
(s/fdef get-contacts
	:args :fiber/id
	:ret  (s/* :fiber/contact))

(defn delete-contact
	[contact]
	(j/delete! fiberdb :contacts
		["memberid = ? and contactid = ?" (:memberid contact) (:contactid contact)]))

(defn update-contact
	[contact]
	(j/update! fiberdb :contacts contact
		["memberid = ? and contactid = ?" (:memberid contact) (:contactid contact)]))

(defn add-contact
	[contact]
	(j/insert! fiberdb :contacts contact))

(defn add-config
	[config]
	(j/insert! fiberdb :configs config))

(defn get-memberdcs
	[memberid]
	(j/query fiberdb
		["select *
		  from fiberdb.memberdcs
		  where memberid = ?
		  order by date" memberid]))

(defn get-memberdcs-all
	[]
	(j/query fiberdb
		["select *
		  from fiberdb.memberdcs"]))

(defn get-memberdc
	[memberid dcid]
	(j/query fiberdb
		["select *
		  from fiberdb.memberdcs
		  where memberid = ? and
		        memberdcid = ?" memberid dcid]))

(defn get-estatedcs
	[estateid]
	(j/query fiberdb
		["select *
		  from fiberdb.estatedcs
		  where estateid = ?" estateid]))

(defn get-estatedcs-all
	[]
	(j/query fiberdb
		["select *
		  from fiberdb.estatedcs"]))

(defn get-estatedc
	[estateid dcid]
	(j/query fiberdb
		["select *
		  from fiberdb.estatedcs
		  where estateid = ? and
		        estatedcid = ?" estateid dcid]))

(defn get-estatebi
	[estateid]
	(j/query fiberdb
		["select *
		  from fiberdb.estatebi
		  where estateid = ?" estateid]))

(defn get-estatebi-all
	[]
	(j/query fiberdb
		["select *
		  from fiberdb.estatebi"]))

(defn get-estatebi-year
	[year]
	(j/query fiberdb
		["select *
		  from fiberdb.estatebi
		  where year = ?" year]))

(defn get-owners
	[estate year]
	(j/query fiberdb
		["select memberid, name, note, fromyear, toyear
		  from fiberdb.estate_owners
		  where estateid = ? and
		        fromyear <= ? and
		        (toyear is NULL or toyear >= ?)"
		  (:estateid estate) year year]))

(defn get-owner
	([estateid]
	(first (j/query fiberdb
		["select memberid, name, note, fromyear, toyear
		  from fiberdb.estate_owners
		  where estateid = ? and
		        toyear is NULL"
		  estateid])))
	([estateid year]
	(first (j/query fiberdb
		["select memberid, name, note, fromyear, toyear
		  from fiberdb.estate_owners
		  where estateid = ? and
		        fromyear <= ? and
		        (toyear is NULL or toyear >= ?)"
		  estateid year year]))))

(defn get-membership-data
	[year]
	(j/query fiberdb
  		["select *
  		  from fiberdb.member_contact_sum
  		  where fromyear <= ? and (toyear IS NULL or toyear = ?) and
  		        total < 0" year year]))

(defn- get-estate-debt
	[id year monthmask]
	(first (j/query fiberdb
		["select *
		  from fiberdb.estatedc_sums
		  where year = ? and estateid = ? and
		        (conmonths & ?) <> 0 and
		        (opmonths & ?) <> 0" year id monthmask monthmask])))

(defn get-usage-data
	[year monthmask bi-months]
	(->> (j/query fiberdb
		["select distinct memberid, name, estateid, address, contact
		  from fiberdb.estate_usage
		  where fromyear <= ? and (toyear is NULL or toyear >= ?) and
			    year = ? and bimonths = ? and (actmonths & ?) <> 0
		  order by memberid"
			year year year bi-months monthmask])
		(map #(merge % (or (get-estate-debt (:estateid %) year monthmask) {:total 0M})))
		(remove #(>= (:total %) 0M))))

(defn get-activities
	[year]
	(j/query fiberdb
		["select distinct memberid, name, estateid, address, bimonths, actmonths
		  from fiberdb.estate_usage
		  where fromyear <= ? and (toyear is NULL or toyear >= ?) and
			    year = ?
		  order by memberid" year year year]))

(defn get-activities-all
	[]
	(j/query fiberdb
		["select *
		  from fiberdb.estateact"]))

(defn get-activities-for
	[year eid]
	(-> (j/query fiberdb
			["select actmonths
			  from fiberdb.estateact
		 	  where year = ? and estateid = ?" year eid])
		first
		:actmonths
		(or 0)))
(s/fdef get-activities-for
	:args (s/cat :year :fiber/valid-year :eid :fiber/id)
	:ret  integer?)

(defn get-acts
	[year]
	(j/query fiberdb
		["select ea.*
		  from fiberdb.estateact ea
		  inner join fiberdb.membersestates me on me.estateid = ea.estateid
		  where me.fromyear <= ? and
		        (me.toyear is NULL or me.toyear >= ?) and
		        ea.year = ?" year year year]))

(defn update-activity
	[eid year actmonths]
	(j/update! fiberdb :estateact
		{:actmonths actmonths}
		["estateid = ? and year = ?" eid year]))
(s/fdef update-activity
	:args (s/cat :eid :fiber/estateid :year :fiber/valid-year :actmonths :fiber/months))

(defn update-member-estate
	[me]
	(j/update! fiberdb :membersestates me
		["memberid = ? and estateid = ?" (:memberid me) (:estateid me)]))
(s/fdef update-member-estate
	:args :fiber/me-entry)

(defn insert-member-estate
	[me]
	(j/insert! fiberdb :membersestates me))
(s/fdef insert-member-estate
	:args :fiber/me-entry)

(defn get-member-cont
	[]
	(->> (j/query fiberdb
			["select memberid, name, fromyear, note
			  from fiberdb.members
		 	  where toyear is NULL"])
		 (map #(assoc % :contacts (get-contacts (:memberid %))))))

(defn get-all
	[]
	(j/query fiberdb
		["select *
		  from fiberdb.current_full
		  order by memberid"]))

(defn get-full
	[year]
	(let [members (j/query fiberdb [
			"select memberid, name, total
			 from fiberdb.member_contact_sum
			 where fromyear <= ? and (toyear is NULL or toyear >= ?)
			 order by memberid" year year])
		  estates (j/query fiberdb [
			"select estateid, address, total, memberid
			from fiberdb.estate_sums
			where year = ?" year])]
		 (->> members
		 	  (map (fn [member] (assoc member :estates (filter #(= (:memberid %) (:memberid member)) estates))))
		 	  (remove (fn [member] (and (>= (:total member) 0M)
		 	                            (>= (reduce + (map :total (:estates member))) 0M)))))))

(defn add-member
	[member eid contacts]
	(if (memberid-exists? (:memberid member))
		(throw (Exception. "duplicate member ID")))
	(if-not (estateid-exists? eid)
		(throw (Exception. "unknown estate ID")))
	(if (some? (get-owner eid))
		(throw (Exception. "estate not available")))
	(j/with-db-transaction [db-conn fiberdb]
		(j/insert! db-conn :members member)
		(j/insert! db-conn :membersestates {
			:memberid (:memberid member)
			:estateid eid
			:fromyear (:fromyear member)})
		(doseq [contact contacts]
			(j/insert! db-conn :contacts contact))))
(s/fdef add-member
	:args (s/cat :member :fiber/member :eid :fiber/estateid :contacts (s/+ :fiber/contact)))

;;------------------------------------------------------------------------------------

(defn add-member-payment
	[memberid amount year]
	{:pre [(decimal? amount) (not= amount 0M) (s/valid? :fiber/valid-year year)]}
	(j/insert! fiberdb :memberdcs
		{:amount amount
		 :tax 0M
		 :type "payment"
		 :year year
		 :memberid memberid}))

(defn add-estate-payment
	[estateid amount year]
	{:pre [(decimal? amount) (not= amount 0M) (s/valid? :fiber/valid-year (utils/spy year))]}
	(j/insert! fiberdb :estatedcs
		{:amount amount
		 :tax 0M
		 :type "payment"
		 :year year
		 :months 4095
		 :estateid estateid}))

;;------------------------------------------------------------------------------------

;(defn get-estates-at
;	[year]
;	(j/query fiberdb
;		["select *
;		  from fiberdb.estates
;		  where fromyear <= ? and
;		       (toyear is NULL or toyear >= ?)" year year]))

;(defn get-members-with-estates
;	[year]
;	(j/query fiberdb
;		["select m.*
;		  from fiberdb.members m
;		  inner join fiberdb.membersestates me on me.memberid = m.memberid
;		  where me.fromyear <= ? and
;		       (me.toyear is NULL or me.toyear >= ?)
;		  oreder by m.name" year year]))

;;------------------------------------------------------------------------------------

;(defn get-members-not-charged-membership
;	[year]
;	{:pre [(s/valid? :fiber/valid-year year)]}
;	(j/query fiberdb
 ; 		["select m.*
  ;		  from fiberdb.members m
  ;		  inner join fiberdb.memberdcs dcs on dcs.memberid = m.memberid
  ;		  where dcs.year <= ? and dcs.type = 'membership-fee'
  ;		  and m.fromyear <= ? and (m.toyear is NULL or m.toyear >= ?)" year year year]))

;;------------------------------------------------------------------------------------

(defn get-config-at
	[year]
	(last (j/query fiberdb
  		["select *
  		  from fiberdb.configs
  		  where fromyear <= ?
  		  order by fromyear" year])))

(defn get-configs
	[]
	(j/query fiberdb
  		["select *
  		  from fiberdb.configs"]))

; mongoimport -u fiberuser -p "kAllE.kUlA399" --authenticationDatabase fiberdb --db fiberdb --file estates.json --drop --jsonArray
