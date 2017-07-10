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
            	(cheshire 		[core     		:refer :all])
				(monger 		[core     		:as mg]
            					[credentials 	:as mcr]
            					[collection 	:as mc]
            					[joda-time  	:as jt]
            					[operators 		:refer :all])
            	(clojure 		[string     :as str]
            					[set        :as set]
            					[spec       :as s])))

;;-----------------------------------------------------------------------------

(defonce db-conn (mg/connect-with-credentials "127.0.0.1"
							(mcr/create "fiberuser" "fiberdb" "kAllE.kUlA399")))
(defonce fiberdb (mg/get-db db-conn "fiberdb"))

(defonce configs "configs")
(defonce members "members")
(defonce estates "estates")

;;-----------------------------------------------------------------------------

(s/fdef fname
	:args (s/cat :s :shop/string)
	:ret :shop/string)

;monger.collection$find_one_as_map@5f2b4e24users
(defn fname
	[s]
	(second (re-matches #"^[^$]+\$(.+)@.+$" (str s))))

(defn- do-mc
	[mc-func caller tbl & args]
	(log/trace (apply str caller ": " (fname mc-func) " " tbl " " (first args)))
	(let [ret (apply mc-func shopdb tbl (first args))]
		(log/trace caller "returned:" (pr-str ret))
		ret))

(defn mc-aggregate
	[func tbl & args]
	(do-mc mc/aggregate func tbl args))

(defn mc-find-maps
	[func tbl & args]
	(do-mc mc/find-maps func tbl args))

(defn mc-find-one-as-map
	[func tbl & args]
	(do-mc mc/find-one-as-map func tbl (vec args)))

(defn mc-find-map-by-id
	[func tbl & args]
	(do-mc mc/find-map-by-id func tbl args))

(defn mc-insert
	[func tbl & args]
	(do-mc mc/insert func tbl args))

(defn mc-insert-batch
	[func tbl & args]
	(do-mc mc/insert-batch func tbl args))

(defn mc-update
	[func tbl & args]
	(do-mc mc/update func tbl args))

(defn mc-update-by-id
	[func tbl & args]
	(do-mc mc/update-by-id func tbl args))

(defn mc-remove-by-id
	[func tbl & args]
	(do-mc mc/remove-by-id func tbl args))

;;-----------------------------------------------------------------------------

(s/fdef mk-enlc :args :shop/string :ret :shop/string)

(defn mk-enlc
	[en]
	(-> en str/trim str/lower-case (str/replace #"[ \t-]+" " ")))

(s/fdef get-by-enlc
	:args (s/cat :tbl :shop/string :en :shop/string)
	:ret (s/nilable map?))

(defn get-by-enlc
	[tbl en]
	(mc-find-one-as-map "get-by-enlc" tbl {:entrynamelc en}))

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
	[estate]
	(when (estateid-exists? (:_id estate))
		(throw (Exception. "duplicate estate ID")))
	(mc-insert "add-estate" estates estate))

(defn insert-config
	[config]
	(j/insert! fiberdb :configs config))

(defn update-estate
	[estate-part year bi]
	(mc-update-by-id "update-estate" estates (:_id estate)
		{$set (dissoc estate :_id)}))

(defn update-estatebi
	[eid from-year yearly?]
	(j/update! fiberdb :estatebi
		{:bimonths (if yearly? 12 3)} ["estateid = ? and year >= ?" eid from-year]))


(defn update-estatedc
	[dc]
	(j/update! fiberdb :estatedcs
		dc ["estateid = ? and estatedcid = ?" (:estateid dc) (:estatedcid dc)]))


(defn add-estatedc
	[dc]
	(j/insert! fiberdb :estatedcs dc))

(defn delete-estatedc
	[eid idx]
	(mc-update-by-id "delete-estatedc" estates eid
		{$unset {(str "dcs." idx) }}))


(defn update-memberdc
	[dc]
	(j/update! fiberdb :memberdcs
		dc ["memberid = ? and memberdcid = ?" (:memberid dc) (:memberdcid dc)]))


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

(defn get-memberdcs-all
	[]
	(j/query fiberdb
		["select *
		  from fiberdb.memberdcs"]))

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

(defn get-estatedcs-all
	[]
	(j/query fiberdb
		["select *
		  from fiberdb.estatedcs"]))

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
	(let [whole-year (t/interval (t/date-time year 1 1) (t/date-time year 12 31))
		  year-match (fn [{mid :_id {from :from to :to} :from-to}]
		  				(and (t/within? whole-year from)
		  					 (or (nil? to) (t/within? whole-year to))))
		  mids (filter year-match (:owners estate))]
		(mc-find-maps "get-owners" members
			{:_id {$in mids}})))

(defn get-owner
	[estate]
	(when-let [mid (utils/find-first #(nil? (-> % :from-to :to)) (:owners estate))]
		(mc-find-map-by-id "get-owner" members mid)))

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

(defn update-member-estate
	[me]
	(j/update! fiberdb :membersestates me
		["memberid = ? and estateid = ?" (:memberid me) (:estateid me)]))

(defn insert-member-estate
	[me]
	(j/insert! fiberdb :membersestates me))

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
<<<<<<< 35b96e22334c1f5fb988d6dfba4021266c45d5e8
=======

>>>>>>> work in progress

; mongoimport -u fiberuser -p "kAllE.kUlA399" --authenticationDatabase fiberdb --db fiberdb --file estates.json --drop --jsonArray
