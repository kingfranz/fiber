(ns fiberweb.db
  	(:require 	(garden 		[core       	:as g]
            					[units      	:as u]
            					[selectors  	:as sel]
            					[stylesheet 	:as ss]
            					[color      	:as color])
             	(clj-time 		[core       	:as t]
            					[local      	:as l]
            					[format     	:as f]
            					[periodic   	:as p])
            	(hiccup 		[core       	:as h]
            					[def        	:as hd]
            					[element    	:as he]
            					[form       	:as hf]
            					[page       	:as hp]
            					[util       	:as hu])
            	(taoensso 		[timbre     	:as timbre])
            	(fiberweb		[spec       	:as spec]
            					[utils      	:as utils])
            	(cheshire 		[core     		:refer :all])
				(monger 		[core     		:as mg]
            					[credentials 	:as mcr]
            					[collection 	:as mc]
            					[joda-time  	:as jt]
            					[operators 		:refer :all])
            	(clojure 		[string     	:as str]
            					[set        	:as set]
            					[spec       	:as s])))

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
;; estate

(defn estateid-exists?
	[eid]
	(some? (mc-find-one-as-map "estateid-exists?" fiberdb eid)))
(s/fdef estateid-exists?
	:args :estate/_id
	:ret  boolean?)

(defn estate-count
	[]
	(mc/count fiberdb estates {}))
(s/fdef estate-count
	:ret integer?)

(defn add-estate
	[estate]
	(when (estateid-exists? (:_id estate))
		(throw (Exception. "duplicate estate ID")))
	(mc-insert "add-estate" estates estate))
(s/fdef add-estate
	:args :fiber/estate)

(defn update-estate
	[estate-part]
	(mc-update-by-id "update-estate" estates (:_id estate)
		{$set (dissoc estate :_id)}))
(s/fdef update-estate
	:args :fiber/estate-light)

(defn add-estatedc
	[eid dc]
	(mc-update-by-id "add-estatedc" estates eid
		{$push {:dcs dc}}))

(defn delete-estatedc
	[eid idx]
	(let [dcs     (mc-find-map-by-id "delete-estatedc" estates eid [dcs])
		  new-dcs (utils/drop-nth idx dcs)]
		(mc-update-by-id "delete-estatedc" estates eid
			{$set {:cs new-dcs}})))

(defn get-estate
	[id]
	(mc-find-map-by-id "get-estate" estates id))
(s/fdef get-estate
	:args :estate/estateid
	:ret  :fiber/estate)

(defn get-estates
	([]
	(mc-find-maps "get-estates" estates {}))
	([memberid]
	(mc-find-maps "get-estates" estates {:owners._id memberid}))
	([memberid year]
	(mc-find-maps "get-estates" estates {$and [{:owners._id memberid}
											   {:owners.from-to.from {$lte (t/date-time year 12 31)}}
											   {$or [{:owners.from-to.to nil}
											   		 {:owners.from-to.to {$gte (t/date-time year 1 1)}}]}]})))
(s/fdef get-estates
	:args (s/alt :none empty? :mid :estate/_id :mid-year (s/cat :mid :member/_id :year :fiber/year))
	:ret  (s/* :fiber/estate))

(defn get-estates-at
	[year]
	(mc-find-maps "get-estates-at" estates {$and [{:owners.from-to.from {$lte (t/date-time year 12 31)}}
											      {$or [{:owners.from-to.to nil}
											   		    {:owners.from-to.to {$gte (t/date-time year 1 1)}}]}]}))
(s/fdef get-estates-at
	:args :fiber/year
	:ret  (s/* :fiber/estate))

(defn update-activity
	[eid year months]
	(mc-update "update-activity" estates
		{:_id eid :activities.year year}
		{$set {:activities.$.months months}}))
(s/fdef update-activity
	:args (s/cat :eid :estate/estateid :year :fiber/year :months :fiber/months))

(defn add-estate-payment
	[estateid amount year]
	(mc-update-by-id "add-estate-payment" estates estateid
		{$set {:date   (l/local-now)
			   :amount amount
		 	   :tax    0M
			   :type   :payment
			   :year   year
			   :months (set (range 1 13))}}))
(s/fdef add-estate-payment
	:args (s/cat :estateid :estate/estateid :amount :fiber/amount :year :fiber/year))

;;-----------------------------------------------------------------------------
;; member

(defn memberid-exists?
	[mid]
	(some? (mc-find-one-as-map "memberid-exists?" fiberdb mid)))
(s/fdef memberid-exists?
	:args :fiber/memberid
	:ret  boolean?)

(defn member-count
	[]
	(mc/count fiberdb members {}))
(s/fdef member-count
	:ret integer?)

(defn add-memberdc
	[mid dc]
	(mc-update-by-id "add-memberdc" members mid
		{$push {:dcs dc}}))

(defn delete-memberdc
	[mid idx]
	(let [dcs     (mc-find-map-by-id "delete-memberdc" members mid [dcs])
		  new-dcs (utils/drop-nth idx dcs)]
		(mc-update-by-id "delete-memberdc" members mid
			{$set {:cs new-dcs}})))

(defn update-member
	[member]
	(mc-update-by-id members (:_id member)
		{$set member}))
(s/fdef update-member
	:args :fiber/member)

(defn get-member
	[id]
	(mc-find-one-as-map "get-member" members {:_id id}))
(s/fdef get-member
	:args :fiber/memberid
	:ret  :fiber/member)

(defn get-members
	([]
	(mc-find-maps "get-members" members {}))
	([year]
	(mc-find-maps "get-members" members
		{$and [{:from-to.from {$lte (t/date-time year 12 31)}}
		       {$or [{:from-to.to nil}
		             {:from-to.to {$gte (t/date-time year 1 1)}}]}]})))
(s/fdef get-members
	:args (s/? :fiber/year)
	:ret  (s/* :fiber/member))

(defn get-current-members
	[]
	(mc-find-maps "get-members" members
		{$and [{:from-to.from {$lte (l/local-now)}}
			   {:from-to.to nil}]}))

(defn add-member
	[member]
	(when (memberid-exists? (:_id member))
		(throw (Exception. "duplicate member ID")))
	(mc-insert "add-member" members member))
(s/fdef add-member
	:args :fiber/member)

(defn add-member-payment
	[memberid amount year]
	(mc-update-by-id "add-member-payment" members memberid
		{$set {:date (l/local-now)
			   :amount amount
		 	   :tax 0M
			   :type :payment
			   :year year}}))
(s/fdef add-member-payment
	:args (s/cat :memberid :member/memberid :amount :fiber/amount :year :fiber/year))

;;-----------------------------------------------------------------------------
;; config

(defn add-config
	[config]
	(mc-insert "insert-config" configs config))
(s/fdef insert-config
	:args :fiber/config)

(defn get-config-at
	[year]
	(->> (mc-find-maps "get-config-at" configs {:year {$lte (t/date-time year 12 31)}})
		 (sort-by :year)
		 last))
(s/fdef get-config-at
	:args :fiber/year
	:ret  :fiber/config)

(defn get-configs
	[]
	(mc-find-maps "get-configs" configs {}))
(s/fdef get-configs
	:ret  (s/* :fiber/config))

;;------------------------------------------------------------------------------------

; mongoimport -u fiberuser -p "kAllE.kUlA399" --authenticationDatabase fiberdb --db fiberdb --file estates.json --drop --jsonArray
