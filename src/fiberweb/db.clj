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
            	(taoensso 		[timbre     	:as log])
            	(fiberweb		[spec       	:as spec]
            					[utils      	:as utils]
            					[config         :as config])
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

;monger.collection$find_one_as_map@5f2b4e24users
(defn fname
	[s]
	(second (re-matches #"^[^$]+\$(.+)@.+$" (str s))))
(s/fdef fname
	:args (s/cat :s :fiber/string)
	:ret :fiber/string)

(defn- do-mc
	[mc-func caller tbl & args]
	(log/trace (apply str caller ": " (fname mc-func) " " tbl " " (first args)))
	(let [ret (apply mc-func fiberdb tbl (first args))]
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
	(mc-update-by-id "update-estate" estates (:_id estate-part)
		{$set (dissoc estate-part :_id)}))
(s/fdef update-estate
	:args :fiber/estate-light)

(defn add-estatedc
	[eid dc]
	(mc-update-by-id "add-estatedc" estates eid
		{$push {:dcs dc}}))
(s/fdef add-estatedc
	:args (s/cat :eid :estate/_id :dc :estate/dc-entry))

(defn delete-estatedc
	[eid idx]
	(let [dcs     (mc-find-map-by-id "delete-estatedc" estates eid ["dcs"])
		  new-dcs (utils/drop-nth idx dcs)]
		(mc-update-by-id "delete-estatedc" estates eid
			{$set {:cs new-dcs}})))
(s/fdef delete-estatedc
	:args (s/cat :eid :estate/_id :idx integer?))

(defn- upd-estate
	[e]
	(-> e
		(update :activities #(mapv (fn [a]
			{:year   (:year a)
			 :months (set (:months a))}) %))
		(update :dcs #(mapv (fn [dc] 
			{:date    (:date dc)
			 :amount  (bigdec (:amount dc))
			 :tax     (bigdec (:tax dc))
			 :dc-type (keyword (:dc-type dc))
			 :year    (:year dc)
			 :months  (set (:months dc))}) %))
		(update :billing-intervals #(mapv (fn [bi]
			{:year      (:year bi)
			 :bi-months (keyword (:bi-months bi))}) %))
		))

(defn- upd-estates
	[ms]
	(map upd-estate ms))

(defn get-estate
	[id]
	(upd-estate (mc-find-map-by-id "get-estate" estates id)))
(s/fdef get-estate
	:args :estate/_id
	:ret  :fiber/estate)

(defn get-estates
	([]
	(upd-estates (mc-find-maps "get-estates" estates {})))
	([memberid]
	(upd-estates (mc-find-maps "get-estates" estates {:owners._id memberid})))
	([memberid year]
	(upd-estates (mc-find-maps "get-estates" estates
		{$and [{:owners._id memberid}
			   {:owners.from-to.from {$lte (t/date-time year 12 31)}}
			   {$or [{:owners.from-to.to nil}
			   		 {:owners.from-to.to {$gte (t/date-time year 1 1)}}]}]}))))
(s/fdef get-estates
	:args (s/alt :none empty? :mid :estate/_id :mid-year (s/cat :mid :member/_id :year :fiber/year))
	:ret  (s/* :fiber/estate))

(defn get-estates-at
	[year]
	(upd-estates (mc-find-maps "get-estates-at" estates
		{$and [{:owners.from-to.from {$lte (t/date-time year 12 31)}}
		       {$or [{:owners.from-to.to nil}
				     {:owners.from-to.to {$gte (t/date-time year 1 1)}}]}]})))
(s/fdef get-estates-at
	:args :fiber/year
	:ret  (s/* :fiber/estate))

(defn update-activity
	[eid acts]
	(mc-update-by-id "update-activity" estates eid
		{$set {:activities acts}}))
(s/fdef update-activity
	:args (s/cat :eid :estate/_id :acts :estate/activities))

(defn add-estate-payment
	[eid amount year]
	(mc-update-by-id "add-estate-payment" estates eid
		{$set {:date   (l/local-now)
			   :amount amount
		 	   :tax    0M
			   :type   :payment
			   :year   year
			   :months (set config/month-range)}}))
(s/fdef add-estate-payment
	:args (s/cat :eid :estate/_id :amount :fiber/amount :year :fiber/year))

;;-----------------------------------------------------------------------------
;; member

(defn memberid-exists?
	[mid]
	(some? (mc-find-one-as-map "memberid-exists?" fiberdb mid)))
(s/fdef memberid-exists?
	:args :member/_id
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
(s/fdef add-memberdc
	:args (s/cat :mid :member/_id :dc :member/dc-entry))

(defn delete-memberdc
	[mid idx]
	(let [dcs     (mc-find-map-by-id "delete-memberdc" members mid ["dcs"])
		  new-dcs (utils/drop-nth idx dcs)]
		(mc-update-by-id "delete-memberdc" members mid
			{$set {:cs new-dcs}})))
(s/fdef delete-memberdc
	:args (s/cat :mid :member/_id :idx integer?))

(defn update-member
	[member]
	(mc-update-by-id members (:_id member)
		{$set member}))
(s/fdef update-member
	:args :fiber/member)

(defn- upd-member
	[m]
	(-> m
		(update-in [:contacts :preferred :type] keyword)
		(update-in [:contacts :other] #(mapv (fn [c] (update c :type keyword)) %))
		(update :dcs #(mapv (fn [dc] {:date    (:date dc)
									  :amount  (bigdec (:amount dc))
									  :tax     (bigdec (:tax dc))
									  :dc-type (keyword (:dc-type dc))
									  :year    (:year dc)}) %))))

(defn- upd-members
	[ms]
	(map upd-member ms))

(defn get-member
	[id]
	(upd-member (mc-find-one-as-map "get-member" members {:_id id})))
(s/fdef get-member
	:args :member/_id
	:ret  :fiber/member)

(defn get-members
	([]
	(upd-members (mc-find-maps "get-members" members {})))
	([year]
	(upd-members (mc-find-maps "get-members" members
		{$and [{:from-to.from {$lte (t/date-time year 12 31)}}
		       {$or [{:from-to.to nil}
		             {:from-to.to {$gte (t/date-time year 1 1)}}]}]}))))
(s/fdef get-members
	:args (s/? :fiber/year)
	:ret  (s/* :fiber/member))

(defn get-current-members
	[]
	(upd-members (mc-find-maps "get-members" members
		{$and [{:from-to.from {$lte (l/local-now)}}
			   {:from-to.to nil}]})))

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
