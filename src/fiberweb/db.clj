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
            					[set        	:as set])
				[clojure.spec.alpha :as s]))

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
	{:pre [(utils/q-valid? :estate/_id eid)]
	 :post [(utils/q-valid? boolean? %)]}
	(some? (mc-find-one-as-map "estateid-exists?" fiberdb eid)))

(defn estate-count
	[]
	{:post [(utils/q-valid? integer? %)]}
	(mc/count fiberdb estates {}))

(defn add-estate
	[estate]
	{:pre [(utils/q-valid? :fiber/estate estate)]}
	(when (estateid-exists? (:_id estate))
		(throw (Exception. "duplicate estate ID")))
	(mc-insert "add-estate" estates estate))

(defn update-estate
	[estate-part]
	{:pre [(utils/q-valid? :fiber/estate-light estate-part)]}
	(mc-update-by-id "update-estate" estates (:_id estate-part)
		{$set (dissoc estate-part :_id)}))

(defn add-estatedc
	[eid dc]
	{:pre [(utils/q-valid? :estate/_id eid) (utils/q-valid? :estate/dc-entry dc)]}
	(mc-update-by-id "add-estatedc" estates eid
		{$push {:dcs dc}}))

(defn delete-estatedc
	[eid idx]
	{:pre [(utils/q-valid? :estate/_id eid) (utils/q-valid? integer? idx)]}
	(let [dcs     (mc-find-map-by-id "delete-estatedc" estates eid ["dcs"])
		  new-dcs (utils/drop-nth idx dcs)]
		(mc-update-by-id "delete-estatedc" estates eid
			{$set {:cs new-dcs}})))

(defn- upd-estate
	[e]
	(-> e
		;utils/spy
		(update :activities #(mapv (fn [a]
			{:year   (:year a)
			 :months (set (:months a))}) %))
		(update :dcs #(mapv (fn [dc] 
			{:date    (:date dc)
			 :amount  (:amount dc)
			 :tax     (:tax dc)
			 :dc-type (keyword (:dc-type dc))
			 :year    (:year dc)
			 :months  (set (:months dc))}) %))
		(update :billing-intervals #(mapv (fn [bi]
			{:year     (:year bi)
			 :interval (keyword (:interval bi))}) %))
		))

(defn- upd-estates
	[ms]
	(map upd-estate ms))

(defn get-estate
	[id]
	{:pre [(utils/q-valid? :estate/_id id)]
	 :post [(utils/q-valid? :fiber/estate %)]}
	(upd-estate (mc-find-map-by-id "get-estate" estates id)))

(defn get-estates
	([]
	{:post [(utils/q-valid? (s/* :fiber/estate) %)]}
	(upd-estates (mc-find-maps "get-estates" estates {})))
	([memberid]
	{:pre [(utils/q-valid? :member/_id memberid)]
	 :post [(utils/q-valid? (s/* :fiber/estate) %)]}
	(upd-estates (mc-find-maps "get-estates" estates {:owners._id memberid})))
	([memberid year]
	{:pre [(utils/q-valid? :member/_id memberid) (utils/q-valid? :fiber/year year)]
	 :post [(utils/q-valid? (s/* :fiber/estate) %)]}
	(upd-estates (mc-find-maps "get-estates" estates
		{$and [{:owners._id memberid}
			   {:owners.from-to.from {$lte (t/date-time year 12 31)}}
			   {$or [{:owners.from-to.to nil}
			   		 {:owners.from-to.to {$gte (t/date-time year 1 1)}}]}]}))))

(defn get-estates-at
	[year]
	{:pre [(utils/q-valid? :fiber/year year)]
	 :post [(utils/q-valid? (s/* :fiber/estate) %)]}
	(upd-estates (mc-find-maps "get-estates-at" estates
		{$and [{:owners.from-to.from {$lte (t/date-time year 12 31)}}
		       {$or [{:owners.from-to.to nil}
				     {:owners.from-to.to {$gte (t/date-time year 1 1)}}]}]})))

(defn update-activity
	[eid acts]
	{:pre [(utils/q-valid? :estate/_id eid) (utils/q-valid? :estate/activities acts)]}
	(mc-update-by-id "update-activity" estates eid
		{$set {:activities acts}}))

(defn add-estate-payment
	[eid amount year]
	{:pre [(utils/q-valid? :estate/_id eid) (utils/q-valid? :fiber/amount amount) (utils/q-valid? :fiber/year year)]}
	(let [dc {:date    (l/local-now)
			  :amount  amount
		 	  :tax     0.0
			  :dc-type :payment
			  :year    year
			  :months  (set config/month-range)}]
		(s/assert :estate/dc-entry dc)
		(mc-update-by-id "add-estate-payment" estates eid
			{$push {:dcs dc}})))

;;-----------------------------------------------------------------------------
;; member

(defn memberid-exists?
	[mid]
	{:pre [(utils/q-valid? :member/_id mid)]
	 :post [(utils/q-valid? boolean? %)]}
	(some? (mc-find-one-as-map "memberid-exists?" fiberdb mid)))

(defn member-count
	[]
	{:post [(utils/q-valid? integer? %)]}
	(mc/count fiberdb members {}))

(defn add-memberdc
	[mid dc]
	{:pre [(utils/q-valid? :member/_id mid) (utils/q-valid? :member/dc-entry dc)]}
	(mc-update-by-id "add-memberdc" members mid
		{$push {:dcs dc}}))

(defn delete-memberdc
	[mid idx]
	{:pre [(utils/q-valid? :member/_id mid) (utils/q-valid? integer? idx)]}
	(let [dcs     (mc-find-map-by-id "delete-memberdc" members mid ["dcs"])
		  new-dcs (utils/drop-nth idx dcs)]
		(mc-update-by-id "delete-memberdc" members mid
			{$set {:dcs new-dcs}})))

(defn update-member
	[member]
	{:pre [(utils/q-valid? :fiber/member member)]}
	(mc-update-by-id members (:_id member)
		{$set member}))

(defn- upd-member
	[m]
	(-> m
		(update :contacts #(mapv (fn [c] (update c :type keyword)) %))
		(update :dcs #(mapv (fn [dc] (update dc :dc-type keyword)) %))
		))

(defn- upd-members
	[ms]
	(map upd-member ms))

(defn get-member
	[id]
	{:pre [(utils/q-valid? :member/_id id)]
	 :post [(utils/q-valid? :fiber/member %)]}
	(upd-member (mc-find-one-as-map "get-member" members {:_id id})))

(defn get-members
	([]
	{:post [(utils/q-valid? (s/* :fiber/member) %)]}
	(upd-members (mc-find-maps "get-members" members {})))
	([year]
	{:pre [(utils/q-valid? :fiber/year year)]
	 :post [(utils/q-valid? (s/* :fiber/member) %)]}
	(upd-members (mc-find-maps "get-members" members
		{$and [{:from-to.from {$lte (t/date-time year 12 31)}}
		       {$or [{:from-to.to nil}
		             {:from-to.to {$gte (t/date-time year 1 1)}}]}]}))))

(defn get-current-members
	[]
	{:post [(utils/q-valid? (s/* :fiber/member) %)]}
	(upd-members (mc-find-maps "get-current-members" members
		{$and [{:from-to.from {$lte (l/local-now)}}
			   {:from-to.to nil}]})))

(defn add-member
	[member]
	[:pre [(utils/q-valid? :fiber/member member)]]
	(when (memberid-exists? (:_id member))
		(throw (Exception. "duplicate member ID")))
	(mc-insert "add-member" members member))

(defn add-member-payment
	[memberid amount year]
	{:pre [(utils/q-valid? :member/memberid memberid) (utils/q-valid? :fiber/amount amount) (utils/q-valid? :fiber/year year)]}
	(let [dc {:date    (l/local-now)
			  :amount  amount
		 	  :tax     0.0
			  :dc-type :payment
			  :year    year}]
		(s/assert :member/dc-entry dc)
		(mc-update-by-id "add-member-payment" members memberid
			{$push {:dcs dc}})))

;;-----------------------------------------------------------------------------
;; config

(defn add-config
	[config]
	{:pre [(utils/q-valid? :fiber/config config)]}
	(mc-insert "insert-config" configs config))

(defn get-config-at
	[year]
	{:pre [(utils/q-valid? :fiber/year year)]
	 :post [(utils/q-valid? :fiber/config %)]}
	(->> (mc-find-maps "get-config-at" configs
			{:from {$lte (t/date-time year 12 31)}})
		 (sort-by :from)
		 last))

(defn get-configs
	[]
	{:post [(utils/q-valid? (s/* :fiber/config) %)]}
	(mc-find-maps "get-configs" configs {}))

;;------------------------------------------------------------------------------------

; mongoimport -u fiberuser -p "kAllE.kUlA399" --authenticationDatabase fiberdb --db fiberdb --file estates.json --drop --jsonArray
