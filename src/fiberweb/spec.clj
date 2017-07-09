(ns fiberweb.spec
	(:require (fiberweb [config      	:as config])
	          (clojure 	[spec         :as s]
			  			[set   		:as set]
			  			[string       :as str])
			  (clj-time [core        :as t]
			  			[format      :as f]
			  			[local       :as l])
			  (taoensso [timbre      :as timbre])))

;;------------------------------------------------------------------------------------

(s/def :fiber/valid-string   (s/and string? seq))
(s/def :fiber/valid-year     (s/double-in config/min-year (inc config/max-year)))
(s/def :fiber/valid-month    (s/int-in 1 13))
(s/def :fiber/id             (s/and int? pos?))
(s/def :fiber/date           (s/and string? #(f/parse (f/formatters :date) %)))
(s/def :fiber/amount         decimal?)
(s/def :fiber/tax            decimal?)
(s/def :fiber/fromyear       :fiber/valid-year)
(s/def :fiber/toyear         (s/nilable :fiber/valid-year))
(s/def :fiber/name           :fiber/valid-string)
(s/def :fiber/note           string?)
(s/def :fiber/year           :fiber/valid-year)
(s/def :fiber/months         (s/int-in 0 4095))
(s/def :fiber/memberid       :fiber/id)
(s/def :fiber/memberdcid     :fiber/id)
(s/def :fiber/estateid       :fiber/id)
(s/def :fiber/estatedcid     :fiber/id)

;;-----------------------------------------------------------------------------
;; contact
;;-----------------------------------------------------------------------------

(s/def :fiber/preferred     boolean?)
(s/def :addr/value          string?)
(s/def :addr/type           #{:address})
(s/def :fiber/addr-entry    (s/keys :req-un [:addr/type :addr/value :fiber/preferred]))
(s/def :email/value         :fiber/valid-string)
(s/def :email/type          #{:email})
(s/def :fiber/email-entry   (s/keys :req-un [:email/type :email/value :fiber/preferred]))
(s/def :phone/value    	    :fiber/valid-string)
(s/def :phone/type     	    #{:phone})
(s/def :fiber/phone-entry 	(s/keys :req-un [:phone/type :phone/value :fiber/preferred]))
(s/def :fiber/contact       (s/or :addr  :fiber/addr-entry
							 	  :email :fiber/email-entry
							 	  :phone :fiber/phone-entry))

;;-----------------------------------------------------------------------------
;; member-estates
;;-----------------------------------------------------------------------------

(s/def :fiber/me-entry       (s/keys :req-un [:fiber/fromyear :fiber/toyear]))
(s/def :fiber/member-estates (s/map-of :fiber/memberid :fiber/me-entry))

;;-----------------------------------------------------------------------------
;; member-dc
;;-----------------------------------------------------------------------------

(s/def :fiber/member-dc-type  #{:membership-fee :payment})
(s/def :fiber/member-dc-entry (s/keys :req-un [:fiber/memberid
											   :fiber/memberdcid
											   :fiber/date
											   :fiber/amount
											   :fiber/tax
											   :fiber/type
											   :fiber/year]))

;;-----------------------------------------------------------------------------
;; estate-dc
;;-----------------------------------------------------------------------------

(s/def :fiber/estate-dc-type  #{:connection-fee :operator-fee :payment :entry-fee})
(s/def :fiber/estate-dc-entry (s/keys :req-un [:fiber/estateid
											   :fiber/estatedcid
											   :fiber/date
											   :fiber/amount
											   :fiber/tax
											   :fiber/type
											   :fiber/year
											   :fiber/months]))

;;-----------------------------------------------------------------------------
;; member
;;-----------------------------------------------------------------------------

(s/def :fiber/member   (s/keys :req-un [:fiber/memberid
										:fiber/name
                               	  		:fiber/fromyear
                               	  		:fiber/toyear
                               	  		:fiber/note]))

;;------------------------------------------------------------------------------------
;; billing
;;------------------------------------------------------------------------------------

(s/def :estate/bi-months         (s/or :quarter #(= % 3) :whole #(= % 12)))
(s/def :estate/billing-intervals (s/map-of :fiber/valid-year :estate/bi-months))

;;------------------------------------------------------------------------------------
;; activities
;;------------------------------------------------------------------------------------

(s/def :estate/activities        (s/map-of :fiber/valid-year :fiber/months))

;;------------------------------------------------------------------------------------
;; estate
;;------------------------------------------------------------------------------------

(s/def :estate/address           :fiber/valid-string)
(s/def :estate/location          :fiber/valid-string)

(s/def :fiber/estate (s/keys :req-un [:fiber/estateid
								      :estate/location
								      :estate/address
                        		      :fiber/fromyear :fiber/toyear
                               	  	  :fiber/note]))

;;------------------------------------------------------------------------------------

(s/def :conf/entered        #(f/parse (f/formatters :mysql) %))
(s/def :conf/membership-fee decimal?)
(s/def :conf/membership-tax (s/and decimal? #(>= % 0.0M) #(< % 1.0M)))
(s/def :conf/connection-fee decimal?)
(s/def :conf/connection-tax (s/and decimal? #(>= % 0.0M) #(< % 1.0M)))
(s/def :conf/operator-fee   decimal?)
(s/def :conf/operator-tax   (s/and decimal? #(>= % 0.0M) #(< % 1.0M)))

(s/def :fiber/config (s/keys :req-un [:conf/entered
								  	  :conf/membership-fee
								  	  :conf/membership-tax
								  	  :conf/connection-fee
								  	  :conf/connection-tax
								  	  :conf/operator-fee
								  	  :conf/operator-tax
								  	  :fiber/fromyear
								  	  :fiber/frommonth]))

;;------------------------------------------------------------------------------------
