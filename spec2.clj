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
(s/def :fiber/valid-month    (s/int-in 1 13))
(s/def :fiber/date           #(instance? org.joda.time.DateTime %))
(s/def :fiber/amount         decimal?)
(s/def :fiber/tax            decimal?)
(s/def :fiber/note           string?)
(s/def :fiber/year           (s/int-in config/min-year (inc config/max-year)))
(s/def :fiber/fromyear       #(instance? org.joda.time.DateTime %))
(s/def :fiber/toyear         (s/nilable #(instance? org.joda.time.DateTime %)))

;;-----------------------------------------------------------------------------
;; contact
;;-----------------------------------------------------------------------------

(s/def :addr/value          :fiber/valid-string)
(s/def :addr/type           #{:address})
(s/def :contact/addr-entry  (s/keys :req-un [:addr/type :addr/value]))
(s/def :email/value         :fiber/valid-string)
(s/def :email/type          #{:email})
(s/def :contact/email-entry (s/keys :req-un [:email/type :email/value]))
(s/def :phone/value    	    :fiber/valid-string)
(s/def :phone/type     	    #{:phone})
(s/def :contact/phone-entry (s/keys :req-un [:phone/type :phone/value]))
(s/def :contact/entry       (s/or :addr  :contact/addr-entry
							 	  :email :contact/email-entry
							 	  :phone :contact/phone-entry))
(s/def :contact/contacts    (s/* :contact/entry))
(s/def :contact/preferred   :contact/entry)
(s/def :member/contacts     (s/keys :req-un [:contact/preferred :contact/contacts]))

;;-----------------------------------------------------------------------------
;; member
;;-----------------------------------------------------------------------------

; "member-37"
(defonce memberid-regex #"member-([0-9])+")
(s/def :member/_id      #(and (string? %) (re-matches memberid-regex %)))

(s/def :fiber/from-to   (s/keys :req-un [:fiber/fromyear :fiber/toyear]))

(s/def :member/estates  (s/* (s/map-of :estate/_id :fiber/from-to)))

(s/def :member/name     :fiber/valid-string)

(s/def :member/dc-type  #{:membership-fee :payment})
(s/def :member/dc-entry (s/keys :req-un [:fiber/date
										 :fiber/amount
										 :fiber/tax
										 :member/dc-type
										 :fiber/year]))
(s/def :member/dcs      (s/* :member/dc-entry))

(s/def :fiber/member    (s/keys :req-un [:member/_id
										 :member/name
                               	  		 :fiber/from-to
                               	  		 :member/contacts
                               	  		 :member/estates
                               	  		 :member/dcs
                               	  		 :fiber/note]))

;;------------------------------------------------------------------------------------
;; estate
;;------------------------------------------------------------------------------------

; "estate-37"
(defonce estateid-regex #"estate-([0-9])+")
(s/def :estate/_id    #(and (string? %) (re-matches estateid-regex %)))

(s/def :estate/months    (s/every :fiber/valid-month :kind set?))

(s/def :estate/dc-type  #{:connection-fee :operator-fee :payment :entry-fee})
(s/def :estate/dc-entry (s/keys :req-un [:fiber/date
										 :fiber/amount
										 :fiber/tax
										 :estate/dc-type
										 :fiber/year
										 :estate/months]))
(s/def :estate/dcs (s/* :estate/dc-entry))

(s/def :estate/bi-months         #{:quarterly :yearly})
(s/def :estate/billing-interval  (s/map-of :fiber/year :estate/bi-months))
(s/def :estate/billing-intervals (s/* :estate/billing-interval))

(s/def :estate/activity        (s/map-of :fiber/year :estate/months))
(s/def :estate/activities      (s/* :estate/activity))

(s/def :estate/address           :fiber/valid-string)
(s/def :estate/location          :fiber/valid-string)

(s/def :estate/owner           (s/keys :req-un [:member/_id :fiber/from-to]))
(s/def :estate/owners          (s/* :estate/owner))

(s/def :fiber/estate (s/keys :req-un [:estate/_id
								      :estate/location
								      :estate/address
                        		      :fiber/from-to
                        		      :estate/activities
                        		      :estate/billing-intervals
                        		      :estate/dcs
                        		      :estate/owners
                               	  	  :fiber/note]))

;;------------------------------------------------------------------------------------
;; config
;;------------------------------------------------------------------------------------

(s/def :conf/entered        :fiber/date)
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
								  	  :fiber/fromyear]))

;;------------------------------------------------------------------------------------
