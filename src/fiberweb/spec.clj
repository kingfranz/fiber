(ns fiberweb.spec
	(:require (fiberweb [config     :as config])
	          (clojure 	[set 		:as set]
			  			[string     :as str])
	          [clojure.spec.alpha :as s]
			  (clj-time [core       :as t]
			  			[format     :as f]
			  			[local      :as l])
			  (taoensso [timbre     :as timbre])))

;;------------------------------------------------------------------------------------

(s/def :fiber/string      (s/and string? seq))
(s/def :fiber/valid-month (s/int-in 1 13))
(s/def :fiber/date        #(instance? org.joda.time.DateTime %))
(s/def :fiber/amount      float?)
(s/def :fiber/tax         float?)
(s/def :fiber/note        string?)
(s/def :fiber/year        (s/int-in config/min-year (inc config/max-year)))
(s/def :fiber/from        #(instance? org.joda.time.DateTime %))
(s/def :fiber/to          (s/nilable #(instance? org.joda.time.DateTime %)))
(s/def :fiber/from-to     (s/keys :req-un [:fiber/from :fiber/to]))

;;-----------------------------------------------------------------------------
;; contact
;;-----------------------------------------------------------------------------

(s/def :addr/value          :fiber/string)
(s/def :addr/type           #{:address})
(s/def :contact/addr-entry  (s/keys :req-un [:addr/type :addr/value :contact/preferred]))
(s/def :email/value         :fiber/string)
(s/def :email/type          #{:email})
(s/def :contact/email-entry (s/keys :req-un [:email/type :email/value :contact/preferred]))
(s/def :phone/value    	    :fiber/string)
(s/def :phone/type     	    #{:phone})
(s/def :contact/phone-entry (s/keys :req-un [:phone/type :phone/value :contact/preferred]))
(s/def :contact/preferred   boolean?)
(s/def :contact/entry       (s/or :addr  :contact/addr-entry
							 	  :email :contact/email-entry
							 	  :phone :contact/phone-entry))
(s/def :member/contacts     (s/* :contact/entry))

;;-----------------------------------------------------------------------------
;; member
;;-----------------------------------------------------------------------------

; "member-37"
(defonce memberid-regex #"member-([0-9])+")
(s/def :member/_id      #(and (string? %) (re-matches memberid-regex %)))

(s/def :member/estate   (s/keys :req-un [:estate/_id :estate/address :fiber/from-to]))
(s/def :member/estates  (s/* :member/estate))

(s/def :member/name     :fiber/string)

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

(s/def :member/feeamount         float?)
(s/def :member/feetax            float?)
(s/def :member/payamount         float?)
(s/def :member/total             float?)

(s/def :fiber/member-sum         (s/keys :req-un [:member/fee-amount
		 										  :member/fee-tax
		 										  :member/pay-amount
		 										  :member/total]))

;;------------------------------------------------------------------------------------
;; estate
;;------------------------------------------------------------------------------------

; "estate-37"
(defonce estateid-regex #"estate-([0-9])+")
(s/def :estate/_id               #(and (string? %) (re-matches estateid-regex %)))

(s/def :estate/months            (s/every :fiber/valid-month :kind set?))

(s/def :estate/dc-type           #{:connection-fee :operator-fee :payment :entry-fee})
(s/def :estate/dc-entry          (s/keys :req-un [:fiber/date
												  :fiber/amount
												  :fiber/tax
												  :estate/dc-type
												  :fiber/year
												  :estate/months]))
(s/def :estate/dcs               (s/* :estate/dc-entry))

(s/def :estate/interval          #{:quarterly :yearly})
(s/def :estate/billing-interval  (s/keys :req-un [:fiber/year :estate/interval]))
(s/def :estate/billing-intervals (s/* :estate/billing-interval))

(s/def :estate/activities        (s/* (s/keys :req-un [:fiber/year :estate/months])))

(s/def :estate/address           :fiber/string)
(s/def :estate/location          :fiber/string)

(s/def :estate/owners            (s/* (s/keys :req-un [:member/_id :member/name :fiber/from-to])))

(s/def :fiber/estate             (s/keys :req-un [:estate/_id
											      :estate/location
											      :estate/address
			                        		      :fiber/from-to
			                        		      :estate/activities
			                        		      :estate/billing-intervals
			                        		      :estate/dcs
			                        		      :estate/owners
			                               	  	  :fiber/note]))

(s/def :fiber/estate-light       (s/keys :req-un [:estate/_id
											      :estate/location
											      :estate/address
			                        		      :fiber/from-to
			                        		      :estate/billing-intervals
			                        		      :fiber/note]))

(s/def :estate/con-amount        float?)
(s/def :estate/con-tax           float?)
(s/def :estate/op-amount         float?)
(s/def :estate/op-tax            float?)
(s/def :estate/pay-amount        float?)
(s/def :estate/total             float?)

(s/def :fiber/estate-sum         (s/keys :req-un [:estate/con-amount
		 										  :estate/con-tax
		 										  :estate/op-amount
		 										  :estate/op-tax
		 										  :estate/pay-amount
		 										  :estate/total]))

;;------------------------------------------------------------------------------------
;; config
;;------------------------------------------------------------------------------------

(s/def :conf/_id           :fiber/date)
(s/def :conf/membership-fee float?)
(s/def :conf/membership-tax float?)
(s/def :conf/connection-fee float?)
(s/def :conf/connection-tax float?)
(s/def :conf/operator-fee   float?)
(s/def :conf/operator-tax   float?)
(s/def :conf/entry-fee      float?)
(s/def :conf/entry-tax      float?)

(s/def :fiber/config (s/keys :req-un [:conf/_id
								  	  :conf/membership-fee
								  	  :conf/membership-tax
								  	  :conf/connection-fee
								  	  :conf/connection-tax
								  	  :conf/operator-fee
								  	  :conf/operator-tax
								  	  :conf/entry-fee
								  	  :conf/entry-tax
								  	  :fiber/from]))

;;------------------------------------------------------------------------------------
