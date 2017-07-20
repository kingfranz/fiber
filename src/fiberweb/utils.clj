(ns fiberweb.utils
  	(:require 	(garden 	[core       :as g]
            				[units      :as u]
            				[selectors  :as sel]
            				[stylesheet :as ss]
            				[color      :as color])
             	(clj-time 	[core       :as t]
            				[local      :as l]
            				[coerce     :as c]
            				[format     :as f]
            				[periodic   :as p])
            	(hiccup 	[core       :as h]
            				[def        :as hd]
            				[element    :as he]
            				[form       :as hf]
            				[page       :as hp]
            				[util       :as hu])
            	(taoensso	[timbre    	:as log])
            	(clojure 	[string     :as str]
            				[set        :as set])
            	(fiberweb 	[spec 		:as spec]
            				[config     :as config])
            	[clojure.spec.alpha :as s]))

;;-----------------------------------------------------------------------------

(defmacro q-valid?
	[sp v]
	`(if-not (s/valid? ~sp ~v)
		(log/trace
			(str "\n---------- " ~*file* " " ~(:line (meta &form)) " ------------\n"
				 ~v
				 "\n---------------------------------------\n"
				 (s/explain-str ~sp ~v)
				 "\n---------------------------------------\n"))
		true))

(defn spy
	([x]
	(prn "spy:" (type x) x)
	x)
	([t x]
	(prn "spy:" t x)
	x))

(defn drop-nth
	[n coll]
	{:pre [(pos? n) (coll? coll)]
	 :post [(coll? %)]}
	(vec (keep-indexed #(when (not= %1 n) %2) coll)))

(defn find-first
	[f coll]
	(some (fn [x] (when (f x) x)) coll))

(defn current-year
	[]
	(t/year (l/local-now)))

(defn current-month
	[]
	(t/month (l/local-now)))

(defn current-quarter
	[]
	(nth [0 1 1 1 2 2 2 3 3 3 4 4 4] (current-month)))

(defn curr-year-range
	[]
	(range (current-year) (inc config/max-year)))

(defn now-str
    []
    (f/unparse (f/with-zone (f/formatters :mysql) (t/default-time-zone)) (l/local-now)))

(defn today-str
    []
    (f/unparse (f/with-zone (f/formatters :date) (t/default-time-zone)) (l/local-now)))

(defn year-month
    [dt]
    (f/unparse (f/with-zone (f/formatters :year-month) (t/default-time-zone)) dt))

(defn b->n
	[x]
	(when (seq x) x))

(defn mk-tag
	[id idx]
	(keyword (str "tag-" id "-" idx)))

(defn param->int
	[params tag]
	{:pre [(q-valid? map? params) (q-valid? keyword? tag)]
	 :post [(q-valid? (s/nilable integer?) %)]}
	(some->> tag (get params) b->n Integer/valueOf))

(defn param->double
	[params tag]
	{:pre [(q-valid? map? params) (q-valid? keyword? tag)]
	 :post [(q-valid? (s/nilable float?) %)]}
	(some->> tag (get params) b->n Double/valueOf))

(defn ts->d
	[ts]
	{:pre [(q-valid? #(instance? org.joda.time.DateTime %) ts)]
	 :post [(q-valid? string? %)]}
	(f/unparse (f/formatters :date) ts))

(defn get-year
	[year mcoll]
	{:pre [(q-valid? :fiber/year year) (q-valid? (s/* map?) mcoll)]
	 :post [(q-valid? map? %)]}
	(if (nil? (find-first #(= (:year %) year) mcoll))
		(prn "get-year:" year mcoll)
		(find-first #(= (:year %) year) mcoll)))

;;-----------------------------------------------------------------------------

(defn within
	([ft y]
	{:pre [(q-valid? :fiber/from-to ft) (q-valid? :fiber/year y)]
	 :post [(q-valid? boolean? %)]}
	(let [whole-year (t/interval (t/date-time y 1 1) (t/date-time y 12 31))
		  year-match (fn [{mid :_id {from :from to :to} :from-to}]
		  				(and (t/within? whole-year from)
		  					 (or (nil? to) (t/within? whole-year to))))]
		(year-match ft)))
	([ft y m d]
	{:pre [(q-valid? :fiber/from-to ft)
		   (q-valid? :fiber/year y)
		   (q-valid? :fiber/valid-month m)
		   (q-valid? (s/int-in 1 32) d)]
	 :post [(q-valid? boolean? %)]}
	(let [fromto (t/interval (:from ft) (or (:to ft) (l/local-now)))]
		(t/within? fromto (t/date-time y m d)))))

(defn all-months?
	[months]
	{:pre [(q-valid? :estate/months months)]
	 :post [(q-valid? boolean? %)]}
	(= months #{1 2 3 4 5 6 7 8 9 10 11 12}))
