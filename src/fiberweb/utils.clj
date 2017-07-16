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
            	(clojure 	[string     :as str]
            				[set        :as set]
            				[spec       :as s])))

;;-----------------------------------------------------------------------------

(defn spy
	([x]
	(prn "spy:" (type x) x)
	x)
	([t x]
	(prn "spy:" t x)
	x))

(defn drop-nth [n coll]
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

(defn now-str
    []
    (f/unparse (f/with-zone (f/formatters :mysql) (t/default-time-zone)) (l/local-now)))

(defn today-str
    []
    (f/unparse (f/with-zone (f/formatters :date) (t/default-time-zone)) (l/local-now)))

(defn year-month
    [dt]
    (f/unparse (f/with-zone (f/formatters :year-month) (t/default-time-zone)) dt))

(defn same-submap?
	[m1 m2]
	(let [m1-ks (set (keys m1))
		  m2-ks (set (keys m2))
		  inter (set/intersection m1-ks m2-ks)
		  m1i   (apply dissoc m1 (set/difference m1-ks inter))
		  m2i   (apply dissoc m2 (set/difference m2-ks inter))]
		(and (= m1i m2i) (seq m1i))))

(defn b->n
	[x]
	(when (seq x) x))

(defn mk-tag
	[id idx]
	(keyword (str "tag-" id "-" idx)))

(defn param->bigdec
	[params tag]
	(some->> tag (get params) b->n BigDecimal.))

(defn param->int
	[params tag]
	(some->> tag (get params) b->n Integer/valueOf))

(defn param->double
	[params tag]
	(some->> tag (get params) b->n Double/valueOf))

(defn ts->d
	[ts]
	(f/unparse (f/formatters :date) ts))

;;-----------------------------------------------------------------------------

(defn within
	([ft y]
	(let [whole-year (t/interval (t/date-time y 1 1) (t/date-time y 12 31))
		  year-match (fn [{mid :_id {from :from to :to} :from-to}]
		  				(and (t/within? whole-year from)
		  					 (or (nil? to) (t/within? whole-year to))))]
		(year-match ft)))
	([ft y m d]
	(let [ftomto (t/interval (:from ft) (or (:to ft) (l/local-now)))]
		(t/within fromto (t/date-time y m d)))))
(s/fdef within
	:args (s/alt :year (s/cat :ft :fiber/from-to :y :fiber/year)
				 :ymd  (s/cat :ft :fiber/from-to :y :fiber/year
				 	          :m :fiber/month :d (s/int-in 1 13)))
	:ret boolean?)

(defn all-months?
	[months]
	(= months #{1 2 3 4 5 6 7 8 9 10 11 12}))
(s/fdef all-months?
	:args :fiber/months
	:ret  boolean?)
