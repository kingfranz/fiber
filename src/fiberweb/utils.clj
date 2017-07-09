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

(defn ym->f
	[y m]
	(when (some? y)
		(+ (Double/valueOf y) (/ (Double/valueOf m) 13.0))))

(defn ymd->f
	[y m]
	(when (some? y)
		(+ y (/ m 13.0))))

(defn current-year
	[]
	(t/year (l/local-now)))

(defn current-month
	[]
	(t/month (l/local-now)))

(defn current-quarter
	[]
	(nth [0 1 1 1 2 2 2 3 3 3 4 4 4] (current-month)))

(defn get-year
	[ym]
	(if (nil? ym)
		(current-year)
		(int ym)))

(defn get-month
	[ym]
	(if (nil? ym)
		(current-month)
		(int (* (- ym (int ym)) 13))))

(defn now-str
    []
    (f/unparse (f/with-zone (f/formatters :mysql) (t/default-time-zone)) (l/local-now)))

(defn today-str
    []
    (f/unparse (f/with-zone (f/formatters :date) (t/default-time-zone)) (l/local-now)))

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
	(f/unparse (f/formatters :date) (c/from-sql-time ts)))

;;-----------------------------------------------------------------------------

