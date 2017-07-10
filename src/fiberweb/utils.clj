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

(defn find-first
	[f coll]
	(some (fn [x] (when (f x) x)) coll))

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

(defn q->b
	[q]
	(nth [2r111111111111 2r111000000000 2r000111000000 2r000000111000 2r000000000111] q))

(defn bit->month
	[b]
	(cond
		(not (zero? (bit-and 2r100000000000 b))) 1
		(not (zero? (bit-and 2r010000000000 b))) 2
		(not (zero? (bit-and 2r001000000000 b))) 3
		(not (zero? (bit-and 2r000100000000 b))) 4
		(not (zero? (bit-and 2r000010000000 b))) 5
		(not (zero? (bit-and 2r000001000000 b))) 6
		(not (zero? (bit-and 2r000000100000 b))) 7
		(not (zero? (bit-and 2r000000010000 b))) 8
		(not (zero? (bit-and 2r000000001000 b))) 9
		(not (zero? (bit-and 2r000000000100 b))) 10
		(not (zero? (bit-and 2r000000000010 b))) 11
		(not (zero? (bit-and 2r000000000001 b))) 12
		:else (throw (ex-info (str "Invalid months: " b) {:cause :invalid-months}))))
(s/fdef bit->month
	:args (s/int-in 0 4096)
	:ret  (s/int-in 1 13))

(defn month->bit
	[m]
	(unsigned-bit-shift-right 2r1000000000000 m))
(s/fdef month->bit
	:args (s/int-in 1 13)
	:ret  integer?)

(defn bits->months
	[bits qmonths]
	;(prn "bits->months:" bits qmonths)
	(for [m (range 1 13)
		:let [mb (month->bit m)]
		:when (not (zero? (bit-and bits mb qmonths)))]
		m))
(s/fdef bits->months
	:args (s/cat :bits integer? :qmonths integer?)
	:ret  (s/* integer?))

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

