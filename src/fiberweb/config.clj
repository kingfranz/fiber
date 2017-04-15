(ns fiberweb.config
	(:require [clojure.set :as set]))

(def window-width 1200)
(def window-height 800)

(def min-year (long 2010))
(def max-year (long 2030))
(def year-range (range min-year (inc max-year)))
(def month-range (range 1 13))
(def every-month #{1 2 3 4 5 6 7 8 9 10 11 12})

