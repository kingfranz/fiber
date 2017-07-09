(ns fiberweb.views.layout
  	(:require 	[hiccup.core 	   :as hc]
          		[hiccup.page 	   :as hp]
          		[garden.core       :as g]
            	[garden.units      :as u]
            	[garden.selectors  :as sel]
            	[garden.stylesheet :as ss]
            	[garden.color      :as color]
            	))

(def line-size (u/px 24))
(def transparent (color/rgba 200 200 200 0))
(def full (u/percent 100))
(def half (u/percent 50))

(defn grey% [p] (color/as-rgb (* (/ p 100) 255)))

(defn mk-lnk
	[padd-v padd-h margin-v margin-h fnt-sz border-sz]
	{
	:background-color     (grey% 30)
 	:color                :white
	:padding              [[(u/px padd-v) (u/px padd-h)]]
	:margin               [[(u/px margin-v) (u/px margin-h)]]
    :text-align           :center
	:text-decoration      :none
	:font-weight          :bold
	:font-size            (u/px fnt-sz)
	:border               [[(u/px border-sz) :white :solid]]
	:border-radius        (u/px 8)
	:display              :inline-block
	:cursor               :pointer})

(def css-misc
	(g/css
		[:a.link-thin:link
		 :a.link-thin:visited (mk-lnk 0 6 0 0 24 1)]
		[:a.link-thin:hover
		 :a.link-thin:active {:background-color :green}]
		[:a.link-thick:link
		 :a.link-thick:visited (mk-lnk 0 6 6 0 24 1)]
		[:a.link-thick:hover
		 :a.link-thick:active {:background-color :green}]
		[:a.link-head:link
		 :a.link-head:visited (mk-lnk 8 16 4 2 24 2)]
		[:a.link-head:hover
		 :a.link-head:active {:background-color :green}]
		[:a.link-home:link
		 :a.link-home:visited (mk-lnk 8 16 4 2 24 2)]
		[:a.link-home:hover
		 :a.link-home:active {:background-color :green}]
		[:.button (mk-lnk 8 16 4 2 24 2)]
		[:.button:hover {:background-color :green}]
    	[:input {:font-size line-size :width full}]
    	[:select {:font-size line-size}]
    	[:.cb {:transform "scale(2)"}]
    	[:.rafield {:text-align :right}]
    	[:.tafield {:vertical-align :text-top}]
    	[:.udpad {:padding [[(u/px 8) 0 (u/px 8) 0]]}]
    	[:.rpad {:padding [[0 (u/px 18) 0 0]]}]
		[:.tbl-brdr {:border-left [[(u/px 1) :grey :solid]]
			         :border-top [[(u/px 1) :grey :solid]]
			         :border-right [[(u/px 1) :grey :solid]]}]
		[:.tbl-brdr1 {:border-left [[(u/px 1) :grey :solid]]
			         :border-right [[(u/px 1) :grey :solid]]}]
		[:.tbl-co {:border-collapse :collapse}]
		[:.txtcol {:width (u/px 500)}]
		[:.ncol {:width (u/px 300)}]
		[:.acol {:width (u/px 400)}]
		[:.ccol {:word-wrap :break-word :width (u/px 250)}]
		[:.dcol {:width (u/px 100)}]
		[:.ddcol {:width (u/px 150)}]
		[:.brdr {:border [[(u/px 1) :grey :solid]]}]
		[:.brdrcol {:border-collapse :collapse}]
		))

(def css-html
	(g/css
		[:html {
			;:background (color/linear-gradient [(color/as-rgb 32) (color/as-rgb 64)])
			:background (color/rgb 32 32 32)
			:background-size :contain}]
		[:body {
			:font-family ["HelveticaNeue" "Helvetica Neue" "Helvetica" "Arial" "sans-serif"]
			:font-size line-size
			:color :white
	        ;:-webkit-font-smoothing :antialiased
	        ;:-webkit-text-size-adjust :none
			}]
		[:.app-div {
			:width  full}
			(ss/at-media {:screen true :min-width (u/px 480)})]
		[:.master-table {
			:width full}]))

(defn common
	[title css & body]
	(hp/html5
		[:head {:lang "sv"}
			[:meta {:charset "utf-8"}]
			[:meta {:http-equiv "X-UA-Compatible" :content "IE=edge,chrome=1"}]
			[:meta {:name "viewport" :content "width=device-width, initial-scale=1, maximum-scale=1"}]
			[:title title]
			[:script "function toggleText(cb, t1, t2) {
    			document.getElementById(t1).disabled = !document.getElementById(cb).checked;
    			document.getElementById(t2).disabled = !document.getElementById(cb).checked;
			}"]
			[:style css-html]
			[:style css-misc]
			(map (fn [x] [:style x]) css)
			;(hp/include-css css)
			]
		[:body
			[:div.app-div body]]))

(defn four-oh-four
	[]
  	(common "Page Not Found" "/css/home.css"
            [:div {:id "four-oh-four"} "The page you requested could not be found"]))
