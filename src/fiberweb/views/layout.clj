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
		 :a.link-head:visited (assoc (mk-lnk 8 16 4 2 36 2) :width (u/px 250))]
		[:a.link-head:hover
		 :a.link-head:active {:background-color :green}]
		[:a.link-home:link
		 :a.link-home:visited (assoc (mk-lnk 8 16 4 2 36 2) :width (u/percent 90))]
		[:a.link-home:hover
		 :a.link-home:active {:background-color :green}]
		[:.button (assoc (mk-lnk 8 16 4 2 36 2) :width (u/px 250))]
		[:.button:hover {:background-color :green}]
    		))

(def css-html
	(g/css
		[:html {
			;:background (color/linear-gradient [(color/as-rgb 32) (color/as-rgb 64)])
			:background "linear-gradient(rgb(32, 32, 32), rgb(64, 64, 64))"
			:background-size :contain}]
		[:body {
			:font-family ["HelveticaNeue" "Helvetica Neue" "Helvetica" "Arial" "sans-serif"]
			:font-size line-size
			:color :white
	        ;:-webkit-font-smoothing :antialiased
	        ;:-webkit-text-size-adjust :none
			}]
		[:.app-div {
			:width  full
			:height (u/px 1080)}
			(ss/at-media {:screen true :min-width (u/px 480)}
				[:& {
					:width  (u/px 1000)
					:height (u/px 1080)}])]
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
