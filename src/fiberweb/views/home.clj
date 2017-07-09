(ns fiberweb.views.home
  	(:require 	(fiberweb 	[db         :as db])
  				(fiberweb.views [layout     :as layout]
            					[common     :as common])
 	          	(garden 	[core       :as g]
            				[units      :as u]
            				[selectors  :as sel]
            				[stylesheet :as stylesheet]
            				[color      :as color])
             	(clj-time 	[core       :as t]
            				[local      :as l]
            				[format     :as f]
            				[periodic   :as p])
            	(hiccup 	[core       :as h]
            				[def        :as hd]
            				[element    :as he]
            				[form       :as hf]
            				[page       :as hp]
            				[util       :as hu])
            	(clojure 	[string     :as str]
            				[set        :as set])))

;;-----------------------------------------------------------------------------

(def indentation (u/px 36))

(def css-home-tree
	(g/css
		[:.tree
		 ".tree ul" {
			:margin [[0 0 0 indentation]] ;/* indentation */
			:padding 0
			:list-style :none
			:color :white
			:position :relative}]
		[".tree ul" {
		 	:margin-left (u/px-div indentation 2)}]
		[:.tree:before
		 ".tree ul:before" {
			:content "\"\""
			:display :block
			:width 0
			:position :absolute
			:top 0
			:bottom 0
			:left 0
			:border-left [[(u/px 1) :solid]]}]
		[".tree li" {
			:margin 0
			:padding [[0 0 0 (u/px+ indentation (u/px 12))]] ;/* indentation + .5em */
			:line-height indentation ;/* default list item's `line-height` */
			:font-weight :bold
			:position :relative}]
		[".tree li:before" {
			:content "\"\""
			:display :block
			:width indentation ;/* same with indentation */
			:height 0
			:border-top [[(u/px 1) :solid]]
			:margin-top (u/px -1) ;/* border top width */
			:position :absolute
			:top (u/px 18) ;/* (line-height/2) */
			:left 0}]
		[".tree li:last-child:before" {
			:height :auto
			:top (u/px-div indentation 2) ;/* (line-height/2) */
			:bottom 0
			:border-left 0}]
		[:.tree-top {:vertical-align :top}]))

(defn home-page
	[]
	(layout/common "Myrhult-Bålerud Fiber förening" [css-home-tree]
		[:p (str "Fiber - Medlemmar: " (db/member-count) " Fastigheter: " (db/estate-count))]
		[:ul.tree
			[:li "System"
				[:ul
					[:li [:a.link-thick {:href "/invoice/membership"} "Skapa saldolista (medlemsavgift)"]]
					[:li [:a.link-thick {:href "/invoice/quarter"} "Skapa saldolista (kvartals)"]]
					[:li [:a.link-thick {:href "/invoice/yearly"} "Skapa saldolista (helår)"]]
					[:li [:a.link-thick {:href "/enter-payments"} "Bokför inbetalningar"]]
					[:li [:a.link-thick {:href "/config"} "Konfigurera"]]
					[:li [:a.link-thick {:href "/list-members"} "Se medlemslistan"]]
					[:li [:a.link-thick {:href "/list-all"} "Se hela listan"]]
					[:li [:a.link-thick {:href "/exit"} "Stäng av webserver"]]]]
			[:li "Medlemmar"
				[:ul
					[:li [:a.link-thick {:href "/new-member"} "Ny medlem"]]
					[:li [:a.link-thick {:href "/choose-member"} "Ändra medlem"]]]]
			[:li "Fastigheter"
				[:ul
					[:li [:a.link-thick {:href "/new-estate"} "Ny fastighet"]]
					[:li [:a.link-thick {:href "/choose-estate"} "Ändra fastighet"]]
					[:li [:a.link-thick {:href "/enter-activities"} "Bokför aktiviteter"]]]]]))
