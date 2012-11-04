(ns nba-scraper.core
  (:require [net.cgrand.enlive-html :as html]
	    [clojure.string :as st]))

; Base url for ESPN score scraping
(def base-url "http://scores.espn.go.com")

(defn fetch-url [url]
  "Function to get html-resource for all scraping operations"
  (html/html-resource (java.net.URL. url)))

(defn- get-game-nodes [date]
  "Helper function for get-game-ids. base-url must be set"
  (filter #(re-find #"Box" (html/text %)) 
	   (html/select
	     (fetch-url (str base-url "/nba/scoreboard?date=" date)) [:a])))

(defn get-game-ids [date]
  "Gets all boxscore links for a given date. Date format must be yyyymmdd"
  (map #(get-in % [:attrs :href]) (get-game-nodes date)))

(defn get-time-location [url-html]
  "Gets time and location data from boxscore page"
  (let [tl-data (map #(first (:content %)) 
                     (html/select url-html [:div.game-time-location :p]))]
    {:time (first tl-data) :location (last tl-data)}))

(defn get-line-score [url-html]
  "Gets line-score information from boxcore page"
  (let [line-scores (map html/text 
			 (html/select url-html [:table.linescore :tr :td]))
	periods (map keyword 
		  (map st/trim (rest (take-while #(not= % "T") line-scores))))
        ah-scores (rest (partition (+ 2 (count periods)) 
			(map st/trim line-scores)))
	a-scores (zipmap (conj (vec periods) :T) (rest (first ah-scores)))
	h-scores (zipmap (conj (vec periods) :T) (rest (last ah-scores)))]
    {:away a-scores :home h-scores}))

(defn- top-performer-data [node]
  "Helper function for get-top-performers"
  {:player (-> node :content (nth 1) :content first)
   :stats (-> node :content last st/trim)})

(defn get-top-performers [url-html]
  "Get top performer info from boxscore page"
  (let [[away-p home-p] 
	  (map top-performer-data 
	       (rest (html/select url-html [:div.game-notes :p])))]
    {:away away-p :home home-p}))

(defn get-game-data [game]
  "Main scraper function for a boxscore page.
   game must be a link to the boxscore page, relative to base-url"
  (let [url-html (fetch-url (str base-url game))]
    {:time-location (get-time-location url-html)
     :line-score (get-line-score url-html)
     :top-perfomers (get-top-performers url-html)}))
