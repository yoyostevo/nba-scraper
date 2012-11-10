(ns nba-scraper.core
  (:require [net.cgrand.enlive-html :as html]
	    [clojure.string :as st]))

(defn parse-int [s]
  "Helper function to parse integers from strings"
  (Integer. (re-find #"\d+" s)))

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
	a-scores (zipmap (conj (vec periods) :T) 
			 (map parse-int (rest (first ah-scores))))
	h-scores (zipmap (conj (vec periods) :T) 
			 (map parse-int (rest (last ah-scores))))]
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

(defn extract-player-data [row]
  "Helper function for get-player-data"
  (let [[name pos mins fgm-a tpm-a ftm-a oreb dreb reb ast stl blk to pf pm pts]
	(flatten (map :content (html/select row [:td])))]
    (when fgm-a
      {:name (first (:content name)) :pos (st/replace pos #", " "") 
       :mins (parse-int mins) 
       :fgm (parse-int (first (st/split fgm-a #"-")))
       :fga (parse-int (last (st/split fgm-a #"-")))
       :tpm (parse-int (first (st/split tpm-a #"-")))
       :tpa (parse-int (last (st/split tpm-a #"-")))
       :ftm (parse-int (first (st/split ftm-a #"-")))
       :fta (parse-int (last (st/split ftm-a #"-")))
       :oreb (parse-int oreb) :dreb (parse-int dreb) :reb (parse-int reb)
       :ast (parse-int ast) :stl (parse-int stl) :blk (parse-int blk) 
       :to (parse-int to) :pf (parse-int pf) :pm (read-string pm) 
       :pts (parse-int pts)})))
       
(defn get-player-data [url-html]
  "Get player data from boxscore page"
  (let [rows (html/select url-html [:table.mod-data :tr])
	arows (take-while #(nil? (-> % :content first :tag)) (drop 2 rows))
	hrows (drop (+ (count arows) 2)
		    (filter #(nil? (-> % :content first :tag)) rows))]
    {:away (filter #((complement nil?) %) (map extract-player-data arows))
     :home (filter #((complement nil?) %) (map extract-player-data hrows))}))

(defn get-team-names [url-html]
  "Away and home teams"
  (let [title (-> url-html (html/select [:title]) first :content first)]
    {:away (first (st/split title #" vs. "))
     :home (first (st/split (last (st/split title #" vs. ")) #" - "))}))

(defn- team-total-data [row]
  "Helper function for get-team-totals"
  (let [[_ fgm-a tpm-a ftm-a oreb dreb reb ast stl blk to pf _ pts]
	(flatten (map :content 
		      (flatten (map :content (html/select row [:td])))))]
    {:fgm (parse-int (first (st/split fgm-a #"-"))) 
     :fga (parse-int (last (st/split fgm-a #"-")))
     :tpm (parse-int (first (st/split tpm-a #"-")))
     :tpa (parse-int (last (st/split tpm-a #"-")))
     :ftm (parse-int (first (st/split ftm-a #"-")))
     :fta (parse-int (last (st/split ftm-a #"-")))
     :oreb (parse-int oreb) :dreb (parse-int dreb) :reb (parse-int reb) 
     :ast (parse-int ast) :stl (parse-int stl) :blk (parse-int blk) 
     :to (parse-int to) :pf (parse-int pf) :pts (parse-int pts)}))

(defn- team-extra-data [row]
  "Helper function for get-team-totals"
  (let [dat (:content row)]
    {:FastBrkPts (parse-int (second dat))
     :PtsInPnt (parse-int (nth dat 4))
     :TotTeamTOs (parse-int (first (st/split (nth dat 7) #"\(")))
     :PtsOffTOs (parse-int (first (st/split 
			     (second (st/split (nth dat 7) #"\(")) #"\)")))}))

(defn get-team-totals [url-html]
  "Away and home team totals from boxscore page"
  (let [rows (filter #(= "2" (-> % :content first :attrs :colspan))
		     (html/select url-html [:table.mod-data :tr]))
	extras (html/select url-html [:table.mod-data :tr :td :div])]
    {:away (into (team-total-data (first rows)) 
	         (team-extra-data (first extras)))
     :home (into (team-total-data (first (next (next rows))))
		 (team-extra-data (nth extras 2)))}))
 
(defn get-game-extras [url-html]
  "Flagrants, Techs, Refs, Attendance and GameTime from boxscore page"
  (let [items (map st/trim (filter (complement map?) 
		      (:content (last (html/select url-html 
						   [:div.mod-content])))))]
    {:flagrants (first items)
     :techs (nth items 1)
     :officials (nth items 2)
     :attendance (nth items 3)
     :game-length (nth items 4)}))

(defn get-game-data [game]
  "Main scraper function for a boxscore page.
   game must be a link to the boxscore page, relative to base-url"
  (let [url-html (fetch-url (str base-url game))]
    {:teams (get-team-names url-html)
     :time-location (get-time-location url-html)
     :line-score (get-line-score url-html)
     :top-perfomers (get-top-performers url-html)
     :players (get-player-data url-html)
     :team-totals (get-team-totals url-html)
     :game-extras (get-game-extras url-html)}))
