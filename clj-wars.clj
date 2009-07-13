(ns clj-wars
  (:use [clojure.contrib server-socket duck-streams]))

(def *debug* *out*)
(defn debug-out [& in-str]
  (.append *debug* (apply str in-str))
  (.append *debug* "\r\n")
  (.flush *debug*))

(load-file "graph.clj")
(load-file "color.clj")

(def *sector-count* 25)
(defn get-rands [index] 
  (filter #(not (== index %))
	  (distinct (take 4 (repeatedly #(rand-int *sector-count*))))))
(defn create-sectors [] (map get-rands (range *sector-count*)))

(def *sectors* (ref (create-sectors)))
(graph/write-image @*sectors*)

(def *players* (ref []))
(def *bombs* (ref []))

(defn write [& in-str]
  (print (apply str in-str))
  (print \return)
  (print \newline)
  (flush))

(defn get-menu-choice-from-sector [sector]
  [sector (apply str "Go to sector " sector ".")])

(defn get-menu [player]
  (let [sectors (sort (nth @*sectors* (:in-sector @player)))
	menu-items (concat (map get-menu-choice-from-sector sectors)
			   [["b" "Drop Bomb!"]
			    ["q" "Quit."]
			    ["s" "See Scoreboard."]])]
    (if (and (@player :num-bombs) (> (@player :num-bombs) 0))
      menu-items
      (filter (fn [[key message]] (not(= key "b"))) menu-items))))

(defn print-menu [player]
  (let [menu (get-menu player)]
    (write "Hello, " (color/inject :blue true)  (:name @player) (color/reset) ".")
    (write "You're in sector " (:in-sector @player) ".")
    (doseq [[choice text] menu]
        (write "(" choice ") - " text))
    (print "> ")(flush)))

(defn bomb-hit? [bomb sector player]
  (and (= (:in-sector bomb) (Integer. sector))
       (not (= (:player bomb) player))))

(defn remove-bombs [bombs sector player]
  (filter #(not (bomb-hit? % sector player)) bombs))

(defn send-to-sector [sector player]
  (doseq [bomb (filter #(bomb-hit? % sector player) @*bombs*)]
    (write (:name @(:player bomb)) " punk'd you with a bomb!")
    (dosync (alter (:player bomb) assoc :score (inc (:score @(:player bomb))))))
  (dosync (alter *bombs* remove-bombs sector player))
  (dosync (alter player assoc :in-sector (Integer. sector)))
  (when (== 1 (rand-int 5))
    (write (color/inject :green true) "You found a new bomb" (color/reset))
    (dosync (alter player assoc :num-bombs (inc (@player :num-bombs))))))


(defn execute-choice [choice player]
  (let [menu (get-menu player)]
    (cond (= "b" choice)
	    (if (and (@player :num-bombs) (> (@player :num-bombs) 0))
	      (do
		(dosync (commute *bombs* conj {:in-sector (:in-sector @player) :player player}))
		(dosync (alter player assoc :num-bombs (dec (@player :num-bombs)))))
	      (do
		(dosync (alter player assoc :num-bombs 0))
		(write (color/inject :red true) "You ain't got no bombs, foo" (color/reset))))
          (= "q" choice)
            (dosync (dosync (alter player assoc :keep-playing false)))
          (= "s" choice)
            (do (write "=== Score ===")
                (doseq [p @*players*] (write (:name @p) ": " (:score @p)))
                (write ""))
          (some #{(str choice)} (map #(str (first %)) menu))
            (send-to-sector choice player)
          :else (write (color/inject :red true) "Invailid choice." (color/reset)))))

(defn new-player [name]
  (ref {:name name
        :score 0
        :in-sector (rand-int *sector-count*)
        :keep-playing true
	:num-bombs 5}))

(defn remove-player [players player]
  (filter #(not(= player %)) players))

(defn client-handler [in out]
  (binding [*in* (reader in)
            *out* (writer out)]
    (write "Enter your name:")
    (print "> ")(flush)
    (let [player (new-player (read-line))]
      (dosync (commute *players* conj player))
      (loop []
        (print-menu player)
        (let [input (read-line)]
          (if input
            (do (execute-choice input player)(flush))
            (dosync (alter player assoc :keep-playing false))))
        (if (:keep-playing @player)
          (recur)
          (dosync (alter *players* remove-player player)))))))

(def port 8866)
(defonce server (create-server port #(client-handler %1 %2)))
