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
(defn get-rands [index] (distinct (take 4 (repeatedly #(rand-int *sector-count*)))))
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
  (let [sectors (sort (nth @*sectors* (:in-sector @player)))]
    (concat (map get-menu-choice-from-sector sectors)
            [["b" "Drop Bomb!"]
             ["q" "Quit."]])))

(defn print-menu [player]
  (let [menu (get-menu player)]
    (write "Hello, " (color/inject :blue true)  (:name @player) (color/reset) ".")
    (write "You're in sector " (:in-sector @player) ".")
    (if (some #{(:in-sector @player)} @*bombs*)
      (write "**This sector has a bomb in it!**"))
    (doseq [[choice text] menu]
        (write "(" choice ") - " text))
    (print "> ")(flush)))

(defn execute-choice [choice player]
  (let [menu (get-menu player)]
    (cond (= "b" choice)
            (dosync (commute *bombs* conj (:in-sector @player)))
          (= "q" choice)
            (dosync (dosync (alter player assoc :keep-playing false)))
          (some #{(str choice)} (map #(str (first %)) menu))
            (dosync (alter player assoc :in-sector (Integer. choice)))
          :else (write "Invailid choice."))))

(defn new-player [name]
  (ref {:name name
        :in-sector (rand-int *sector-count*)
        :keep-playing true}))

(defn remove-player [players player]
  (filter #(= @player %) players))

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
