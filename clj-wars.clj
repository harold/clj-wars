(ns clj-wars
  (:use [clojure.contrib server-socket duck-streams]))

(load-file "graph.clj")

(def *sector-count* 25)
(defn get-rands [index] (distinct (take 4 (repeatedly #(rand-int *sector-count*)))))
(defn create-sectors [] (map get-rands (range *sector-count*)))

(def *sectors* (ref (create-sectors)))
(graph/write-image @*sectors*)

(def *players* (ref []))

(defn write [& in-str]
  (print (apply str in-str))
  (print \return)
  (print \newline)
  (flush))

(defn get-menu-choice-from-sector [sector]
  [sector (apply str "Go to sector " sector ".")])

(defn get-menu [player]
  (let [sectors (sort (nth @*sectors* (:in-sector @player)))]
    (map get-menu-choice-from-sector sectors)))

(defn print-menu [player]
  (let [menu (get-menu player)]
    (write "Hello, " (:name @player) ".")
    (write "You're in sector " (:in-sector @player) ".")
    (doseq [option menu]
      (let [choice (first option)
            text (first (rest option))]
        (write "(" choice ") - " text)))
    (print "> ")
    (flush)))

(defn execute-choice [choice player]
  (let [menu (get-menu player)]
    (if (some #{(str choice)} (map #(str (first %)) menu))
      (dosync (alter player assoc :in-sector (Integer. choice)))
      (write "Invailid choice."))))

(defn client-handler [in out]
  (binding [*in* (reader in)
            *out* (writer out)]
    (write "Enter your name:")
    (print "> ")(flush)
    (let [player (ref {:name (read-line)
                       :in-sector (rand-int *sector-count*)})]
      (loop []
        (print-menu player)
        (execute-choice (read-line) player)
        (flush)
        (recur)))))

(def port 8866)
(defonce server (create-server port #(client-handler %1 %2)))
