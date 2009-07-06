(ns clj-wars
  (:use [clojure.contrib server-socket duck-streams]))

(load-file "graph.clj")

(def *sector-count* 25)
(defn get-rands [index] (distinct (take 4 (repeatedly #(rand-int *sector-count*)))))
(defn create-sectors [] (map get-rands (range *sector-count*)))

(def *sectors* (ref (create-sectors)))
(graph/write-image @*sectors*)

(defn write [& in-str]
  (print (apply str in-str))
  (print \return)
  (print \newline)
  (flush))

(defn get-menu [player]
  (let [menu (ref [])]
    (dosync
     (doseq [option (sort (nth @*sectors* (:in-sector @player)))]
       (alter menu conj [option (apply str "Go to sector " option ".")])))
    menu))

(defn print-menu [player menu]
  (write "Hello, " (:name @player) ".")
  (write "You're in sector " (:in-sector @player) ".")
  (doseq [option @menu]
    (let [choice (first option)
          text (first (rest option))]
      (write "(" choice ") - " text)))
  (print "> ")
  (flush))

(defn execute-choice [choice player menu]
  (if (some #{(str choice)} (map #(str (first %)) @menu))
    (dosync (alter player assoc :in-sector (Integer. choice)))
    (write "Invailid choice.")))

(defn client-handler [in out]
  (binding [*in* (reader in)
            *out* (writer out)]
    (write "Enter your name:")
    (print "> ")(flush)
    (let [player (ref {:name (read-line)
                       :in-sector (rand-int *sector-count*)})]
      (loop [menu (get-menu player)]
        (print-menu player menu)
        (execute-choice (read-line) player menu)
        (flush)
        (recur (get-menu player))))))

(def port 8866)
(defonce server (create-server port #(client-handler %1 %2)))
