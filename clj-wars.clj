(ns clj-wars
  (:use [clojure.contrib server-socket duck-streams]))

(def *sector-count* 25)
(defn get-rands [index] (distinct (take 4 (repeatedly #(rand-int *sector-count*)))))
(defn create-sectors [] (map get-rands (range *sector-count*)))

(def *sectors* (ref (create-sectors)))
(write-graph-image @*sectors*)

(defn write [& in-str]
  (print (apply str in-str))
  (print "\r\n")
  (flush))

(defn print-menu [player]
  (write "Hello, " (:name @player) ".")
  (write "You're in sector " (:in-sector @player) ".")
  (doseq [exit (sort (nth @*sectors* (:in-sector @player)))]
    (write "(" exit ") - Go to sector " exit "."))
  (print "> ")
  (flush))

(defn execute-choice [choice player]
  (write "Going to sector " choice ".")
  (dosync (alter player assoc :in-sector (Integer. choice))))

(defn client-handler [in out]
  (binding [*in* (reader in)
            *out* (writer out)]
    (write "Enter your name:")
    (print "> ")(flush)
    (let [player (ref {:name (read-line)
                       :in-sector (rand-int 30)})]
      (loop []
        (print-menu player)
        (execute-choice (read-line) player)
        (flush)
        (recur)))))
  
(def port 8866)

;; This noise makes recompiling in the repl restart the server
(declare server)
(if (.isBound #'server) (close-server server))
(def server (create-server port client-handler))
