(ns clj-wars
  (:use [clojure.contrib server-socket duck-streams]))

(defn item-to-graph-str [in-index in-item]
  (apply str (map #(format "%d -> %s;\n" in-index %) in-item)))
(defn seq-to-graph-str [in-seq]
  (apply str (map item-to-graph-str (iterate inc 0) in-seq)))
(defn get-graph-str [in-seq]
  (apply str ["digraph G {\n" (seq-to-graph-str in-seq) "}\n"]))
(defn write-graph-file [in-seq]
  (doto (new java.io.FileWriter "a.dot") (.write (get-graph-str in-seq)) (.close)))
(defn write-graph-image [in-seq]
  (let [process (new java.lang.ProcessBuilder ["dot" "-Tpng" "a.dot" "-o" "a.png"])]
    (write-graph-file in-seq)
    (.start process)))

(def *sector-count* 25)
(defn get-rands [index] (distinct (take 4 (repeatedly #(rand-int *sector-count*)))))
(defn create-sectors [] (map get-rands (range *sector-count*)))

(def *sectors* (ref (create-sectors)))
(write-graph-image @*sectors*)

(defn write [& in-str]
  (print (apply str in-str))
  (print "\r\n")
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
