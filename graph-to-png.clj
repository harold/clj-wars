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

(write-graph-image [[1 2 3] [2 3 4] [4] [0 4 5 6] [] [6] [5 0]])