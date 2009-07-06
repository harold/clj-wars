(ns graph)

(defn item-to-str [in-index in-item]
  (apply str (map #(format "%d -> %s;\n" in-index %) in-item)))

(defn seq-to-str [in-seq]
  (apply str (map item-to-str (iterate inc 0) in-seq)))

(defn get-str [in-seq]
  (apply str ["digraph G {\n" (seq-to-str in-seq) "}\n"]))

(defn write-file [in-seq]
  (doto (new java.io.FileWriter "a.dot") (.write (get-str in-seq)) (.close)))

(defn write-image [in-seq]
  (let [process (new java.lang.ProcessBuilder ["dot" "-Tpng" "a.dot" "-o" "a.png"])]
    (write-file in-seq)
    (.start process)))
