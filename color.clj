(ns color)

(defn get-color-string [color]
  (color {:black  "[30m"
          :red    "[31m"
          :green  "[32m"
          :yellow "[33m"
          :blue   "[34m"
          :purple "[35m"
          :cyan   "[36m"
          :white  "[37m"}))

(defn inject [color strong-flag]
  (apply str [(char 27) (if strong-flag "[1m" "[22m")
              (char 27) (get-color-string color)]))

(defn reset []
  (apply str [(char 27) "[0m"]))