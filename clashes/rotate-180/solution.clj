(ns Solution
  (:gen-class))

(defn trans [c]
  (case c
    \b \q
    \q \b
    \u \n
    \n \u
    \p \d
    \d \p
    c))

(defn -main [& args]
  (let [input (read-line)
        ans (apply str (reverse (map trans input)))]
    (println ans)))