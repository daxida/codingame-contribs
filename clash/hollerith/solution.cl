(ns Solution
  (:require [clojure.string :as str])
  (:gen-class))

(defn debug [& msg] (binding [*out* *err*] (println msg) (flush)))

(defn chunkify [n s]
  (if (<= (count s) n)
    [s]
    (conj (chunkify n (subs s n)) (subs s 0 n))))

(defn hollerith [n s]
;; I could't make [chunks (partition-all n s)] work
  (let [chunks (reverse (chunkify n s))
        hollerith-chunks (mapv #(str (count %) "H" %) chunks)
        hollerith-string (str/join "," hollerith-chunks)]
    (debug "Chunks:" chunks)
    (debug "Hollerith Chunks:" hollerith-chunks)
    (debug "Hollerith String:" hollerith-string)
    (str "/" hollerith-string "/")))

(defn -main []
  (let [n (read-string (read-line))
        s (read-line)]
    (debug "Input n:" n)
    (debug "Input s:" s)
    (println (hollerith n s))))
