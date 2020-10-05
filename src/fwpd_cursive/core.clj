(ns fwpd-cursive.core)
(def fileName "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name          identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\r\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn get-glitter-filter-names
  [records]
  (map #(get % :name) records))

(defn append [suspects new-suspect]
  (conj suspects new-suspect))

(defn validate
  [record]
  (and (contains? record :name)
       (contains? record :glitter-index)))

(defn to-csv
  [suspects]
  (reduce #(clojure.string/join % ",") "" suspects))

(defn maping [input]
  (reduce #(into %1 (inc %2)) [] input))

(defn sum ([] (+ 1 1))
  ([x] (+ 1 x)))

(defn two-comp
  [f g]
  (fn [& args]
    (f (apply g args))))

(defn three-comp
  [f g]
  (fn [& args]
    (f
      (apply g args))))