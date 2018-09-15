(ns big-data.sandbox)

(require '[honeysql.core :as sql]
         '[honeysql.helpers :refer :all :as helpers])

(require '[clojure.java.jdbc :as j])
(require '[gadjett.collections :as gadjett])
;(require '[gadjett.core :refer [dbg]])

(require '[clojure.pprint :refer [pprint]])
(require '[clj-time.core :as t])
(require '[clj-time.format :as f])
(require '[clj-time.coerce :as c])




;; Global settings

;;; db connection
(def db {:dbtype "postgresql", :dbname "audyx", :host "localhost", :port "5433", :user "me", :password "me"})

(map :tablename (j/query db ["select tablename from pg_catalog.pg_tables"]))

;;; utils


(defmacro dbg3 [x]
  (let [y (gensym "yyy")]
    `(let [x# ~x]
       (def ~y x#)
       (println (str "(" '~y ")  " '~x  " => " (with-out-str (pprint x#))))
       x#)))


(defn diff-in-months [a b]
  (try
    (t/in-months (t/interval a b))
    (catch Exception e
           (println a b)
           (throw e))))
;;; data

(def tonal-type-id 2)

(def tests-count-by-user
  (-> (select :patient_id [:%count.1 "tests"])
      (from :tests)
      (where [:= :type_id tonal-type-id])
      (group :patient_id)
      (order-by [[:tests :desc]])
      (limit 10)))


(j/query db (sql/format tests-count-by-user))

(defn patient-tests-query [patient-id]
  {:select [:*]
   :from [:tests]
   :where  [:= :patient_id patient-id]
   :order [:done_at]})

;(f/show-formatters)

(defn display-date [ttt]
  (f/unparse (f/formatter :date) (c/from-date ttt)))

(defn patient-tests [patient-id]
  (map
    (fn [test]
      (-> test
          (dissoc :noah_settings)
          (assoc :done_at_day (display-date (:done_at test)))))
    (j/query db (sql/format (patient-tests-query patient-id)))))


(defn tonal-tests [patient-id]
  (map #(dissoc % :noah_settings) (patient-tests patient-id)))

(defn first-equipped-date [tests]
  (:done_at (first (filter :equipped tests))))


(defn tests->results [tests]
  (map #(assoc (select-keys % [:done_at :done_at_day :ears :equipped])
          :result (read-string (:result %))) (sort-by :done_at tests)))

(defn result->freq-threshold [result]
  (as->
    (map #(select-keys % [:val :res]) (vals (:result result))) $
       (gadjett/map-object :res (gadjett/mapify :val $))))

(defn thresholds-over-time [results]
  (map (fn [res]
         (-> res
             (assoc :summary (result->freq-threshold res))
             (dissoc :result)))
       results))

(def freq 1000)

(defn calculate-gains [thresholds-over-time ear freq]
  (->> thresholds-over-time
       (filter #(= (:ears %) ear))
       (map (fn [x]
             (-> (dissoc x :summary)
                 (assoc :threshold (get-in x [:summary freq] "n/a")))))
       (group-by #(select-keys % [:done_at_day :ears]))
       (filter (fn [[_ thresholds]]
                (> (count (set (map :equipped thresholds))) 1)))
       (into {})))


(defn gain-and-loss [kkk]
  (let [kkk (remove #(= (:threshold %) "n/a") kkk)
        threshold-equipped (:threshold (first (sort-by :threshold (filter :equipped kkk))))
        threshold-non-equipped (:threshold (first (sort-by :threshold (remove :equipped kkk))))]
    (if (or (nil? threshold-non-equipped)
            (nil? threshold-equipped))
      {:gain "n/a"
       :loss "n/a"
       :distance-from-target "n/a"}
      (let [gain (- threshold-non-equipped threshold-equipped)
            target (* 0.5 threshold-non-equipped)]
        {:gain                 gain
         :loss                 threshold-non-equipped
         :distance-from-target (- target gain)}))))

(defn calculate-gain-freq [equipped-date thresholds-over-time ear freq]
  (->> (calculate-gains thresholds-over-time ear freq)
       (map (fn [[k v]] (merge (assoc k :months-since-equipped (diff-in-months (c/from-date equipped-date) (c/from-string (:done_at_day k))))
                               (gain-and-loss v))))
       (sort-by :age-since-equipped)))

(defn calculate-gains-all-freqs [equipped-date thresholds-over-time]
  (let [freqs '(250 500 750 1000 1500 2000 3000 4000 6000 8000)]
    (for [ear ["L" "R"]]
      [ear
       (for [freq freqs]
         (let [data (calculate-gain-freq equipped-date thresholds-over-time ear freq)]
           [freq
            (map #(gadjett/select-vals % [:months-since-equipped :distance-from-target]) data)
            #_data]))])))

(defn tests-after [date tests]
  (filter #(t/after? (c/from-date (:done_at %)) (c/from-date date)) tests))


(defn patient-history [patient-id]
  (let [all-tests (tonal-tests patient-id)
        start-date (first-equipped-date all-tests)
        tests (tests-after start-date all-tests)
        results (tests->results tests)
        thresholds (thresholds-over-time results)]
    (calculate-gains-all-freqs start-date thresholds)))


(patient-history 3164)