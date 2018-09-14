(ns big-data.sandbox)

(require '[honeysql.core :as sql]
         '[honeysql.helpers :refer :all :as helpers])

(require '[clojure.java.jdbc :as j])
(require '[gadjett.collections :as gadjett])

(require '[clj-time.core :as t])
(require '[clj-time.format :as f])
(require '[clj-time.coerce :as c])




;; Global settings
(def freqs '(250 500 750 1000 1500 2000 3000 4000 6000 8000))

;;; db connection
(def db {:dbtype "postgresql", :dbname "audyx", :host "localhost", :port "5433", :user "me", :password "me"})

(map :tablename (j/query db ["select tablename from pg_catalog.pg_tables"]))


;;; data

(def tests-count-by-user
  #_=>   (-> (select :patient_id [:%count.1 "tests"])
             #_=>       (from :tests)
             #_=>       (group :patient_id)
             (order-by [[:tests :desc]])
             (limit 10)))


(j/query db (sql/format tests-count-by-user))

(defn patient-tests [patient-id]
  {:select [:*]
   :from [:tests]
   :where  [:= :patient_id patient-id]
   :order [:done_at]})

(f/show-formatters)

(defn display-date [ttt]
  (f/unparse (f/formatter :date) (c/from-date ttt)))

(def the-tests (map
                 (fn [test]
                   (-> test
                       (dissoc :noah_settings)
                       (assoc :done_at_day (display-date (:done_at test)))))
                 (j/query db (sql/format (patient-tests 40546)))))

(first the-tests)

(def tonal-tests (map #(dissoc % :noah_settings)(get (group-by :type_id the-tests) 2)))
(def results (map #(assoc (select-keys % [:done_at :done_at_day :ears :equipped])
                     #_=>              :result (read-string (:result %))) (sort-by :done_at tonal-tests)))

(defn result->freq-threshold [result]
  (as->
    (map #(select-keys % [:val :res]) (vals (:result result))) $
       (gadjett/map-object :res (gadjett/mapify :val $))))

(def thresholds-over-time (map (fn [res]
                                 (-> res
                                     (assoc :summary (result->freq-threshold res))
                                     (dissoc :result)))
                               results))

(def freq 1000)

(defn calculate-gains [thresholds-over-time]
  (->> thresholds-over-time
      (map (fn [x]
             (-> (dissoc x :summary)
                 (assoc :threshold (get-in x [:summary freq])))))
      (group-by #(select-keys % [:done_at_day :ears]))
      (filter (fn [[date thresholds]]
                (> (count (set (map :equipped thresholds))) 1)))
      (into {})))


(defn gain-estimate [kkk]
  (let [threshold-equipped (:threshold (first (sort-by :threshold (filter :equipped kkk))))
        threshold-non-equipped (:threshold (first (sort-by :threshold (remove :equipped kkk))))]
    (- threshold-non-equipped threshold-equipped)))

(gadjett/map-object gain-estimate (calculate-gains thresholds-over-time))