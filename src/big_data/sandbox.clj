(ns big-data.sandbox)

(require '[honeysql.core :as sql]
         '[honeysql.helpers :refer :all :as helpers])

(require '[clojure.java.jdbc :as j])
(require '[gadjett.collections :as gadjett :refer [map-object]])
(require '[gadjett.core :refer [dbg]])

(require '[clojure.pprint :refer [pprint]])
(require '[clj-time.core :as t])
(require '[clj-time.format :as f])
(require '[clj-time.coerce :as c])

(require '[clojure.set :refer [intersection]])



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

(defn tonal-tests-query [patient-id]
  {:select [:done_at [(sql/call :to_char :done_at "dd/mm/yyyy") "done_at_day"] :result :ears :equipped]
   :from   [:tests]
   :where  [:and [:= :type_id tonal-type-id] [:= :patient_id patient-id]]
   :order  [:done_at]})

(defn tonal-tests [patient-id]
  (j/query db (sql/format (tonal-tests-query patient-id))))

(defn first-equipped-date [tests]
  (:done_at (first (filter :equipped tests))))

(defn convert-result [result]
  (->> (read-string result)
       vals
       (map #(vec (gadjett/select-vals % [:val :res])))
       (into {})))

(defn convert-result-in-test [test]
  (clojure.core/update test :result convert-result))

(defn gain-and-loss [threshold-non-equipped threshold-equipped]
  (let [gain (- threshold-non-equipped threshold-equipped)
        target (* 0.5 threshold-non-equipped)]
    {:gain                 gain
     :loss                 threshold-non-equipped
     :distance-from-target (- target gain)}))

(defn tests-after [date tests]
  (remove #(t/before? (c/from-date (:done_at %))
                      (c/from-date date))
          tests))

(defn months-since-equipped [start-date]
  (map (fn [[k v]] [(assoc k :months-since-equipped (diff-in-months (c/from-date start-date)
                                                                    (c/from-string (:done_at_day k)))) v])))
(defn tests-by-day [tests]
  (->> tests
       (group-by #(select-keys % [:done_at_day :ears]))
       (filter (fn [[_ the-tests]]
                 (> (count (set (map :equipped the-tests))) 1)))
       (into {})))

(defn common-freqs [tests]
  (sort (apply intersection (map #(set (keys (:result %))) tests))))

(defn target-measure [tests]
  (let [tests (->> (group-by :equipped tests)
                   (map-object last))
        tests (map-object convert-result-in-test tests)
        freqs (common-freqs (vals tests))
        {equipped     true
         non-equipped false} tests]
    (for [freq freqs]
      [freq (gain-and-loss (get-in non-equipped [:result freq])
                           (get-in equipped [:result freq]))])))


(defn patient-history [patient-id]
  (let [all-tests (tonal-tests patient-id)
        start-date (first-equipped-date all-tests)
        m-s-e (months-since-equipped start-date)
        tests (tests-after start-date all-tests)
        tests-by-day (tests-by-day tests)]
    (into []
          m-s-e
          (map-object target-measure tests-by-day))))



(patient-history 43510)