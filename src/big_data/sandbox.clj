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

(defn hash->id [hash]
  (->
    (- hash 171)
    (/ 317)))

(defn id->hash [id]
  (->
    (* id 317)
    (+ 171)))

(hash->id 366623)
(id->hash 43510)

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

(j/query db ["SELECT * from patients where id = 43510"])
(j/query db ["SELECT * from centers where id = 7"])
(defn tonal-tests-query [patient-id]
  {:select [:done_at [(sql/call :to_char :done_at "yyyy-mm-dd") "done_at_day"] :result :ears :equipped]
   :from   [:tests]
   :where  [:and [:= :type_id tonal-type-id] [:= :patient_id patient-id]]
   :order-by  [[:done_at :asc]]})

(def patient-id 39577)
(defn tonal-tests [patient-id]
  (j/query db (sql/format (tonal-tests-query patient-id))))

(defn convert-result [result]
  (->> (read-string result)
       vals
       (remove #(or (nil? (:res %)) (nil? (:val %))))
       (map #(vec (gadjett/select-vals % [:val :res])))
       (into {})))

(defn convert-result-in-test [test]
  (clojure.core/update test :result convert-result))

(defn gain-and-loss [threshold-non-equipped threshold-equipped]
  (when-not (or (nil? threshold-non-equipped)
          (nil? threshold-equipped))
    (let [gain (- threshold-non-equipped threshold-equipped)
          target (* 0.5 threshold-non-equipped)]
      {:gain                 gain
       :loss                 threshold-non-equipped
       :distance-from-target (- target gain)})))

(defn target-measure [non-equipped equipped]
  (for [freq (keys (:result equipped))]
    [freq (gain-and-loss (get-in non-equipped [:result freq])
                         (get-in equipped [:result freq]))]))


(defn calc-gain-vs-target [audiogram equipped-start {:keys [done_at] :as equipped-test}]
  [{:date                  done_at
    :months-since-equipped (diff-in-months (c/from-date equipped-start)
                                           (c/from-date done_at))}
   (target-measure audiogram equipped-test)])

(defn tests->gains [tests]
  (reduce (fn [{:keys [last-audiogram equipped-start] :as history}
               {:keys [equipped] :as test}]
            (if-not equipped
              (assoc history :last-audiogram (convert-result-in-test test))
              (as-> history $
                    (if (nil? equipped-start)
                      (assoc $ :equipped-start (:done_at test))
                      $)
                    (clojure.core/update $
                                         :gain-over-time
                                         #(conj % (calc-gain-vs-target last-audiogram
                                                                       (:equipped-start $)
                                                                       (convert-result-in-test test)))))))
          {:equipped-start nil
           :last-audiogram nil
           :gain-over-time []}
          tests))

(defn patient-gain-history [patient-id]
  (->> (tonal-tests patient-id)
       (group-by :ears)
       (gadjett/map-object tests->gains)))

(def data (patient-gain-history 39577))
;; good patient
;; 39577 -- droneau

(defn data-to-plot [data freq]
  (->> data
       (map (fn [[k v]] [(:months-since-equipped k)
                         (:distance-from-target (second (first (filter #(= freq (first %)) v))))]))))

(data-to-plot (get-in data ["R" :gain-over-time]) 1000)

(memoize)

(defn transparent [arg]
  [(type arg) (meta arg) (when (seq? arg) (seq arg)) arg])


(defn memoize-tr
  "Returns a memoized version of a function (even if it is not referentially
  transparent). The memoized version of the function keeps a cache of the
  mapping from arguments to results and, when calls with the same arguments
  are repeated often, has higher performance at the expense of higher memory use."
  {:added "1.0"
   :static true}
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (let [ref-transparent-args (map transparent args)]
        (if-let [e (find @mem ref-transparent-args)]
          (val e)
          (let [ret (apply f args)]
            (swap! mem assoc ref-transparent-args ret)
            ret))))))

(def mem-conj (memoize conj))

(= (mem-conj [1 2] 3) (conj [1 2] 3))                       ;; true
(= (mem-conj '(1 2) 3) (conj '(1 2) 3))                     ;; false

(def mem-tr-conj (memoize-tr conj))

(= (mem-tr-conj [1 2] 3) (conj [1 2] 3))                    ;;true
(= (mem-tr-conj '(1 2) 3) (conj '(1 2) 3))                  ;; true