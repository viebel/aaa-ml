(ns big-data.sandbox)

(require '[honeysql.core :as sql]
         '[honeysql.helpers :refer :all :as helpers])

(require '[clojure.java.jdbc :as j])
(require '[gadjett.collections :as gadjett :refer [map-object mean select-vals]])
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
(defn patient-folder-url [id]
  (str "https://beta.audyx.com/#/patientFolder/" (id->hash id) "/patient"))

(hash->id 366623)
(id->hash 39577)

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

(def all-audiograms-q
  (-> (select :patient_id :done_at :result :ears :equipped)
      (from :tests)
      (where [:and [:= :equipped false] [:= :type_id tonal-type-id]])))

(defn all-audiograms []
  (j/query db (sql/format all-audiograms-q)))


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
       :distance-from-target (- gain target)})))

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
       (map-object tests->gains)))

(def data (patient-gain-history 39577))
;; good patient
;; 39577 -- droneau



(defn data-to-plot [data freq]
  (->> data
       (map (fn [[k v]] [(:months-since-equipped k)
                         (:distance-from-target (second (first (filter #(= freq (first %)) v))))]))))

(data-to-plot (get-in data ["R" :gain-over-time]) 1000)



(defn audio-mean [audiogram]
  (let [vals (select-vals audiogram [500 1000 2000 4000])]
    (if (= (count vals) 4)
      (mean (select-vals audiogram [500 1000 2000 4000]))
      nil)))

(defn threshold->level [threshold]
  (cond
    (nil? threshold) nil
    (<= threshold 20) "normal"
    (<= 20 threshold 40) "light"
    (<= 40 threshold 55) "mid"                              ;; "mid-1"
    (<= 55 threshold 70) "mid"                              ;; "mid-2"
    (<= 70 threshold 80) "severe"                           ;; "severe-1"
    (<= 80 threshold 90) "severe"                           ;; "severe-2"
    (>= threshold 90) "deep"
    :else nil))

(defn typology [audiogram]
  {:average-loss (threshold->level (audio-mean audiogram))
   :high-loss    (threshold->level (mean (->> audiogram
                                              (filter (fn [[freq _]] (>= freq 1000)))
                                              (map val))))
   :low-loss     (threshold->level (mean (->> audiogram
                                              (filter (fn [[freq _]] (<= freq 1000)))
                                              (map val))))})


(defn tests->typology [tonal-tests]
  (->> tonal-tests
       (group-by :ears)
       (map-object #(->> %
                         (sort-by :done_at)
                         last
                         convert-result-in-test
                         :result
                         typology))))

(def ttt (all-audiograms))

(defn audiograms->typologies [audiograms]
  (as-> audiograms $
        (group-by :patient_id $)
        (map-object tests->typology $)))

(defn distribution-of [typologies ear]
  (->> (map #(get % ear) typologies)
       (filter :average-loss)
       frequencies
       (sort-by val)))

(defn typology-distribution [audiograms]
  (let [typologies (vals (audiograms->typologies audiograms))]
    {"L" (distribution-of typologies "L")
     "R" (distribution-of typologies "R")}))

(typology-distribution ttt)



