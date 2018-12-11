(ns big-data.sandbox
  (:refer-clojure :exclude [update])
  (:require [honeysql.core :as sql]
            [honeysql.helpers :refer :all :as helpers]
            [clojure.java.jdbc :as j]
            [gadjett.collections :as gadjett :refer [map-object mean select-vals mapify]]
            [gadjett.core :refer [dbg]]
            [clojure.pprint :as pprint]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [clojure.set :refer [intersection]]))


(set! *print-length* 30)

;; Global settings

;;; db connection
(def db {:dbtype "postgresql", :dbname "audyx_prod", :host "localhost", :port "5433", :user "me", :password "me"})


(declare typology)
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

(comment
  (hash->id 366623)
  (id->hash 39577))

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

(def all-tests-q
  (-> (select :patient_id :done_at :result :ears :equipped)
      (from :tests)
      (order-by [[:done_at :asc]])
      (where [:= :type_id tonal-type-id])))

(defn all-tests []
  (j/query db (sql/format all-tests-q)))

(defn all-centers []
  (->> (j/query db (sql/format
                     (-> (select :id)
                         (from :centers))))
       (map :id)))

(defn delete-table [t]
  (j/execute! db (sql/format (delete-from t))))

(comment
  (count (all-centers)))



(defn all-tests-of-center-q [center-id]
  (-> (select :patient_id :center_id :done_at :result :ears :equipped)
      (from :tests)
      (join :patients [:= :tests.patient_id :patients.id])
      (order-by [[:done_at :asc]])
      (where [:and [:= :type_id tonal-type-id] [:= :center_id center-id]])))

(defn all-tests-of-center [center-id]
  (j/query db (sql/format (all-tests-of-center-q center-id))))

(comment
  (j/query db (sql/format tests-count-by-user))

  (j/query db ["SELECT * from patients where id = 43510"])
  (j/query db ["SELECT * from centers where id = 7"]))

(defn tonal-tests-query [patient-id]
  {:select [:done_at [(sql/call :to_char :done_at "yyyy-mm-dd") "done_at_day"] :result :ears :equipped]
   :from   [:tests]
   :where  [:and [:= :type_id tonal-type-id] [:= :patient_id patient-id]]
   :order-by  [[:done_at :asc]]})

(def patient-id 39577)

(defn tonal-tests [patient-id]
  (j/query db (sql/format (tonal-tests-query patient-id))))

(defn patient-details-q []
  (-> (select :id :birthdate :gender)
      (from :patient_details)))


(defn patient-details []
  (->> (j/query db (sql/format (patient-details-q)))
       (mapify :id)
       (map-object (fn [{:keys [birthdate] :as details}]
                     (assoc details :birthdate
                                    (when birthdate ""
                                      (f/parse (f/formatters :date) birthdate)))))))


(defn insert-equipment-success-q [data]
  (-> (insert-into :equipment_success)
      (values data)))

(defn insert-equipment-success! [data]
  (j/execute! db (sql/format (insert-equipment-success-q data))))

(defn insert-equipment-success-in-parts! [data]
  (doseq [p (partition 100 100 nil data)]
    (println (insert-equipment-success! p))))

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
       :distance-from-target (- gain target)
       :success (>= (- gain target) -3) })))

(defn target-measure [non-equipped equipped]
  (for [freq (keys (:result equipped))]
    [freq (gain-and-loss (get-in non-equipped [:result freq])
                         (get-in equipped [:result freq]))]))


(defn calc-gain-vs-target [audiogram equipped-start {:keys [done_at] :as equipped-test}]
  [{:date                  done_at
    :months-since-equipped (diff-in-months (c/from-date equipped-start)
                                           (c/from-date done_at))}
   (target-measure audiogram equipped-test)])


(defn equipped-audiogram->story
  [audiogram equipped-start {:keys [done_at] :as equipped-test} {:keys [birthdate gender]}]
  {:date                            done_at
   :gender gender
   :age-in-months-at-equipped-start (when birthdate
                                      (try (diff-in-months birthdate
                                                           (c/from-date equipped-start))
                                           (catch Exception _ nil)))
   :months-since-equipped           (diff-in-months (c/from-date equipped-start)
                                                    (c/from-date done_at))
   :typology                        (typology (:result audiogram))
   :non-equipped-audiogram          audiogram
   :equipped-audiogram              equipped-test})

(defn tests->gains [tests]
  (reduce (fn [{:keys [last-audiogram equipped-start] :as history}
               {:keys [equipped] :as test}]
            (if-not equipped
              (let [audiogram (convert-result-in-test test)]
                (assoc history :last-audiogram audiogram
                               :last-typology (typology (:result audiogram))))
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


(defn tests->story [details tests]
  (:equipped-audiograms
    (reduce (fn [{:keys [last-audiogram equipped-start] :as history}
                 {:keys [equipped patient_id] :as test}]
              (if-not equipped
                (let [audiogram (convert-result-in-test test)]
                  (assoc history :last-audiogram audiogram))
                (as-> history $
                      (if (nil? equipped-start)
                        (assoc $ :equipped-start (:done_at test))
                        (clojure.core/update $
                                             :equipped-audiograms
                                             #(conj % (equipped-audiogram->story last-audiogram
                                                                                 (:equipped-start $)
                                                                                 (convert-result-in-test test)
                                                                                 (get details patient_id))))))))
            {:equipped-start      nil
             :last-audiogram      nil
             :equipped-audiograms []}
            tests)))

(defn patient-gain-history [patient-id]
  (->> (tonal-tests patient-id)
       (group-by :ears)
       (map-object tests->gains)))


(defn data-to-plot [data freq]
  (->> data
       (map (fn [[k v]] [(:months-since-equipped k)
                         (:distance-from-target (second (first (filter #(= freq (first %)) v))))]))))


(defn audio-mean [audiogram]
  (let [vals (select-vals audiogram [500 1000 2000 4000])]
    (if (= (count vals) 4)
      (float (mean (select-vals audiogram [500 1000 2000 4000])))
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


(defn audio-means [audiogram]
  {:freq-250 (get audiogram 250)
   :freq-500 (get audiogram 500)
   :freq-750 (get audiogram 750)
   :freq-1000 (get audiogram 1000)
   :freq-1500 (get audiogram 1500)
   :freq-2000 (get audiogram 2000)
   :freq-3000 (get audiogram 3000)
   :freq-4000 (get audiogram 4000)
   :freq-6000 (get audiogram 6000)
   :freq-8000 (get audiogram 8000)
   :average-loss (audio-mean audiogram)
   :high-loss    (float (mean (->> audiogram
                                   (filter (fn [[freq _]] (>= freq 1000)))
                                   (map val))))
   :low-loss     (float (mean (->> audiogram
                                   (filter (fn [[freq _]] (<= freq 1000)))
                                   (map val))))})


(defn typology [audiogram]
  (map-object threshold->level (audio-means audiogram)))

(defn tests->typology [tonal-tests]
  (->> tonal-tests
       (group-by :ears)
       (map-object #(->> %
                         (sort-by :done_at)
                         last
                         convert-result-in-test
                         :result
                         typology))))

(comment
  (def data (patient-gain-history 39577))
  (data-to-plot (get-in data ["R" :gain-over-time]) 1000)

  (def audiograms (all-audiograms))

  (first audiograms)
  (count (group-by :patient_id audiograms)))

(defn audiograms->typologies [audiograms]
  (as-> audiograms $
        (group-by :patient_id $)
        (map-object tests->typology $)))

(defn distribution-of [typologies ear]
  (->> (map vals typologies)
       (apply concat)
       (filter :average-loss)
       frequencies
       (sort-by val)))

(defn typology-distribution [audiograms]
  (let [typologies (vals (audiograms->typologies audiograms))]
    (distribution-of typologies "L")))


(defn gain->stats [last-typology gain]
  (let [[{:keys [months-since-equipped]} freqs] gain]
    (for [[freq {:keys [success]}] freqs]
      [last-typology freq months-since-equipped success])))

(defn results->stats [{:keys [gain-over-time last-typology]}]
  (->> gain-over-time
       (mapcat (partial gain->stats last-typology))))

(defn safe-inc [x]
  (if (nil? x)
    1
    (inc x)))

(defn success-by-months [ccc]
        (->> ccc
             (reduce (fn [res [freq months success?]]
                       (update-in res [months success?] safe-inc))
                     {})
             (sort-by key)
             (reduce (fn [[res total-true total-false] [months data]]
                       (let [total-true (+ (get data true 0) total-true)
                             total-false (+ (get data false 0) total-false)
                             total (+ total-true total-false)]
                         [(if (zero? total)
                            res
                            (-> res
                                (assoc-in [months :total-true] total-true)
                                (assoc-in [months :total-false] total-false)
                                (assoc-in [months :total] total)
                                (assoc-in [months :success-ratio] (float (/ total-true total)))))
                          total-true
                          total-false
                          ]))
                     [{} 0 0])
             first
             (sort-by key)))

(defn success-by-typology [abc]
  (as-> abc $
        (group-by first $)
        (map-object success-by-months $)))


(defn dist->table [dist]
  (with-out-str (let [total (apply + (map second dist))]
                  (pprint/print-table (reverse (map (fn [[k v]] (assoc k :count v :ratio (format "%.2f" (* 100 (float (/ v total)))))) dist))))))



(comment
  (def tests (all-tests))

  (count tests)
  (def data-success-by-typology (time (as-> tests $
                                            ;(take 10 $)
                                            (group-by #(select-vals % [:ears :patient_id]) $)
                                            (vals $)
                                            (map tests->gains $)
                                            (filter :equipped-start $)
                                            (mapcat results->stats $)
                                            (group-by first $)
                                            (map-object #(map rest %) $)
                                            (map-object success-by-typology $)
                                            )))
  (def dist (typology-distribution audiograms))
  (print (dist->table dist))
  )





(defn flatten-test [{:keys [months-since-equipped age-in-months-at-equipped-start gender non-equipped-audiogram equipped-audiogram]}]
  (let [{:keys [patient_id center_id ears]} equipped-audiogram
        {:keys [result]} non-equipped-audiogram
        {:keys [average-loss high-loss low-loss freq-250 freq-500 freq-750 freq-1000 freq-1500 freq-2000 freq-3000 freq-4000 freq-6000 freq-8000]} (audio-means result)
        {eq-average-loss :average-loss
         eq-high-loss :high-loss
         eq-low-loss :low-loss
         eq-freq-500 :freq-500
         eq-freq-1000 :freq-1000
         eq-freq-2000 :freq-2000
         eq-freq-4000 :freq-4000
         } (audio-means (:result equipped-audiogram))]
    {:patient_id   patient_id
     :gender gender
     :center_id    center_id
     :ears ears
     :age_in_months_at_equipped_start age-in-months-at-equipped-start
     :months_since_equipped months-since-equipped
     :freq_250 freq-250
     :freq_500 freq-500
     :freq_750 freq-750
     :freq_1000 freq-1000
     :freq_1500 freq-1500
     :freq_2000 freq-2000
     :freq_3000 freq-3000
     :freq_4000 freq-4000
     :freq_6000 freq-6000
     :freq_8000 freq-8000
     :average_loss average-loss
     :high_loss    high-loss
     :low_loss     low-loss
     :eq_freq_500 eq-freq-500
     :eq_freq_1000 eq-freq-1000
     :eq_freq_2000 eq-freq-2000
     :eq_freq_4000 eq-freq-4000
     :eq_average_loss eq-average-loss
     :eq_high_loss    eq-high-loss
     :eq_low_loss     eq-low-loss}))


(defn flatten-tests [tests]
  (map flatten-test tests))

(defn tests->stories [details tests]
  (as-> tests $
        (group-by #(select-vals % [:ears :patient_id]) $)
        (vals $)
        (map (partial tests->story details) $)
        (remove empty? $)
        (mapcat flatten-tests $)))

(defmacro doseq-indexed [index-sym [item-sym coll] & body]
  `(doseq [[~item-sym ~index-sym]
           (map vector ~coll (range))]
     ~@body))

(defn create-equipment-success-unique []
  (j/execute! db ["DROP TABLE equipment_success_unique"])
  (j/execute! db ["select * into equipment_success_unique from (\nwith summary as (\nselect *, row_number() over (partition by p.patient_id, p.ears\n                               order   by months_since_equipped desc) as rk\n  from equipment_success p)\nselect t.*\nfrom summary t\nwhere t.rk = 1) l "]))

(comment

  (def my-tests (all-tests-of-center 667))
  (count my-tests)
  (sort-by second (frequencies (mapcat #(map :val (-> %
                                               :result
                                               read-string
                                               vals))
                                my-tests)))
  (def details (patient-details))

  (def my-stories (tests->stories details my-tests))

  (take 10 my-stories)
  (count my-stories)

  (insert-equipment-success-in-parts! my-stories)


  (delete-table :patient_details)
  (do
    (delete-table :equipment_success)
    (let [centers (all-centers) #_[667]
          num-centers (count centers)
          birthdates (patient-details)]
      (doseq-indexed i [center centers]
                     (println (str "center #" (inc i) "/" num-centers) "id:" center)
                     (let [tests (all-tests-of-center center)
                           stories (tests->stories birthdates tests)]
                       (println "number of tests:" (count tests) "stories:" (count stories))
                       (when-not (empty? stories)
                         (insert-equipment-success-in-parts! stories))))))
  (create-equipment-success-unique)
  )
