;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns gains
  (:require [gorilla-plot.core :as plot]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

(require '[honeysql.core :as sql]
         '[honeysql.helpers :refer :all :as helpers])

(require '[clojure.java.jdbc :as j])
(require '[gadjett.collections :as gadjett])

(require '[clj-time.core :as t])
(require '[clj-time.format :as f])
(require '[clj-time.coerce :as c])

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def db {:dbtype "postgresql", :dbname "audyx", :host "localhost", :port "5433", :user "me", :password "me"})

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gains/db</span>","value":"#'gains/db"}
;; <=

;; @@
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gains/gain-estimate</span>","value":"#'gains/gain-estimate"}
;; <=

;; @@
(def data (gadjett/map-object gain-estimate (calculate-gains thresholds-over-time)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gains/data</span>","value":"#'gains/data"}
;; <=

;; @@
(def data-to-plot (->> data
     (filter (fn [[k v]] (= (:ears  k) "R")))
     (map (fn [[k v]] [(:done_at_day k) v]))))
data-to-plot

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;2014-02-28&quot;</span>","value":"\"2014-02-28\""},{"type":"html","content":"<span class='clj-long'>35</span>","value":"35"}],"value":"[\"2014-02-28\" 35]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;2015-01-22&quot;</span>","value":"\"2015-01-22\""},{"type":"html","content":"<span class='clj-long'>25</span>","value":"25"}],"value":"[\"2015-01-22\" 25]"}],"value":"([\"2014-02-28\" 35] [\"2015-01-22\" 25])"}
;; <=

;; @@
(plot/list-plot data-to-plot)
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"c55efd5c-16df-4f86-95b9-c9207298316e","values":[{"x":"2014-02-28","y":35},{"x":"2015-01-22","y":25}]}],"marks":[{"type":"symbol","from":{"data":"c55efd5c-16df-4f86-95b9-c9207298316e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"c55efd5c-16df-4f86-95b9-c9207298316e","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"c55efd5c-16df-4f86-95b9-c9207298316e","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"c55efd5c-16df-4f86-95b9-c9207298316e\", :values ({:x \"2014-02-28\", :y 35} {:x \"2015-01-22\", :y 25})}], :marks [{:type \"symbol\", :from {:data \"c55efd5c-16df-4f86-95b9-c9207298316e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"c55efd5c-16df-4f86-95b9-c9207298316e\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"c55efd5c-16df-4f86-95b9-c9207298316e\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@

;; @@
