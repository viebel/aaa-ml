;; gorilla-repl.fileformat = 1

;; **
;;; 
;; **

;; @@
(ns gains
  (:require [gorilla-plot.core :as plot]
            [gadjett.collections :refer [map-object]]
            [big-data.sandbox :refer [patient-gain-history]]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1000</span>","value":"1000"},{"type":"html","content":"<span class='clj-long'>1500</span>","value":"1500"},{"type":"html","content":"<span class='clj-long'>2000</span>","value":"2000"},{"type":"html","content":"<span class='clj-long'>3000</span>","value":"3000"},{"type":"html","content":"<span class='clj-long'>4000</span>","value":"4000"},{"type":"html","content":"<span class='clj-long'>750</span>","value":"750"},{"type":"html","content":"<span class='clj-long'>500</span>","value":"500"},{"type":"html","content":"<span class='clj-long'>250</span>","value":"250"}],"value":"(1000 1500 2000 3000 4000 750 500 250)"}
;; <=

;; @@

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gains/my-data</span>","value":"#'gains/my-data"}
;; <=

;; @@
(defn freqs [data]
  (->> data
       first
       second
       (map first)
       sort))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gains/freqs</span>","value":"#'gains/freqs"}
;; <=

;; @@
(defn data-to-plot [data freq]
  (->> data
       (map (fn [[k v]] 
              [(:months-since-equipped k)
               (:distance-from-target (second (first (filter #(= freq (first %)) v))))]))))

(defn freq-colours [freq]
  (let [colours {8000 "violet"
                 4000 "blue"
                 3000 "lightblue"
                 2000 "darkgreen"
                 1500 "cyan"
                 1000 "green"
                 750 "yellow"
                 500 "orange"
                 250 "red"}]
       (colours freq "black")))
(defn low-freqs [d]
  (map-object (fn [x] (filter #(<= (first %) 1000) x)) (into {} d)))
(defn high-freqs [d]
  (map-object (fn [x] (filter #(> (first %) 1000) x)) (into {} d)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gains/high-freqs</span>","value":"#'gains/high-freqs"}
;; <=

;; @@
(defn draw [data]
  (apply plot/compose 
         (map 
           #(plot/list-plot 
              (data-to-plot data %) 
              :joined true
              :colour (freq-colours %)
              :plot-range [:all [-20 50]]) 
           (freqs data))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gains/draw</span>","value":"#'gains/draw"}
;; <=

;; **
;;; # Choix du patient
;; **

;; @@
(def patient-id 39577)
(def my-data (patient-gain-history patient-id))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gains/high-freqs</span>","value":"#'gains/high-freqs"}
;; <=

;; @@
(freqs (get-in my-data ["R" :gain-over-time]))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1000</span>","value":"1000"},{"type":"html","content":"<span class='clj-long'>1500</span>","value":"1500"},{"type":"html","content":"<span class='clj-long'>2000</span>","value":"2000"},{"type":"html","content":"<span class='clj-long'>3000</span>","value":"3000"},{"type":"html","content":"<span class='clj-long'>4000</span>","value":"4000"},{"type":"html","content":"<span class='clj-long'>750</span>","value":"750"},{"type":"html","content":"<span class='clj-long'>500</span>","value":"500"},{"type":"html","content":"<span class='clj-long'>250</span>","value":"250"}],"value":"(1000 1500 2000 3000 4000 750 500 250)"}
;; <=

;; **
;;; # Oreille droite
;; **

;; @@
(freqs (get-in my-data ["R" :gain-over-time]))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>250</span>","value":"250"},{"type":"html","content":"<span class='clj-long'>500</span>","value":"500"},{"type":"html","content":"<span class='clj-long'>750</span>","value":"750"},{"type":"html","content":"<span class='clj-long'>1000</span>","value":"1000"},{"type":"html","content":"<span class='clj-long'>1500</span>","value":"1500"},{"type":"html","content":"<span class='clj-long'>2000</span>","value":"2000"},{"type":"html","content":"<span class='clj-long'>3000</span>","value":"3000"},{"type":"html","content":"<span class='clj-long'>4000</span>","value":"4000"}],"value":"(250 500 750 1000 1500 2000 3000 4000)"}
;; <=

;; **
;;; ## Basses fréquences (en dessous de 1000 Hz)
;; **

;; @@
(-> (get-in my-data ["R" :gain-over-time])
    low-freqs
    draw)
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"b5973a08-e620-4dcc-8575-a3be84dadf76","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[-20,50]}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"b5973a08-e620-4dcc-8575-a3be84dadf76","values":[{"x":0,"y":17.5},{"x":13,"y":12.5},{"x":19,"y":-5.0}]},{"name":"d1bec95d-9fb7-4fa8-a783-2fd6ea3e3bc4","values":[{"x":0,"y":-2.5},{"x":13,"y":17.5},{"x":19,"y":2.5}]},{"name":"3b89326b-0a13-4992-bbf7-5e7e36fae663","values":[{"x":0,"y":-15.0},{"x":13,"y":10.0},{"x":19,"y":2.5}]},{"name":"f05d0dd7-9fc2-4a90-9982-bc136580f548","values":[{"x":0,"y":-25.0},{"x":13,"y":null},{"x":19,"y":0.0}]}],"marks":[{"type":"line","from":{"data":"b5973a08-e620-4dcc-8575-a3be84dadf76"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"green"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"d1bec95d-9fb7-4fa8-a783-2fd6ea3e3bc4"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"yellow"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"3b89326b-0a13-4992-bbf7-5e7e36fae663"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"orange"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"f05d0dd7-9fc2-4a90-9982-bc136580f548"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"red"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"b5973a08-e620-4dcc-8575-a3be84dadf76\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [-20 50]}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"b5973a08-e620-4dcc-8575-a3be84dadf76\", :values ({:x 0, :y 17.5} {:x 13, :y 12.5} {:x 19, :y -5.0})} {:name \"d1bec95d-9fb7-4fa8-a783-2fd6ea3e3bc4\", :values ({:x 0, :y -2.5} {:x 13, :y 17.5} {:x 19, :y 2.5})} {:name \"3b89326b-0a13-4992-bbf7-5e7e36fae663\", :values ({:x 0, :y -15.0} {:x 13, :y 10.0} {:x 19, :y 2.5})} {:name \"f05d0dd7-9fc2-4a90-9982-bc136580f548\", :values ({:x 0, :y -25.0} {:x 13, :y nil} {:x 19, :y 0.0})}), :marks ({:type \"line\", :from {:data \"b5973a08-e620-4dcc-8575-a3be84dadf76\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"green\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"d1bec95d-9fb7-4fa8-a783-2fd6ea3e3bc4\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"yellow\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"3b89326b-0a13-4992-bbf7-5e7e36fae663\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"orange\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"f05d0dd7-9fc2-4a90-9982-bc136580f548\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"red\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; **
;;; ## Hautes fréquences (au dessus de 1000 Hz)
;; **

;; @@
(-> (get-in my-data ["R" :gain-over-time])
    high-freqs
    draw)
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"8de59422-709a-40b4-bb96-d540bce6d66e","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[-20,50]}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"8de59422-709a-40b4-bb96-d540bce6d66e","values":[{"x":0,"y":10.0},{"x":13,"y":2.5},{"x":19,"y":-10.0}]},{"name":"a140d5b1-f39e-4a32-aaac-4d6e68f1f9b1","values":[{"x":0,"y":10.0},{"x":13,"y":10.0},{"x":19,"y":15.0}]},{"name":"054ba36d-bf77-4f31-be32-bd34feeef750","values":[{"x":0,"y":-5.0},{"x":13,"y":-5.0},{"x":19,"y":-10.0}]},{"name":"6d49de43-e4f5-4ed6-99ff-138242235067","values":[{"x":0,"y":17.5},{"x":13,"y":0.0},{"x":19,"y":-7.5}]}],"marks":[{"type":"line","from":{"data":"8de59422-709a-40b4-bb96-d540bce6d66e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"cyan"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"a140d5b1-f39e-4a32-aaac-4d6e68f1f9b1"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"darkgreen"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"054ba36d-bf77-4f31-be32-bd34feeef750"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"lightblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"6d49de43-e4f5-4ed6-99ff-138242235067"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"blue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"8de59422-709a-40b4-bb96-d540bce6d66e\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [-20 50]}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"8de59422-709a-40b4-bb96-d540bce6d66e\", :values ({:x 0, :y 10.0} {:x 13, :y 2.5} {:x 19, :y -10.0})} {:name \"a140d5b1-f39e-4a32-aaac-4d6e68f1f9b1\", :values ({:x 0, :y 10.0} {:x 13, :y 10.0} {:x 19, :y 15.0})} {:name \"054ba36d-bf77-4f31-be32-bd34feeef750\", :values ({:x 0, :y -5.0} {:x 13, :y -5.0} {:x 19, :y -10.0})} {:name \"6d49de43-e4f5-4ed6-99ff-138242235067\", :values ({:x 0, :y 17.5} {:x 13, :y 0.0} {:x 19, :y -7.5})}), :marks ({:type \"line\", :from {:data \"8de59422-709a-40b4-bb96-d540bce6d66e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"cyan\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"a140d5b1-f39e-4a32-aaac-4d6e68f1f9b1\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"darkgreen\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"054ba36d-bf77-4f31-be32-bd34feeef750\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"lightblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"6d49de43-e4f5-4ed6-99ff-138242235067\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"blue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; **
;;; # Oreille gauche
;; **

;; @@
(freqs (get-in my-data ["L" :gain-over-time]))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>500</span>","value":"500"},{"type":"html","content":"<span class='clj-long'>750</span>","value":"750"},{"type":"html","content":"<span class='clj-long'>1000</span>","value":"1000"},{"type":"html","content":"<span class='clj-long'>1500</span>","value":"1500"},{"type":"html","content":"<span class='clj-long'>2000</span>","value":"2000"},{"type":"html","content":"<span class='clj-long'>3000</span>","value":"3000"},{"type":"html","content":"<span class='clj-long'>4000</span>","value":"4000"}],"value":"(500 750 1000 1500 2000 3000 4000)"}
;; <=

;; **
;;; ## Basses fréquences (en dessous de 1000 Hz)
;; **

;; @@
(-> (get-in my-data ["L" :gain-over-time])
    low-freqs
    draw)
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"708fe186-9db4-4279-b649-dee9d102ac1d","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[-20,50]}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"708fe186-9db4-4279-b649-dee9d102ac1d","values":[{"x":0,"y":15.0},{"x":13,"y":7.5},{"x":19,"y":-5.0}]},{"name":"0a285ab0-a342-46c0-9c49-3ea55210f22a","values":[{"x":0,"y":20.0},{"x":13,"y":7.5},{"x":19,"y":17.5}]},{"name":"5e2b2cb5-fc6a-4824-92da-10e3f857c129","values":[{"x":0,"y":25.0},{"x":13,"y":12.5},{"x":19,"y":22.5}]}],"marks":[{"type":"line","from":{"data":"708fe186-9db4-4279-b649-dee9d102ac1d"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"green"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"0a285ab0-a342-46c0-9c49-3ea55210f22a"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"yellow"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"5e2b2cb5-fc6a-4824-92da-10e3f857c129"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"orange"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"708fe186-9db4-4279-b649-dee9d102ac1d\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [-20 50]}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"708fe186-9db4-4279-b649-dee9d102ac1d\", :values ({:x 0, :y 15.0} {:x 13, :y 7.5} {:x 19, :y -5.0})} {:name \"0a285ab0-a342-46c0-9c49-3ea55210f22a\", :values ({:x 0, :y 20.0} {:x 13, :y 7.5} {:x 19, :y 17.5})} {:name \"5e2b2cb5-fc6a-4824-92da-10e3f857c129\", :values ({:x 0, :y 25.0} {:x 13, :y 12.5} {:x 19, :y 22.5})}), :marks ({:type \"line\", :from {:data \"708fe186-9db4-4279-b649-dee9d102ac1d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"green\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"0a285ab0-a342-46c0-9c49-3ea55210f22a\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"yellow\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"5e2b2cb5-fc6a-4824-92da-10e3f857c129\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"orange\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; **
;;; ## Hautes fréquences (au dessus de 1000 Hz)
;; **

;; @@
(-> (get-in my-data ["L" :gain-over-time])
    high-freqs
    draw)
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"e6a19552-8f59-42a6-a1d5-b95299713cd7","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[-20,50]}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"e6a19552-8f59-42a6-a1d5-b95299713cd7","values":[{"x":0,"y":17.5},{"x":13,"y":5.0},{"x":19,"y":10.0}]},{"name":"6a588f06-ce2c-4326-89d5-e0fa38ca1ade","values":[{"x":0,"y":20.0},{"x":13,"y":10.0},{"x":19,"y":10.0}]},{"name":"0d9ff281-2408-4e9c-97b8-3e5d29a9ac0a","values":[{"x":0,"y":17.5},{"x":13,"y":-2.5},{"x":19,"y":5.0}]},{"name":"dcabb358-9103-4609-bc07-673819bf8aa1","values":[{"x":0,"y":12.5},{"x":13,"y":0.0},{"x":19,"y":10.0}]}],"marks":[{"type":"line","from":{"data":"e6a19552-8f59-42a6-a1d5-b95299713cd7"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"cyan"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"6a588f06-ce2c-4326-89d5-e0fa38ca1ade"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"darkgreen"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"0d9ff281-2408-4e9c-97b8-3e5d29a9ac0a"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"lightblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"dcabb358-9103-4609-bc07-673819bf8aa1"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"blue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"e6a19552-8f59-42a6-a1d5-b95299713cd7\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [-20 50]}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"e6a19552-8f59-42a6-a1d5-b95299713cd7\", :values ({:x 0, :y 17.5} {:x 13, :y 5.0} {:x 19, :y 10.0})} {:name \"6a588f06-ce2c-4326-89d5-e0fa38ca1ade\", :values ({:x 0, :y 20.0} {:x 13, :y 10.0} {:x 19, :y 10.0})} {:name \"0d9ff281-2408-4e9c-97b8-3e5d29a9ac0a\", :values ({:x 0, :y 17.5} {:x 13, :y -2.5} {:x 19, :y 5.0})} {:name \"dcabb358-9103-4609-bc07-673819bf8aa1\", :values ({:x 0, :y 12.5} {:x 13, :y 0.0} {:x 19, :y 10.0})}), :marks ({:type \"line\", :from {:data \"e6a19552-8f59-42a6-a1d5-b95299713cd7\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"cyan\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"6a588f06-ce2c-4326-89d5-e0fa38ca1ade\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"darkgreen\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"0d9ff281-2408-4e9c-97b8-3e5d29a9ac0a\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"lightblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"dcabb358-9103-4609-bc07-673819bf8aa1\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"blue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=
