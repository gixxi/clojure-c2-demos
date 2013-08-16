;; Visualizes Swiss Public Transportation Net using C2 (http://keminglabs.com/c2/) - the Clojure data visualization lib.
;; All Data is slurped from https://github.com/gixxi/clojure-swissrailwaynet
;;
;; Author: christian.meichsner@informatik.tu-chemnitz.de

(ns swiss-railway-topo
  (:use [c2.core :only [unify]])
  (:require [c2.scale :as scale]))

(def ^:const nodes-url "https://raw.github.com/gixxi/clojure-swissrailwaynet/master/swiss-public-transportation-net-nodes.dat")
(def ^:const edges-url "https://raw.github.com/gixxi/clojure-swissrailwaynet/master/swiss-public-transportation-net-edges.dat")

(defn slurp-url
  "loads some content from an URL"
  [url]
  (with-open [r (java.io.PushbackReader. 
                  (java.io.InputStreamReader. 
                    (.openStream (java.net.URL. url))))]
    (let [rec (read r)]
       rec)))

(let [;;loading data
      ! (println (format "reading nodes from %s" nodes-url))
      nodes (slurp-url nodes-url)
      ! (println (format "reading edges from %s" edges-url))
      edges (slurp-url edges-url)
      ! (println "done. start rendering ...")
      
      ;;filter-out some errors (the busstop directory contains some points located on the origin which is somewhere near bordeaux, france
      ;;as well as 9640, Warth, Gemeindehaus which is located falsely in italy 
      nodes (filter #(and (contains? % :x) (contains? % :y) (< 1000 (:x %)) (< 1000 (:y %)) (> 51000 (:y %))) nodes)
      node-by-didok (zipmap (map :didok nodes) nodes)

      ;;calculate hull both for scaling as well as for the grid
      min-x (apply min (map :x nodes))
      min-y (apply min (map :y nodes))
      max-x (apply max (map :x nodes))
      max-y (apply max (map :y nodes))

      width 1000, height 600
      scale-x (scale/linear :domain [min-x max-x]
                      :range [0 width])
      scale-y (scale/linear :domain [max-y min-y]
                      :range [0 height])

      ;;reduce set of edges to have one per tuple [:x1 :y1 :x2 :y2]
      visible-edges (reduce #(if (and (contains? node-by-didok (:von %2))
                                      (contains? node-by-didok (:nach %2)))
                               (let [k {:x1 (long (scale-x (:x (get node-by-didok (:von %2)))))
                                        :y1 (long (scale-y (:y (get node-by-didok (:von %2)))))
                                        :x2 (long (scale-x (:x (get node-by-didok (:nach %2)))))
                                        :y2 (long (scale-y (:y (get node-by-didok (:nach %2)))))}]
                                (if 
                                  (contains? % k) %
                                  (assoc % k (merge %2 k))))
                               %)
                               edges)

      ;;calc connectivity per node (mark top 100 nodes) in order to mark some "important" stations
      connectivity (reduce #(cond (contains? % %2)
                                    (assoc % %2 (inc (get % %2)))
                                    :else
                                    (assoc % %2 1)) 
                           {} (concat (map :von edges) (map :nach edges)))
      connectivity (map (fn [val] {:didok (first val) :count (second val)}) connectivity)
      connectivity (take 100 (map :didok (reverse (sort-by :count connectivity))))
      ]


[:svg#main {:style (str "display: block;"
                          "margin: auto;"
                          "height:" height ";"
                          "width:" width ";")}
  ;;render edges
  (unify 
   (vals visible-edges) 
   (fn [val]
     [:line {:id (:id val) :x1 (:x1 val) :y1 (:y1 val) :x2 (:x2 val) :y2 (:y2 val) :stroke "#9999ff" :stroke-width 1}]))
  
  ;;render 100 most important stations
  (unify 
    (filter #(-> % nil? not) 
      (map #(get node-by-didok %) connectivity)) 
    (fn [val]
      [:circle {:id (-> val :didok) :r 3 :cx (-> val :x scale-x float) :cy (-> val :y scale-y float) :fill "red"}]))])
