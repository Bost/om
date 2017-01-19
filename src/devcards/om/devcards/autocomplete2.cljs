(ns om.devcards.autocomplete2
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [<! >! put! chan]]
            [devcards.core :refer-macros [defcard deftest dom-node]]
            [clojure.string :as string]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom])
  (:import [goog Uri]
           [goog.net Jsonp]))

(enable-console-print!)

(def base-url
  "http://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=")

(defn jsonp
  ([uri] (jsonp (chan) uri))
  ([c uri]
   (let [gjsonp (Jsonp. (Uri. uri))]
     (.send gjsonp nil #(put! c %))
     c)))

;; -----------------------------------------------------------------------------
;; Parsing

(defmulti read om/dispatch)

(defn read-state [state k ast]
  (let [hm1 {:value (get state k [])}
        hm2 {:search ast}
        r (merge hm1 hm2)]
    r))

(defmethod read :search/results-id1
  [{:keys [state ast] :as env} k {:keys [query]}]
  (read-state @state k ast))

(defmethod read :search/results-id2
  [{:keys [state ast] :as env} k {:keys [query]}]
  (read-state @state k ast))

;; -----------------------------------------------------------------------------
;; App

(defn result-list [results id]
  (dom/ul #js {:key "list"}
    (map-indexed (fn [idx val]
                   (dom/li #js {:key (str "item-" id "-" idx)}
                           val)) results)))

(defn search-field [ac query id]
  (let [value "foo-bar-ba"]
    (dom/input
     #js {:key "search-field"
          :value (if (empty? query) value query)
          :onChange
          (fn [e]
            (om/set-query!
             ac
             {:params {:query (.. e -target -value)
                       :id id}}))})))

(defui AutoCompleterID1
  static om/IQueryParams
  (params [_]
          {:query "" :id nil})
  static om/IQuery
  (query [_]
         #_(println "AutoCompleterID1" "query")
         '[(:search/results-id1 {:query ?query :id ?id})])
  Object
  (render [this]
          (let [{:keys [search/results-id1 id-prop]} (om/props this)
                {:keys [query id]} (om/get-params this)
                idx (or id id-prop)]
            #_(println "results-id1" results-id1 "query" query "id" id "id-prop" id-prop)
            #_(println "cond->" idx "(not (empty? results-id1))" (not (empty? results-id1)))
            (dom/div #js {:key (str "ac-" id)}
                     (dom/h2 nil "Autocompleter")
                     (cond->
                         [(search-field this query idx)]
                       (not (empty? results-id1)) (conj (result-list results-id1 idx)))))))
(def auto-completer-id1 (om/factory AutoCompleterID1))

(defui AutoCompleterID2
  static om/IQueryParams
  (params [_]
          {:query "" :id nil})
  static om/IQuery
  (query [_]
         #_(println "AutoCompleterID2" "query")
         '[(:search/results-id2 {:query ?query :id ?id})])
  Object
  (render [this]
          (let [{:keys [search/results-id2 id-prop]} (om/props this)
                {:keys [query id]} (om/get-params this)
                idx (or id id-prop)]
            #_(println "results-id2" results-id2 "query" query "id" id "id-prop" id-prop)
            #_(println "cond->" idx "(not (empty? results-id2))" (not (empty? results-id2)))
            (dom/div #js {:key (str "ac-" id)}
                     (dom/h2 nil "Autocompleter")
                     (cond->
                         [(search-field this query idx)]
                       (not (empty? results-id2)) (conj (result-list results-id2 idx)))))))
(def auto-completer-id2 (om/factory AutoCompleterID2))

(defui ACs
  Object
  (render [this]
          (let [props (om/props this)]
            #_(println "props" props)
            (dom/div nil
                     (auto-completer-id1 (merge props {:id-prop "id1"}))
                     (auto-completer-id2 (merge props {:id-prop "id2"}))))))

(defn search-loop [c]
  (go
    (loop [[query id cb remote] (<! c)]
      (let [k :search/results
            kw (keyword (namespace k) (str (name k) "-" id))]
        (println "kw" kw)
        (if-not (empty? query)
          (let [[_ results] (<! (jsonp (str base-url query)))]
            #_(om/transact! this `[(kw ~props)])
            #_(println "results" results)
            (cb {kw results} query remote))
          (do
            (println "(empty? query)" (empty? query))
            (cb {kw []} query remote))))
      (recur (<! c)))))

(defn send-to-chan [c]
  (fn [{:keys [search]} cb]
    (when search
      (let [{[search] :children} (om/query->ast search)
            {:keys [query id]} (get-in search [:params])]
        #_(println "send-to-chan" "id" id)
        (put! c [query id cb :search])))))

(def send-chan (chan))

(def reconciler
  (om/reconciler
   {:state   {:search/results-id1 [] #_(clj->js ["jim"])
              :search/results-id2 [] #_(clj->js ["joe"])}
     :parser  (om/parser {:read read})
     :send    (send-to-chan send-chan)
     :remotes [:remote :search]}))

(search-loop send-chan)

(defcard test-autocomplete
  "Demonstrate multiple ACs"
  (dom-node
    (fn [_ node]
      (om/add-root! reconciler ACs node))))
