(ns om.devcards.autocomplete2
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [<! >! put! chan]]
            [devcards.core :refer-macros [defcard deftest dom-node]]
            [clojure.string :as string]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [sablono.core :as sab :include-macros true])
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

(defmethod read :search/results
  [{:keys [state ast] :as env} k {:keys [query react-key]}]
  (let [results (merge
                 {:value (get @state k [])}
                 (when-not (and (string/blank? query)
                                (<= 2 (count query)))
                   {:search ast}))]
    (println "results" results)
    results))

;; -----------------------------------------------------------------------------
;; App

(defn result-list [react-key results]
  (dom/ul #js {:key (str react-key "result-list-ul")}
          (map #(dom/li #js {:key (str react-key "result-list-li")} %) results)))

(defn search-field [react-key ac query]
  (let [value "foo-bar-ba"]
    (dom/button
      #js {:key (str react-key "search-field")
           :onClick
           (fn []
             (om/set-query! ac
               {:params {:query value :react-key react-key}}))}
      (str "Autocomplete " react-key " by '" value "'"))))

(defui AutoCompleterUI
  static om/IQueryParams
  (params [this] {:query "" :react-key ""})
  static om/IQuery
  (query [this] '[(:search/results {:query ?query :react-key ?react-key})])
  Object
  (render [this]
          (let [{:keys [search/results react-key]} (om/props this)]
            (dom/div #js {:key react-key}
                     (dom/h2 nil "Autocompleter" " " react-key)
                     (cond->
                         [(search-field react-key this (:query (om/get-params this)))]
                       (not (empty? results)) (conj (result-list react-key results)))))))
(def auto-completer-ui (om/factory AutoCompleterUI {:keyfn :react-key}))

(defui AutoCompleter
  Object
  (render [this]
          (let [{:keys [search/results]} (om/props this)]
            (dom/div nil
                     (auto-completer-ui {:react-key "ac1"})
                     (auto-completer-ui {:react-key "ac2"})))))

(defn search-loop [c]
  (go
    (loop [[react-key query cb remote] (<! c)]
      (if-not (empty? query)
        (let [[_ results] (<! (jsonp (str base-url query)))]
          (println "search-loop" "results" results "react-key" react-key "query" query)
          (cb {:search/results results} #_react-key query remote))
        (cb {:search/results []} #_react-key query remote))
      (recur (<! c)))))

(defn send-to-chan [c]
  (fn [{:keys [search]} cb]
    (when search
      (let [{[search] :children} (om/query->ast search)
            query (get-in search [:params :query])
            react-key (get-in search [:params :react-key])]
        (put! c [react-key query cb :search])))))

(def send-chan (chan))

(def reconciler
  (om/reconciler
    {:state   {:search/results []}
     :parser  (om/parser {:read read})
     :send    (send-to-chan send-chan)
     :remotes [:remote :search]}))

(search-loop send-chan)

(defcard test-autocomplete
  "Demonstrate simple autocompleter"
  (dom-node
    (fn [_ node]
      (om/add-root! reconciler AutoCompleter node))))
