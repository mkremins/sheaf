(ns sheaf.app
  (:require [clojure.string :as str]
            [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom]
            [sheaf.data :as data]))

;; generic helper fns

(defn substring? [super sub]
  (not= -1 (.indexOf super sub)))

;; app state

(def app-state
  (atom {:links data/links}))

;; search functionality

(defn domain [link]
  (-> (:url link)
      (str/replace #"https?://" "")
      (str/split #"[#/\?]")
      first))

(defn from-domain?
  ([d] (partial from-domain? d))
  ([d link] (= (domain link) d)))

(defn tagged?
  ([tag] (partial tagged? tag))
  ([tag link] (contains? (:tags link) tag)))

(defn matches-text?
  ([text] (partial matches-text? text))
  ([text link]
    (or (substring? (str/lower-case (:title link)) text)
        (substring? (str/lower-case (:url link)) text)
        (some #(substring? % text) (:tags link)))))

(defn parse-query-part [part]
  (let [[k v] (str/split part #":" 2)]
    (cond
      (and v (= k "domain")) (from-domain? v)
      (and v (= k "tag")) (tagged? v)
      :else (matches-text? part))))

(defn parse-query [query]
  (->> (str/split (str/lower-case query) #"\s+")
       (remove empty?)
       (map parse-query-part)
       (reduce every-pred)))

(defn filtered-links [{:keys [links query]}]
  (cond->> links (seq query) (filter (parse-query query))))

;; Om components

(defcomponent tag-view [data owner]
  (render [_]
    (dom/span {:class "tag"
               :on-click #(swap! app-state assoc :query (str "tag:" data))}
      data)))

(defcomponent link-view [data owner]
  (render [_]
    (dom/div {:class "link"}
      (dom/div
        (dom/a {:class "title" :href (:url data)} (:title data))
        (let [d (domain data)]
          (dom/span {:class "domain"
                     :on-click #(swap! app-state assoc :query (str "domain:" d))}
            d)))
      (dom/div {:class "tags"}
        (om/build-all tag-view (sort (:tags data)))))))

(defcomponent app [data owner]
  (render [_]
    (dom/div
      (dom/div {:class "search"}
        (dom/input {:id "search"
                    :on-change #(om/update! data :query (.. % -target -value))
                    :placeholder "Type to search..."
                    :type "text"
                    :value (:query data)}))
      (dom/div {:class "links"}
        (om/build-all link-view (filtered-links data))))))

;; tying it all together

(om/root app app-state {:target (js/document.getElementById "app")})
