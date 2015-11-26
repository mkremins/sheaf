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
  (atom {:links data/links
         :submission {:url "" :title "" :tags ""}}))

(defn submit-link [{:keys [submission] :as data}]
  (let [submission (update submission :tags #(set (str/split % #",")))]
    (-> (update data :links conj submission)
        (assoc :submission {:url "" :title "" :tags ""}))))

(defn delete-link [data url]
  (update data :links (fn [links] (vec (remove #(= (:url %) url) links)))))

(defn edit-link [data url]
  (let [link (-> (first (filter #(= (:url %) url) (:links data)))
                 (update :tags (partial str/join ",")))]
    (-> (delete-link data url)
        (assoc :submission link))))

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
      (dom/span {:class "tags"}
        (om/build-all tag-view (sort (:tags data))))
      (dom/span {:class "buttons"}
        (dom/span {:on-click #(swap! app-state edit-link (:url data))} "edit")
        (dom/span {:on-click #(swap! app-state delete-link (:url data))} "delete")))))

(defcomponent submit-view [data owner]
  (render [_]
    (dom/div {:class "submit"}
      (dom/table
        (dom/tr
          (dom/td "URL")
          (dom/td
            (dom/input {:on-change #(om/update! data :url (.. % -target -value))
                        :type "text"
                        :value (:url data)})))
        (dom/tr
          (dom/td "Title")
          (dom/td
            (dom/input {:on-change #(om/update! data :title (.. % -target -value))
                        :type "text"
                        :value (:title data)})))
        (dom/tr
          (dom/td "Tags")
          (dom/td
            (dom/input {:on-change #(om/update! data :tags (.. % -target -value))
                        :type "text"
                        :value (:tags data)}))))
      (dom/button (cond-> {:class "submit-button"
                           :on-click #(swap! app-state submit-link)}
                          (or (empty? (:url data)) (empty? (:title data)))
                          (assoc :disabled true))
        "Submit"))))

(defcomponent app [data owner]
  (render [_]
    (dom/div
      (dom/div {:class "search"}
        (dom/input {:id "search"
                    :on-change #(om/update! data :query (.. % -target -value))
                    :placeholder "Type to search..."
                    :type "text"
                    :value (:query data)}))
      (om/build submit-view (:submission data))
      (dom/div {:class "links"}
        (om/build-all link-view (filtered-links data))))))

;; tying it all together

(om/root app app-state {:target (js/document.getElementById "app")})
