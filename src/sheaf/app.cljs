(ns sheaf.app
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom]))

;; generic helper fns

(defn scroll-to-top! []
  ;; same thing, different browsers
  (set! js/document.body.scrollTop 0)
  (set! js/document.documentElement.scrollTop 0))

(defn substring? [super sub]
  (not= -1 (.indexOf super sub)))

(defn value [ev]
  (.. ev -target -value))

;; app state

(def app-state
  (atom {:submission {:url "" :title "" :tags ""}}))

(def default-links
  '({:title "Locked doors, headaches, and intellectual need"
     :url "http://mkremins.github.io/blog/doors-headaches-intellectual-need/"
     :tags #{"games" "learning"}}))

(defn load-links! []
  (let [stored (some-> (js/localStorage.getItem "links") reader/read-string)]
    (swap! app-state assoc :links (or stored default-links))))

(defn store-changes! [_ _ old-state new-state]
  (when-not (= (:links new-state) (:links old-state))
    (js/localStorage.setItem "links" (pr-str (:links new-state)))))

(defn can-submit? [submission]
  (and (seq (:url submission)) (seq (:title submission))))

(defn parse-tags [s]
  (->> (str/split s #",")
       (map #(str/replace % #"\s+" "_"))
       set))

(defn submit-link [{:keys [submission] :as state}]
  (let [submission (update submission :tags parse-tags)]
    (-> (update state :links conj submission)
        (assoc :submission {:url "" :title "" :tags ""}))))

(defn delete-link [state url]
  (update state :links (fn [links] (remove #(= (:url %) url) links))))

(defn edit-link [state url]
  (let [link (-> (first (filter #(= (:url %) url) (:links state)))
                 (update :tags (partial str/join ",")))]
    (-> (delete-link state url)
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
        (some #(substring? % text) (map str/lower-case (:tags link))))))

(defn parse-query-part [part]
  (if (= (first part) "-")
    (complement (parse-query-part (subs part 1)))
    (let [[k v] (str/split part #":" 2)
          k (str/lower-case k)]
      (cond
        (and v (= k "domain")) (from-domain? (str/lower-case v))
        (and v (= k "tag")) (tagged? v)
        :else (matches-text? (str/lower-case part))))))

(defn parse-query [query]
  (->> (str/split (str/trim query) #"\s+")
       (map parse-query-part)
       (reduce every-pred)))

(defn filtered-links [{:keys [links query]}]
  (cond->> links (seq query) (filter (parse-query query))))

;; Om components

(defcomponent search-view [data owner]
  (render [_]
    (dom/div {:class "search"}
      (dom/input {:on-change #(om/update! data :query (value %))
                  :placeholder "Type to search..."
                  :type "text"
                  :value (:query data)})
      (when (seq (:query data))
        (dom/span {:class "clear-search"
                   :on-click (fn [_] (om/transact! data #(dissoc % :query)))}
          "✕")))))

(defcomponent submit-view [data owner]
  (render [_]
    (letfn [(submit-on-enter! [ev]
              (when (and (= (.-key ev) "Enter") (can-submit? @data))
                (swap! app-state submit-link)))]
      (dom/div {:class "submit"}
        (dom/table
          (dom/tr
            (dom/td "URL")
            (dom/td
              (dom/input {:on-change #(om/update! data :url (value %))
                          :on-key-up submit-on-enter!
                          :type "text"
                          :value (:url data)})))
          (dom/tr
            (dom/td "Title")
            (dom/td
              (dom/input {:on-change #(om/update! data :title (value %))
                          :on-key-up submit-on-enter!
                          :type "text"
                          :value (:title data)})))
          (dom/tr
            (dom/td "Tags")
            (dom/td
              (dom/input {:on-change #(om/update! data :tags (value %))
                          :on-key-up submit-on-enter!
                          :type "text"
                          :value (:tags data)}))))
        (dom/button {:class "submit-button"
                     :disabled (not (can-submit? data))
                     :on-click #(swap! app-state submit-link)}
          "Submit")))))

(defn update-query [state type text ev]
  (let [term (str (when (.-shiftKey ev) "-") type ":" text)]
    (if (.-metaKey ev)
      (update state :query str " " term)
      (assoc state :query term))))

(defcomponent tag-view [data owner]
  (render [_]
    (dom/span {:class "tag"
               :on-click #(swap! app-state update-query "tag" data %)}
      (str/replace data #"_" " "))))

(defcomponent link-buttons-view [data owner]
  (init-state [_]
    {:confirm-deletion? false})
  (render-state [_ {:keys [confirm-deletion?]}]
    (dom/span {:class "buttons"}
      (dom/span {:class "button"
                 :on-click #(do (swap! app-state edit-link (:url data))
                                (scroll-to-top!))}
        "edit")
      (dom/span {:class "button"
                 :on-click #(om/set-state! owner :confirm-deletion? true)}
        "delete")
      (when confirm-deletion?
        (dom/span
          "(are you sure? "
          (dom/span {:class "button"
                     :on-click #(swap! app-state delete-link (:url data))}
            "yes")
          " / "
          (dom/span {:class "button"
                     :on-click #(om/set-state! owner :confirm-deletion? false)}
            "no")
          ")")))))

(defcomponent link-view [data owner]
  (render [_]
    (dom/div {:class "link"}
      (dom/div
        (dom/a {:class "title" :href (:url data)} (:title data))
        (let [d (domain data)]
          (dom/span {:class "domain"
                     :on-click #(swap! app-state update-query "domain" d %)}
            d)))
      (dom/span {:class "tags"}
        (om/build-all tag-view (sort (:tags data))))
      (om/build link-buttons-view data))))

(defcomponent tag-counts-view [data owner]
  (render [_]
    (let [tag-counts (->> (frequencies (mapcat :tags data))
                          (sort-by (juxt (comp - val) key)))]
      (dom/div {:class "tag-counts"}
        (for [[tag count] tag-counts]
          (dom/span
            (om/build tag-view tag)
            (dom/span {:class "count"} count)))))))

(defcomponent app [data owner]
  (render [_]
    (let [links (filtered-links data)]
      (dom/div
        (om/build search-view data)
        (om/build submit-view (:submission data))
        (dom/div {:class "links"}
          (om/build-all link-view links {:key :url}))
        (om/build tag-counts-view links)))))

;; tying it all together

(defn init []
  (enable-console-print!)
  (add-watch app-state :storage store-changes!)
  (load-links!)
  (om/root app app-state {:target (js/document.getElementById "app")}))

(init)
