;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.workspace.sidebar.options.menus.color-selection
  (:require
   [app.common.attrs :as attrs]
   [app.common.colors :as clr]
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.pages :as cp]
   [app.common.pages.common :as cpc]
   [app.common.text :as txt]
   [app.main.data.workspace.colors :as dc]
   [app.main.data.workspace.common :as dwc]
   [app.main.store :as st]
   [app.main.ui.hooks :as h]
   [app.main.ui.icons :as i]
   [app.main.ui.workspace.sidebar.options.rows.color-row :refer [color-row]]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [rumext.alpha :as mf]))

(defn fill->color-att
  [fill]
  {:color-prop {:color    (:fill-color fill)
                :opacity  (:fill-opacity fill)
                :id       (:fill-color-ref-id fill)
                :file-id  (:fill-color-ref-file fill)
                :gradient (:fill-color-gradient fill)}
   :prop :fill
   :shape-id (:shape-id fill)
   :index (:index fill)})

(defn stroke->color-att
  [stroke]
  {:color-prop {:color    (:stroke-color stroke)
                :opacity  (:stroke-opacity stroke)
                :id       (:stroke-color-ref-id stroke)
                :file-id  (:stroke-color-ref-file stroke)
                :gradient (:stroke-color-gradient stroke)}
   :prop :stroke
   :shape-id (:shape-id stroke)
   :index (:index stroke)})


(defn text->color-att
  [fill]
  {:color-prop {:color    (:fill-color fill)
                :opacity  (:fill-opacity fill)
                :id       (:fill-color-ref-id fill)
                :file-id  (:fill-color-ref-file fill)
                :gradient (:fill-color-gradient fill)}
   :prop :content
   :shape-id (:shape-id fill)
   :index (:index fill)})

(defn treat-node
  [node shape-id]
  (map-indexed #(assoc %2 :shape-id shape-id :index %1) node))

(defn extract-text-colors
  [text]
  (let [content (txt/node-seq txt/is-text-node? (:content text))
        content-filtered (map :fills content)
        indexed (mapcat #(treat-node % (:id text)) content-filtered)]
    (map text->color-att indexed)))

(defn get-colors-and-props-shape
  [list shape]
  (let [fill-obj   (map-indexed #(assoc %2 :shape-id (:id shape) :index %1) (:fills shape))
        stroke-obj (map-indexed #(assoc %2 :shape-id (:id shape) :index %1) (:strokes shape))]
    (if (= :text (:type shape))
      (-> list
          (into (map stroke->color-att) stroke-obj)
          (into (extract-text-colors shape)))

      (-> list
          (into (map fill->color-att)  fill-obj)
          (into (map stroke->color-att) stroke-obj)))))

(defn update-color-prop
  [element new-color]
  (assoc element :color-prop new-color))

(defn update-grouped-colors
  [grouped-colors new-color old-color]
  (let [changed-key (clojure.set/rename-keys grouped-colors {old-color new-color})
        color-array (get changed-key new-color)
        changed-color-array (map #(update-color-prop % new-color) color-array)
        new-grouped (assoc changed-key new-color changed-color-array)]
    (prn "old" old-color)
    (prn "new" new-color)
    (println "changed-key" changed-key)
    (println "new-grouped" new-grouped)
    new-grouped))

(mf/defc color-selection-menu
  {::mf/wrap [#(mf/memo' % (mf/check-props ["shapes"]))]}
  [{:keys [type shapes] :as props}]
  (let [data (reduce get-colors-and-props-shape [] shapes)
        grouped-colors (group-by :color-prop data)
        colors (-> (mapv :color-prop data)
                   distinct)

        divided-colors (group-by #(some? (:id %)) colors)
        library-colors (get divided-colors true)
        not-library-colors (get divided-colors false)

        expand-lib-color (mf/use-state false)
        expand-color (mf/use-state false)

        color-alias (mf/use-var {})

        on-change (mf/use-callback
                   (mf/deps color-alias)
                   (fn [new-color old-color]
                     (let [alias (if (contains? @color-alias old-color)
                                   (get @color-alias old-color)
                                   old-color)
                           shapes-by-color (get grouped-colors old-color)]

                       (swap! color-alias assoc old-color new-color)
                       (st/emit! (dc/change-color-in-selected new-color shapes-by-color alias)))))

        on-detach (mf/use-callback
                   (mf/deps grouped-colors)
                   (fn [color]
                     (let [shapes-by-color (get @grouped-colors color)
                           new-color (-> color
                                         (assoc :id nil :file-id nil))]
                       (st/emit! (dc/change-color-in-selected new-color shapes-by-color color)))))

        select-only (mf/use-callback
                     (mf/deps grouped-colors)
                     (fn [color]
                       (let [shapes-by-color (get @grouped-colors color)
                             ids (into (d/ordered-set)  (map :shape-id shapes-by-color))]
                         (st/emit! (dwc/select-shapes ids)))))]

    (when (< 1 (count colors))
      [:div.element-set
       [:div.element-set-title
        [:span (tr "workspace.options.selection-color")]]
       [:div.element-set-content
        [:div.selected-colors
         [:*
          (for [[index color] (d/enumerate (take 3 library-colors))]
            [:& color-row {:key (dm/str "color-" index)
                           :color color
                           :index index
                           :on-detach on-detach
                           :select-only select-only
                           :on-change #(on-change % color)}])
          (when (and (false? @expand-lib-color) (< 3 (count library-colors)))
            [:div.expand-colors  {:on-click #(reset! expand-lib-color true)}
             [:span i/actions]
             [:span.text (tr "workspace.options.more-lib-colors")]])
          (when @expand-lib-color
            (for [[index color] (d/enumerate (drop 3 library-colors))]
              [:& color-row {:key (dm/str "color-" index)
                             :color color
                             :index index
                             :on-detach on-detach
                             :select-only select-only
                             :on-change #(on-change % color)}]))]]

        [:div.selected-colors
         [:*
          (for [[index color] (d/enumerate (take 3 not-library-colors))]
            [:& color-row {:key (dm/str "color-" index)
                           :color color
                           :index index
                           :select-only select-only
                           :on-change #(on-change % color)}])
          (when (and (false? @expand-color) (< 3 (count not-library-colors)))
            [:div.expand-colors  {:on-click #(reset! expand-color true)}
             [:span i/actions]
             [:span.text (tr "workspace.options.more-colors")]])
          (when @expand-color
            (for [[index color] (d/enumerate (drop 3 not-library-colors))]
              [:& color-row {:key (dm/str "color-" index)
                             :color color
                             :index index
                             :select-only select-only
                             :on-change #(on-change % color)}]))]]]])))
