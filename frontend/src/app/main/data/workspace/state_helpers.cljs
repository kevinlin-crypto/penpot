;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.workspace.state-helpers
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.pages.helpers :as cph]))

(defn lookup-page
  ([state]
   (lookup-page state (:current-page-id state)))
  ([state page-id]
   (get-in state [:workspace-data :pages-index page-id])))

(defn lookup-page-objects
  ([state]
   (lookup-page-objects state (:current-page-id state)))
  ([state page-id]
   (get-in state [:workspace-data :pages-index page-id :objects])))

(defn lookup-page-options
  ([state]
   (lookup-page-options state (:current-page-id state)))
  ([state page-id]
   (get-in state [:workspace-data :pages-index page-id :options])))

(defn lookup-component-objects
  ([state component-id]
   (get-in state [:workspace-data :components component-id :objects])))

(defn lookup-local-components
  ([state]
   (get-in state [:workspace-data :components])))

;; TODO: improve performance of this
(defn lookup-selected
  ([state]
   (lookup-selected state nil))
  ([state options]
   (lookup-selected state (:current-page-id state) options))
  ([state page-id {:keys [omit-blocked?] :or {omit-blocked? false}}]
   (let [objects  (lookup-page-objects state page-id)
         selected (->> (dm/get-in state [:workspace-local :selected])
                       (cph/clean-loops objects))
         selectable? (fn [id]
                       (and (contains? objects id)
                            (or (not omit-blocked?)
                                (not (get-in objects [id :blocked] false)))))]
     (into (d/ordered-set)
           (filter selectable?)
           selected))))

(defn lookup-shapes
  ([state ids]
   (lookup-shapes state (:current-page-id state) ids))
  ([state page-id ids]
   (let [objects (lookup-page-objects state page-id)]
     (into [] (keep (d/getf objects)) ids))))

(defn filter-shapes
  ([state filter-fn]
   (filter-shapes state (:current-page-id state) filter-fn))
  ([state page-id filter-fn]
   (let [objects (lookup-page-objects state page-id)]
     (into [] (filter filter-fn) (vals objects)))))

(defn get-local-file
  "Get the data content of the file you are currently working with."
  [state]
  (get state :workspace-data))

(defn get-file
  "Get the data content of the given file (it may be the current file
  or one library)."
  [state file-id]
  (if (= file-id (:current-file-id state))
    (get state :workspace-data)
    (get-in state [:workspace-libraries file-id :data])))

(defn get-libraries
  "Retrieve all libraries, including the local file."
  [state]
  (let [{:keys [id] :as local} (:workspace-data state)]
    (-> (:workspace-libraries state)
        (assoc id {:id id
                   :data local}))))
