(ns user
  (:require [emmy.clerk :as ec]
            [emmy.expression.render :as xr]
            [mentat.clerk-utils.css :as css]
            [nextjournal.clerk.config :as cc]))

(try (requiring-resolve 'cljs.analyzer.api/ns-resolve) (catch Exception _ nil))
(require '[emmy.env])
(require '[emmy.expression.render :as xr])

(alter-var-root
 #'xr/*TeX-vertical-down-tuples*
 (constantly true))

(alter-var-root
 #'cc/*bounded-count-limit*
 (constantly 2))

(css/set-css!
 ;; mafs
 "https://unpkg.com/computer-modern@0.1.2/cmu-serif.css"
 "https://unpkg.com/mafs@0.15.2/core.css"
 "https://unpkg.com/mafs@0.15.2/font.css"

 ;; JSXGraph
 "https://cdn.jsdelivr.net/npm/jsxgraph@1.5.0/distrib/jsxgraph.css"

 ;; mathbox
 "https://unpkg.com/mathbox@2.3.1/build/mathbox.css"

 ;; mathlive
 "https://unpkg.com/mathlive@0.85.1/dist/mathlive-static.css"
 "https://unpkg.com/mathlive@0.85.1/dist/mathlive-fonts.css")


(def serve-defaults
  {:port 7777
   :watch-paths ["notebooks"]
   :browse? true})

(def static-defaults
  {:browse? false
   :paths ["notebooks/information/ch1.clj"]
   :git/url "https://github.com/eightysteele/prior"})

(defn serve!
  "Alias of [[emmy.clerk/serve!]] with [[defaults]] supplied as default arguments.

  Any supplied `opts` overrides the defaults." 
  ([] (serve! {}))
  ([opts]
   (ec/serve!
    (merge serve-defaults opts))))

(def ^{:doc "Alias for [[emmy.clerk/halt!]]."}
  halt!
  ec/halt!)

(defn build!
  "Alias of [[emmy.clerk/build!]] with [[static-defaults]] supplied as default
  arguments.

  Any supplied `opts` overrides the defaults."
  [opts]
  (ec/build!
   (merge static-defaults opts)))

