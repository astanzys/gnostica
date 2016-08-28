(ns gnostica.routes.websockets
  (:require [compojure.core :refer [GET defroutes wrap-routes]]
            [clojure.tools.logging :as log]
            [immutant.web.async       :as async]))

(defonce channels (atom #{}))

(defn disconnect! [channel {:keys [code reason]}]
  (log/info "close code:" code "reason:" reason)
  (swap! channels #(remove #{channel} %)))

(defn send-message
  [channel msg]
  (async/send! channel msg))

(defn broadcast [channel msg]
  (doseq [channel @channels]
    (send-message channel msg)))

(defn connect! [channel]
  (log/info "channel open")
  (swap! channels conj channel)
  (async/send! channel "Welcome! Type /help to see available commands"))

(def websocket-callbacks
  "WebSocket callback functions"
  {:on-open connect!
   :on-close disconnect!
   :on-message broadcast})

(defn ws-handler [request]
  (async/as-channel request websocket-callbacks))

(defroutes websocket-routes
  (GET "/ws" [] ws-handler))



