(ns user
  (:require [mount.core :as mount]
            gnostica.core))

(defn start []
  (mount/start-without #'gnostica.core/repl-server))

(defn stop []
  (mount/stop-except #'gnostica.core/repl-server))

(defn restart []
  (stop)
  (start))


