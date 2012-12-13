(ns hello-world.core
  (:use [ring.adapter.jetty]
        [ring.middleware.reload]
        [ring.util.response]
        [ring.middleware.session]))

(defn handler [{session :session}]
  (let [count   (:count session 0)
        session (assoc session :count (inc count))]
    (-> (response (str "You accessed this page " count " times."))
        (assoc :session session))))

(def app
  (-> handler
      (wrap-reload)
      (wrap-session)))

(run-jetty app {:port 3000})
