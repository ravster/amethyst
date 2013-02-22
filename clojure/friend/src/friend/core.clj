(ns friend.core
  (:require [cemerick.friend :as friend]
            (cemerick.friend [workflows :as workflows]
                             [credentials :as creds])
            [compojure.core :refer :all]
            [compojure.route :as route]))

(defroutes app-routes
  (GET "/" [] "Hello World")
  (GET "/authorized" request
       (friend/authorize #{::user} "This page can only be seen by authenticated users."))
  (GET "/login" [] (ring.util.response/file-response "login.html" {:root "resources"})) ;Just serve an ordinary file
  (route/not-found "Not Found"))


;;; The interactive-form thing automatically creates a /login POST route.
(def app
  (handler/site
   (friend/authenticate app-routes {:credential-fn (partial creds/bcrypt-credential-fn users)
                                    :workflows [(workflows/interactive-form)]})))
