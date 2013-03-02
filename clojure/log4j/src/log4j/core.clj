(ns log4j.core
  (:require [clojure.tools.logging :refer [info error]]))

(defn divide [x y]
  (try
    (info "dividing" x "by" y)
    (/ x y)
    (catch Exception ex
      (error ex "There was an error in calculation"))))
