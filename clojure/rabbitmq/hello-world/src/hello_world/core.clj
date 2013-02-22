(ns hello-world.core
  (:require (langohr [core :as rmq]
                     [channel :as lch]
                     [queue :as lq]
                     [consumers :as lc]
                     [basic :as lb])))

;;; The exchange with no name is provided by rabbit.  When a queue is made, it is bound to this exchange through the routing key that has the same value as the name of the queue.  It allows us to pretend like we are sending a message straight into a queue instead of through an exchange.
;;; Use this when you don't have complicate processing involved to decide where the message goes.
(def ^{:const true}
  default-exchange-name "")

(defn message-handler
  [ch {:keys [content-type delivery-tag type] :as meta} ^bytes payload]
  (println (format "[consumer] received message: %s, delivery tag: %d, content type %s, type: %s"
                   (String. payload "UTF-8") delivery-tag content-type type)))

(defn start-consumer
  [conn ch queue-name]
  (let [thread (Thread. (fn []
                          (lc/subscribe ch queue-name message-handler :auto-ack true)))]
    (.start thread)))

;;; The rules used to move a message from an exchange to a queue is called a binding.
;;; Explicitly bind a queue to an exchange: (lq/bind channel queue-name exchange-name :routing-key key<string>) . Names are strings.

(defn -main
  [& args]
  (let [conn (rmq/connect)              ;Actual connection to rabbit.
        ch (lch/open conn)              ;A channel is how the rest of the program connects to rabbit.  Rest of program doesn't know about connections.  This means that only one tcp connection is made.  Helps with firewalls and stuff.  The connection is multiplexed through different channels.
        qname "helloworld-example-langohr"]
    (println "Main connected.  Channel id: " (.getChannelNumber ch))
    ; (lq/declare channel queuename<string> & queue-attributes)
    (lq/declare ch qname :exclusive false :auto-delete true) ;Declare a queue for a channel.  This will not create a new one if it already exists.
    (start-consumer ch qname)
    ; (lb/publish channel exchange<string> routing-key<string> message & message-attrs)
    (lb/publish ch default-exchange-name qname "Hello!" :content-type "text/plain" :type "greetings.hi") ;Publish a message to an exchange.
    (Thread/sleep 2000)
    (println "Disconnecting...")
    ; close is a multi-method, not a function.  Clojure's equivalent of CL's generic functions.
    (rmq/close ch)
    (rmq/close conn)))
