(ns datomic-simple-experiments
  (:require
   [datomic-simple.core :as ds]))

(def user-namespace :user)

(def schema (ds/build-schema user-namespace
                             [[:username :string]
                              [:password :string]]))

(ds/start {:schemas [schema]})

(ds/create-model-fns user-namespace)

(create {:username "foo" :password "bar"})

(find-all {:username "foo"})

(find-first {:username "foo"})
