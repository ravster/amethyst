;; Copyright 2011 Ravi Desai
;; Distributed under the GNU Affero General Public License version 3 or later.

(asdf:defsystem "random-entry"
  :description "Random-entry trade simulator"
  :author "Ravi Desai <rd7190@gmail.com>"
  :components ((:file "defpackage")
	       (:file "random-entry" :depends-on ("defpackage")))
  :depends-on (:lucifer))
