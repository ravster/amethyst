;; Copyright 2012 Ravi Desai
;; Distributed under the GNU Affero General Public License version 3 or later.

(asdf:defsystem "20120329"
  :description "Random-entry trade simulator with following routine for active trades."
  :author "Ravi Desai <rd7190@gmail.com>"
  :components ((:file "defpackage")
	       (:file "20120329" :depends-on ("defpackage")))
  :depends-on (:lucifer))
