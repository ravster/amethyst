;; Copyright 2012 Ravi Desai
;; Distributed under the GNU Affero General Public License version 3 or later.

(asdf:defsystem "dice-game"
  :description "Version of the dice game on codeacademy's website."
  :author "Ravi Desai <rd7190@gmail.com>"
  :serial t
  :components ((:file "defpackage")
	       (:file "dice-game"))
  :depends-on (:hunchentoot))
