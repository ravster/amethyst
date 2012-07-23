(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :retro-games)

(defclass game ()
  ((name :initarg :name
	 :reader name)
   (votes :initform 0
	  :accessor votes))
  (:documentation "Each game has two things that define it."))

(defmethod vote-for (name-of-game)
  (incf (votes (game-from-name name-of-game))))

(defvar *games* nil)

(defun game-from-name (name)
  "Don't access the games directly, use a function in-between.  Keep things modular and easy to change."
  (find name *games*
	:test #'string-equal
	:key #'name))

;; Suppose we don't want the game, but just want to know if the game is already there?  We can use GAME-FROM-NAME, but this is better.
;; We want to be clear with our intent, so we use this.
(defun game-stored? (game-name)
  "Not necessary, but its a seperate way to know if there is already a game with the given name."
  (game-from-name game-name))

;; We want to display the games in descending order of popularity, so we do the following.
(defun games ()
  "Return a copy of the sorted (by popularity) list of games."
  (sort (copy-list *games*)
	#'>
	:key #'votes))

;; To continue keeping things encapsulated, we have a function to add games to the list.  This function is allowed to know how the data is implemented.
(defun add-game (name)
  (unless (game-stored? name)
    (push (make-instance 'game :name name)
	  *games*)))

;;;; HTML stuff

(defmacro standard-page ((&key title) &body body)
  "Abstract away the placing of standard page stuff."
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml"
	    :xml\:lang "en" 
	    :lang "en"
	    (:head 
	     (:meta :http-equiv "Content-Type" 
		    :content    "text/html;charset=utf-8")
	     (:title ,title)
	     (:link :type "text/css" 
		    :rel "stylesheet"
		    :href "/retro.css"))
	    (:body 
	     (:div :id "header"		; Retro games header
		   (:img :src "/logo.jpg" 
			 :alt "Commodore 64" 
			 :class "logo")
		   (:span :class "strapline" 
			  "Vote on your favourite Retro Game"))
	     ,@body))))

;; Start the hunchentoot listener
(defparameter hunchentoot-listener (make-instance 'hunchentoot:easy-acceptor :port 4242))
(start hunchentoot-listener)

;; We now make the handler that takes care of a particular request.
(hunchentoot:define-easy-handler (retro-games :uri "/retro-games") ()
    (standard-page
     (:title "Retro Games")
     (:h1 "Vote on your all time favourite retro games!")
     ;; A link to add a game to the list.
     (:p "Missing a game? Make it available for votes " (:a :href "new-game" "here"))
     (:h2 "Current stand")
     (:div :id "chart"			; For CSS styling of links
	   (:ol
	    ;; A list of all the games so far.
	    (dolist (game (games))
	      (htm  
	       (:li 
		(:a :href (format nil "vote?name=~a" (name game)) "Vote!")
		(fmt "~A with ~d votes" (name game) (votes game)))))))))

(hunchentoot:define-easy-handler (vote :uri "/vote") (name)
  "If a game of NAME exists, VOTE-FOR it, and then pass the client back to the retro-games main page."
  (if (game-stored? name)
      (vote-for name))
  (redirect "/retro-games"))

(hunchentoot:define-easy-handler (new-game :uri "/new-game") ()
  (standard-page
      (:title "Add a new game")
    (:h1 "Add a new game tothe chart")
    (:form :action "/game-added" :method "post"
	   :onsubmit
	   (ps-inline
	    (when (= name "")
	      (alert "Please enter a name.")
	      (return false)))
	   (:p "What is the name of the game?" (:br)
	       (:input :type "text"  
		       :name "name" 
		       :class "txt"))
	   (:p (:input :type "submit" 
		       :value "Add" 
		       :class "btn")))))

(hunchentoot:define-easy-handler (game-added :uri "/game-added") (name)
  (unless (or (null name)
	      (zerop (length name)))
    (add-game name))
  (redirect "/retro-games"))
