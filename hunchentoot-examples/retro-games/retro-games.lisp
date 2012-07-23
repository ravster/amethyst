;;;; This is an updated version of the program found at http://www.adampetersen.se/articles/lispweb.htm
;; Its got the Hunchentoot stuff working, but the Elephant stuff still needs to be placed in here.

;; You will have to load cl-who, hunchentoot, and parenscript into your lisp image for this to work.
;; Use Quicklisp
(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :retro-games)

(defclass game ()
  ((name :initarg :name
	 :reader name)
   (votes :initform 0
	  :accessor votes))
  (:documentation "Each game has two things that define it."))

(defvar *games* nil
  "A list of GAME objects, one for each game.")

(defun game-from-name (name)
  "Don't access the games directly, use a function in-between.  Keep things modular and easy to change.  Return a GAME object."
  (find name *games*
	:test #'string-equal
	:key #'name))

(defmethod vote-for (name-of-game)
  "Increase the vote-count for a particular game named by NAME by 1."
  (incf (votes (game-from-name name-of-game))))

;; Suppose we don't want the game, but just want to know if the game is already there?  We can use GAME-FROM-NAME, but this is better.
;; We want to be clear with our intent, so we use this.
(defun game-stored? (game-name)
  "Not necessary, but its a seperate way to know if there is already a game with the given name."
  (game-from-name game-name))

;; We want to display the games in descending order of popularity (Measured by votes), so we do the following.
(defun games ()
  "Return a copy of the sorted (by popularity) list of games."
  (sort (copy-list *games*)
	#'>
	:key #'votes))

;; To continue keeping things encapsulated, we have a function to add games to the list.  This function is allowed to know how the data is implemented.
(defun add-game (name)
  "Use UNLESS to make sure that we aren't adding something that is already there."
  (unless (game-stored? name)
    (push (make-instance 'game :name name)
	  *games*)))

;;;; HTML stuff

;; Reason: We don't want to have to type all of this everytime we make a webpage.  This takes care of it for us.
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

;; Start the hunchentoot listener.  This is the thing that listens for incoming HTTP requests at the specified port.
(defparameter hunchentoot-listener (make-instance 'hunchentoot:easy-acceptor :port 4242))
;; Note: I'm using the EASY-ACCEPTOR version of this.  There is a more general one that a power user can use to tweak things.
(start hunchentoot-listener)

;; We now make the handler that takes care of a particular request.
;; In the original post, we had to manually make a handler that was manually put into a *dispatch-table* which would call a particular function if a particular URL was called.  And then later in the blog post we make a macro that does all of that for us.
;; The DEFINE-EASY-HANDLER macro does all of this for us, so we don't have to worry about that and can focus on making our website.
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

;; The logic for this handler is the same as the one in the blog-post.
(hunchentoot:define-easy-handler (vote :uri "/vote") (name)
  "If a game of NAME exists, VOTE-FOR it, and then pass the client back to the retro-games main page."
  (if (game-stored? name)
      (vote-for name))
  (redirect "/retro-games"))

(hunchentoot:define-easy-handler (new-game :uri "/new-game") ()
  "Page that accepts a new game to be put on the list."
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
  "If the name is valid, add the game to the list of games."
  (unless (or (null name)
	      (zerop (length name)))
    (add-game name))
  (redirect "/retro-games"))
