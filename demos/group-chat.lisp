;; to run this, start "sbcl"
;; sbcl> (load "group-chat.lisp")

;; then point your browser(s) to http://localhost:9021
;;

(require "http-server.lisp" "../http-server.lisp")

(defvar *listeners* (make-array 0
								:element-type 'standard-class
								:fill-pointer 0
								:adjustable t))

;; urls and the functions that handle them
(defvar *url-handlers*
  '(("/updates" handle-updates)
	("/send"    handle-send)
	("/js"      handle-static-js)
	("/css"     handle-static-css)
	("/"        handle-static-home)))


(defun handle-updates (conn)
  (vector-push-extend conn *listeners*))

(defun send-message (conn from body)
  
  (let ((message-body (format nil "~a~%~a" from body)))

	;; if there are listeners, loop through them
	(when (> (fill-pointer *listeners*) 0)
	  (loop for i from 1 to (fill-pointer *listeners*)
		 do (let ((x (vector-pop *listeners*)))
			  ;; send the message to each of of them
			  (http-server::finish-http-response x message-body))))

	;; response with the message... probably ignore by the client
	(http-server::finish-http-response conn message-body)))



(defun handle-send (conn) 
  (send-message 
   conn
   (gethash "from" (slot-value conn 'http-server::get-params))
   (gethash "body" (slot-value conn 'http-server::get-params))))

(defun handle-404 (conn)
  (http-server::finish-http-response conn "404!"))

(defun handle-static-js (conn)
  (http-server::finish-static-file conn "static/group-chat.js"))

(defun handle-static-home (conn)
  (http-server::finish-static-file conn "static/group-chat.html"))

(defun handle-static-css (conn)
  (http-server::finish-static-file conn "static/group-chat.css"))

(defun handle-request (conn) 
  (let ((handled nil))

	;; loop through the URL handlers
	(dolist (map *url-handlers*)
	  ;; when we find the url mapping call the function
	  (when (eq 0 (search (car map) (slot-value conn 'http-server::request-uri)))

		;; fixme: there must be a better way to get the last/second item 
		;; (cdr returns the rest)
		(funcall (car (cdr map)) conn)
		
		(setq handled t)
		(return)))

	;; FIXME: i don't think we should have to keep track of this here.
	(when (not handled)
	  (format t "Handling 404")
			  (handle-404 conn))))

;; setup the server and our handler
(http-server::http-server :request-handler 'handle-request)