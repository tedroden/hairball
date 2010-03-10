;; to run this, start "sbcl"
;; sbcl> (load "group-chat.lisp")

;; then point your browser(s) to http://localhost:9021
;;

(require :asdf)

(setf asdf:*central-registry*
   ;; Default directories, usually just the ``current directory''
  '(*default-pathname-defaults*

    ;; Additional places where ASDF can find
    ;; system definition files
    #p"/Users/troden/Dropbox/Documents/Code/hairball/"))

(asdf:oos 'asdf:load-op 'hairball)
; (in-package 'hairball)


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
			  (finish-http-response x message-body))))

	;; response with the message... probably ignore by the client
	(finish-http-response conn message-body)))



(defun handle-send (conn) 
  (send-message 
   conn
   (gethash "from" (slot-value conn 'get-params))
   (gethash "body" (slot-value conn 'get-params))))

(defun handle-404 (conn)
  (finish-http-response conn "404!"))

(defun handle-static-js (conn)
  (finish-static-file conn "static/group-chat.js"))

(defun handle-static-home (conn)
  (finish-static-file conn "static/group-chat.html"))

(defun handle-static-css (conn)
  (finish-static-file conn "static/group-chat.css"))

(defun handle-request (conn) 
  (let ((handled nil))

	;; loop through the URL handlers
	(dolist (map *url-handlers*)
	  ;; when we find the url mapping call the function
	  (when (eq 0 (search (car map) (slot-value conn 'request-uri)))

		;; fixme: there must be a better way to get the last/second item 
		;; (cdr returns the rest)
		(funcall (second map) conn)
		
		(setq handled t)
		(return)))

	;; FIXME: i don't think we should have to keep track of this here.
	(when (not handled)
	  (format t "Handling 404")
			  (handle-404 conn))))

;; setup the server and our handler
(http-server :request-handler 'handle-request)