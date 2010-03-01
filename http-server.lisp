;; based on code I found: http://jsnell.iki.fi/tmp/echo-server.lisp

(require :sb-bsd-sockets)

(defpackage http-server
  (:use :cl :sb-bsd-sockets)
  (:export :http-server :http-connection))

(in-package http-server)

(defvar *default-http-port* 9021)
(defvar *socket-listen-ip* #(127 0 0 1))
(defvar *socket-listen-backlog* 5)

(defparameter *http-server-version* "0.0.1")

(defvar *http-methods* '("OPTIONS" "GET" "HEAD" "POST" "PUT" "DELETE" "TRACE" "CONNECT"))
(defvar *supported-http-methods* '("GET"))

(defvar *short-day-names* '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat""Sun"))
(defvar *short-month-names* '("Jan" "Feb" "Mar" "Apr" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))


(defclass http-connection ()
  ((id :initarg :id)
   (socket :initarg :socket)
   (handler :initarg :handler)
   stream
   (alive :initform t)
   (request-uri :accessor request-uri)
   (request-method :accessor request-method)
   (request-headers :initform (make-array 0 
										   :element-type 'string
										   :fill-pointer 0
										   :adjustable t)
					:accessor request-headers)
   (response-headers :initform (make-array 0 
										   :element-type 'string
										   :fill-pointer 0
										   :adjustable t))

   ))


(defun http-networking-cleanup (conn)
  (let ((fd (socket-file-descriptor (slot-value conn 'socket))))
	(format t "~a: http-networking-cleanup~%" (slot-value conn 'id))
	(sb-impl::invalidate-descriptor fd)
	(socket-close (slot-value conn 'socket))
	;; free the conn variable?
	))



(defun finish-http-response (conn data)
 ;; fixme: is this a 200 response?
  (vector-push-extend (format nil "HTTP/1.1 200 OK") (slot-value conn 'response-headers))
  (multiple-value-bind
		(second minute hour date month year day-of-week dst-p tz)
	  (get-decoded-time)
	(format nil "~a" dst-p)
	(vector-push-extend
	 (format nil "Date: ~a, ~d ~a ~d ~2,'0d:~2,'0d:~2,'0d GMT~@d" 
			 (nth day-of-week *short-day-names*) 
			 date 
			 (nth month *short-month-names*)
			 year
			 hour minute second
			 (- tz)) (slot-value conn 'response-headers)))
  
  (vector-push-extend 
   (format nil "Server: Hairball/~a" *http-server-version*) 
   (slot-value conn 'response-headers))

  (vector-push-extend 
   "X-Powered-By: Rolaids, coffee, and beer" 
   (slot-value conn 'response-headers))

  ;; print the headers
  (loop 
	 for header across (slot-value conn 'response-headers)
	 do (format (slot-value conn 'stream) "~a~%" header))

  (format (slot-value conn 'stream) "~%~a" data)

  (http-networking-cleanup conn))



;; this if hte first http responder. we want to wait for the theaders
(defun make-http-responder (conn)
  (lambda (_)
    (declare (ignore _))
    (handler-case
		(loop
		  (let ((line (read-line (slot-value conn 'stream)))
				(in-body nil))
			
			;; FIXME: we know the http method/version is on the frist line. get it there.
			(setf line (subseq line 0 (1- (length line))))
			
			(cond 
			  ;; did we find the request line?
			  ((search " HTTP/1.1" line)
			   (setf (slot-value conn 'request-method) (subseq line 0 (search " " line)))
			   (setf (slot-value conn 'request-uri) 
					 (subseq line (+ 1 (search " " line)) (search " HTTP/1.1" line))))

			  ;; do we know? are we in the body?
			  (in-body
			   (format t "body: '~a'~%" line))

			  ;; ahh, end of headers
			  ((string= line "")

			   ;; fixme: clean this up to be (method-is-get conn)
			   (when (string= "GET" (slot-value conn 'request-method))
				 (format t "GET IS DONE~%")
				 (funcall (slot-value conn 'handler) conn)
				 
				  (return))

			   (format t "entering body of request~%")
			   (setq in-body t)
			   ;; (funcall (slot-value conn 'handler) conn)
			   ;; (funcall disconnector)
			   ;; (return)
			   )
			  (t
			   (vector-push-extend line (slot-value conn 'request-headers) )
			   ))))
      (end-of-file ()
        (http-networking-cleanup conn)))))

	  
(defun serve (conn)
  (let ((stream (socket-make-stream (slot-value conn 'socket) :output t :input t))
        (fd (socket-file-descriptor (slot-value conn 'socket))))

	(setf (slot-value conn 'stream) stream)
	(format t "serving ~%")
    (sb-impl::add-fd-handler fd
                             :input
                             (make-http-responder conn))))

(defun http-server (&key request-handler (port *default-http-port*))
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp))
        (counter 0))
    (socket-bind socket *socket-listen-ip* port)
    (socket-listen socket *socket-listen-backlog*)
	(format t "Server Running...~%")
    (sb-impl::add-fd-handler (socket-file-descriptor socket)
                             :input
                             (lambda (_)
                               (declare (ignore _))
							   (setf (sb-bsd-sockets:non-blocking-mode socket) t)
                               (incf counter)
                               (format t "Accepted client ~A~%" counter)
                               (serve (make-instance 'http-connection :socket (socket-accept socket) :id counter :handler request-handler))))))

(provide 'http-server)