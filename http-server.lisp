;; based on code I found: http://jsnell.iki.fi/tmp/echo-server.lisp
;(in-package "sb-bsd-sockets")

(defvar *default-http-port* 9021)
(defvar *socket-listen-ip* #(127 0 0 1))
(defvar *socket-listen-backlog* 500)
(defvar *read-buffer-size* 8)
(defvar *octet* '(unsigned-byte 8))

;; ;; from hunchentoot
;; (defvar *crlf*
;;   (make-array 2 :element-type '(unsigned-byte 8)
;;               :initial-contents (mapcar 'char-code '(#\Return #\Linefeed))))


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
   (get-params :accessor get-params :initform (make-hash-table :test 'equal))
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
  (let ((fd (sb-bsd-sockets::socket-file-descriptor (slot-value conn 'socket))))
	;;	(format t "~a: http-networking-cleanup~%" (slot-value conn 'id))
	(sb-impl::invalidate-descriptor fd)
	(sb-bsd-sockets::socket-close (slot-value conn 'socket))
	;; free the conn variable?
	))

(defun finish-http-response (conn data)

  ;; we we still connected?
  (when (sb-bsd-sockets:socket-open-p (slot-value conn 'socket))

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

	(http-networking-cleanup conn)))


;; sevrve a static file ... no prasing at all!
(defun finish-static-file (conn static-file)
  (format t "printing static file: ~a~%" static-file)
  (let ((in (open static-file :if-does-not-exist nil :element-type *octet*)))
	(when in 
	  (loop with buf = (make-array 1024 :element-type *octet*)
		   for pos = (read-sequence buf in)
		   until (zerop pos)
		   do (write-sequence buf (slot-value conn 'stream) :end pos)
		   (finish-output (slot-value conn 'stream))))
  
	(close in)
	(http-networking-cleanup conn)))

;; (defun finish-static-file (conn static-file)
;;   (let ((in (open static-file :if-does-not-exist nil :element-type '(unsigned-byte 8))))
;; 	(let ((buf (make-array 4096 :element-type (stream-element-type in))))
;; 	  (loop for pos = (read-sequence buf in )
;; 		 while (plusp pos)
;; 		 do (write-sequence buf (slot-value conn 'stream) :end pos)))
;; 	(close in)
;; 	(http-networking-cleanup conn)))



;; this if hte first http responder. we want to wait for the theaders
;; (defun make-http-responder (conn)
;;   (lambda (_)
;;     (declare (ignore _))
;;     (handler-case
;; 		(loop
;; 		  (let ((line (read-line (slot-value conn 'stream)))
;; 				(in-body nil))
			
;; 			;; FIXME: we know the http method/version is on the frist line. get it there.
;; 			(setf line (subseq line 0 (1- (length line))))
			
;; 			(cond 
;; 			  ;; did we find the request line?
;; 			  ((search " HTTP/1." line)
;; 			   (setf (slot-value conn 'request-method) (subseq line 0 (search " " line)))
;; 			   (setf (slot-value conn 'request-uri) 
;; 					 (subseq line (+ 1 (search " " line)) (search " HTTP/1.1" line)))

;; 			   ;; get the GET request parameters
;; 			   (let ((uri (slot-value conn 'request-uri)))
;; 				 (if (search "?" uri)
;; 					 (let ((key-values (split-by-char 
;; 										(subseq uri (+ 1 (search "?" uri))) 
;; 										#\&)))
;; 					   ;; we have the "x=y" and "z=hello+mom+let's+eat+cheese"
;; 					   ;; turn those into parameters
;; 					   (dolist (x key-values)
;; 						 (let ((key   (subseq x 0 (search "=" x)))
;; 							   (value (subseq x (+ 1 (search "=" x)) (length x))))
;; 						   (setf (gethash key (slot-value conn 'get-params)) 
;; 								 (urldecode value)))))))

;; 			   ) ;; should match up to ((search " HTTP ...

;; 			  ;; do we know? are we in the body?
;; 			  (in-body
;; 			   (format t "body: '~a'~%" line))

;; 			  ;; ahh, end of headers
;; 			  ((string= line "")

;; 			   ;; fixme: clean this up to be something cleaner like (method-is-get conn)
;; 			   (when (string= "GET" (slot-value conn 'request-method))
;; 				 (funcall (slot-value conn 'handler) conn)
;; 				 (return))

;; 			   ;; we'll use this when we support post, etc
;; 			   ;; not used now
;; 			   (format t "entering body of request~%")
;; 			   (setq in-body t)
;; 			   )
;; 			  (t
;; 			   (vector-push-extend line (slot-value conn 'request-headers) )
;; 			   ))))
;;       (end-of-file ()
;; 		(http-networking-cleanup conn)))))

(defun hairball-readline (conn)
  (with-output-to-string (out)
	(let ((sock (slot-value conn 'socket))
		  (buffer (make-array *read-buffer-size*
							:element-type 'character
							:adjustable nil
							:fill-pointer t)))
	  (setf (fill-pointer buffer) *read-buffer-size*)
	  (do ((done nil))
		  (done t)
		(multiple-value-bind (buf len raddr)
			(sb-bsd-sockets:socket-receive sock buffer nil)
		  (format t "Recved ~a bytes" len)
		  (format t buffer))))))
  

(defun make-http-responder (conn)
  (lambda (_)
    (declare (ignore _))
	;; get the request and headers...
	(with-output-to-string (out)

	  (let ((sock (slot-value conn 'socket))
			(headers (make-array 0 
								:element-type 'string
								:fill-pointer 0
								:adjustable t))
			(current-header (make-array 0
										:element-type 'character
										:fill-pointer 0
										:adjustable t))
			(buffer (make-array *read-buffer-size*
								:element-type 'character
								:adjustable nil
								:fill-pointer t)))

		(setf (fill-pointer buffer) *read-buffer-size*)
		(do ((done nil))
			(done t)
		  (multiple-value-bind (buf len raddr)
			  (sb-bsd-sockets:socket-receive sock buffer nil)
;;			(declare (ignore raddr))
			
			(loop 
			   for char across buf
			   do (if (equal #\Newline char)
					  (let ((trimmed-header (string-right-trim '(#\Space #\Tab #\Newline #\Return #\Linefeed) current-header))) 
						
						(if (= 0 (length trimmed-header))
							(progn 
							  (format t "DONE!!!!!!~%")
							  (princ headers)
							  (return headers)))
						
							
						;; FIXME: there has to be a better way
						;;        to copy a string onto the array
						(vector-push-extend (format nil "~a" trimmed-header) headers)
						
						(setf (fill-pointer current-header) 0))
					  (progn 
						(if (not (equal #\Linefeed char))
							(vector-push-extend char current-header)))))

			))))))

  

  
;;   (let ((buffer (make-array *read-buffer-size*
;; 							:element-type 'character
;; 							:adjustable nil
;; 							:fill-pointer t))
;;         (sock (slot-value conn 'socket)))

;; 	(do ((fin nil))
;; 		(fin t)

;; 	  (setf (fill-pointer buffer) *read-buffer-size*)
;;       (multiple-value-bind (buf len raddr)
;;           (sb-bsd-sockets:socket-receive sock buffer nil)
;;         (declare (ignore raddr))
;;         (if (null buf)
;;             (setf fin t)
;;             (setf (fill-pointer buffer) len)))
;;       (cond ((= (length buffer) 0)
;;              (format t "  Got 0 bytes, closing socket and removing handler~%")
;;              (sb-bsd-sockets:socket-close sock)
;; ;             (sb-sys:remove-fd-handler (server-session-handler session))
;;              (setf fin t))
;;             (fin (format t "Got NIL, returning~%"))
;;             (t
;;              (format t "  Read ~a bytes: ~a~%" (length buffer) buffer))))))


	  
(defun serve (conn)
  (let ((stream (sb-bsd-sockets::socket-make-stream (slot-value conn 'socket) 
				 :output t 
				 :input t
				 :element-type 'character
				 :buffering :none
				 ))
;;				 :element-type *octet*))

        (fd (sb-bsd-sockets::socket-file-descriptor (slot-value conn 'socket))))

	(setf (slot-value conn 'stream) stream)
    (sb-impl::add-fd-handler fd :input (make-http-responder conn))))

(defun http-server (&key request-handler (port *default-http-port*))
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (counter 0))
	(format t "Using port ~a~%" port)
    (sb-bsd-sockets::socket-bind socket *socket-listen-ip* port)
    (sb-bsd-sockets::socket-listen socket *socket-listen-backlog*)
	(format t "Server Running...~%")
    (sb-impl::add-fd-handler (sb-bsd-sockets::socket-file-descriptor socket)
                             :input
                             (lambda (_)
                               (declare (ignore _))
							   (setf (sb-bsd-sockets:non-blocking-mode socket) t)
                               (incf counter)
;;                               (format t "Accepted client ~A~%" counter)
                               (serve (make-instance 'http-connection :socket (sb-bsd-sockets::socket-accept socket) :id counter :handler request-handler))))))


(provide 'http-server)