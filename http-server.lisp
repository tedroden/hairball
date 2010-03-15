;; based on code I found: http://jsnell.iki.fi/tmp/echo-server.lisp
;(in-package "sb-bsd-sockets")

(defvar *default-http-port* 9021)
(defvar *socket-listen-ip* #(127 0 0 1))
(defvar *socket-listen-backlog* 500)
(defvar *read-buffer-size* 8)
(defvar *octet* '(unsigned-byte 8))

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
   (request-headers :initform (make-hash-table)
					:accessor request-headers)
   (response-headers :initform (make-hash-table))

   ))


(defmethod response-headers ((x http-connection))
  (slot-value x 'response-headers))

(defmacro response-header (conn header)
  `(gethash ,header (response-headers ,conn)))

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

	(multiple-value-bind
		  (second minute hour date month year day-of-week dst-p tz)
		(get-decoded-time)
	  (declare (ignore dst-p))
	  (setf (response-header conn "Date")
			(format nil "Date: ~a, ~d ~a ~d ~2,'0d:~2,'0d:~2,'0d GMT~@d" 
					(nth day-of-week *short-day-names*) 
					date 
					(nth month *short-month-names*)
					year
					hour minute second
					(- tz))))
	
	
	(setf (response-header conn "Server") 
		  (format nil "Server: Hairball/~a" *http-server-version*))

	(setf (response-header conn "X-Powered-By")
		  "X-Powered-By: Rolaids, coffee, and beer")

	;; print the response header
	;; FIXME: why is this 200? Could be a 404 or something, right?
	(format (slot-value conn 'stream) "HTTP/1.0 200 OK~%")
	
	;; print the headers
	(loop for k being the hash-key using (hash-value v) of (response-headers conn)
	  	   do (format (slot-value conn 'stream) "~a: ~a~%" k v))
	
	;; print the data
	(format (slot-value conn 'stream) "~%~a" data)

	(http-networking-cleanup conn)))


;; sevrve a static file ... no prasing at all!
(defun finish-static-file (conn static-file)
  (format t "printing static file: ~a~%" static-file)
  (let ((in (open static-file :if-does-not-exist nil :element-type 'character)))
	(when in 
	  (loop with buf = (make-array 1024 :element-type 'character)
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


(defun hairball-get-headers (conn)
	;; get the request and headers...
	(with-output-to-string (out)
;	  (set 'request-string "")
	  (let ((sock (slot-value conn 'socket))
			(request-string "")
			(final-headers (make-hash-table))
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
			(declare (ignore len))
			(declare (ignore raddr))
			(loop 
			   for char across buf

			   do 
				 (if (equal #\Newline char)
					  (let ((trimmed-header (string-trim '(#\Space #\Tab #\Newline #\Return #\Linefeed) current-header)))
						(progn
						  ;; if we have an empty header, we're dont with headers
						  (if (= 0 (length trimmed-header))
							  (progn 
								(loop for header across headers
								   do (let ((h (string header)))
										(setf (gethash (subseq h 0 (search ": " h)) final-headers) 
											  (subseq h (+ 2 (search ": " h))))))
							  
								(return-from hairball-get-headers (values request-string final-headers))))
							
						  ;; Do we have the request string yet?
						  (if (not (plusp (length request-string)))
							  (progn 
								(setf request-string (copy-seq current-header))
								(setf (fill-pointer current-header) 0))
							  
							  ;; otherwise, we have the request string, setup this header
							  (progn 
								(vector-push-extend (copy-seq trimmed-header) headers)
								(setf (fill-pointer current-header) 0)))))
						
					  ;; othwerise we're not a newline... FIXME: remove this newline check?
					  (if (not (equal #\Return char)) 
						  (vector-push-extend char current-header)))

			))))))
  
(defun make-http-responder (conn)
 (lambda (_)
   (declare (ignore _))

	;; get the request and headers...
	(multiple-value-bind (rs headers)
		(hairball-get-headers conn)

	  (setf (slot-value conn 'request-method) (subseq rs 0 (search " " rs)))
	  (setf (slot-value conn 'request-uri) 
			(subseq rs (+ 1 (search " " rs)) (+ 1 (search " HTTP/1." rs))))
	  (setf (slot-value conn 'request-headers) headers)

	  (when (string= "GET" (slot-value conn 'request-method))
		;; get the GET request parameters
		(let ((uri (slot-value conn 'request-uri)))
		  (if (search "?" uri)
			  (let ((key-values (split-by-char 
								 (subseq uri (+ 1 (search "?" uri))) 
								 #\&)))
				;; we have the "x=y" and "z=hello+mom+let's+eat+cheese"
				;; turn those into parameters
				(dolist (x key-values)
				  (let ((key   (subseq x 0 (search "=" x)))
						(value (subseq x (+ 1 (search "=" x)) (length x))))
					(setf (gethash key (slot-value conn 'get-params)) 
						  (urldecode value))))))))

	  ;; (format t "request-method: ~a~%" (slot-value conn 'request-method))
	  ;; (format t "request-uri: ~a~%" (slot-value conn 'request-uri))

	  ;; ;; dump parameters
	  ;; (loop for k being the hash-key using (hash-value v) of (slot-value conn 'get-params)
	  ;; 	   do (format t "-- ~a => ~a~%" k v))
	  
	  ;; ;; dump headers
	  ;; (loop for k being the hash-key using (hash-value v) of (slot-value conn 'request-headers)
	  ;; 	   do (format t "-- ~a => ~a~%" k v))

	  (when (string= "GET" (slot-value conn 'request-method))
		(funcall (slot-value conn 'handler) conn)))))

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