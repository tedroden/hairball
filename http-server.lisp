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
  (let ((fd (socket-file-descriptor (slot-value conn 'socket))))
	;;	(format t "~a: http-networking-cleanup~%" (slot-value conn 'id))
	(sb-impl::invalidate-descriptor fd)
	(socket-close (slot-value conn 'socket))
	;; free the conn variable?
	))

;; based on code at: 
;; http://cl-cookbook.sourceforge.net/strings.html#process
(defun split-by-char (string char)
    (loop for i = 0 then (1+ j)
          as j = (position char string :start i)
          collect (subseq string i j)
          while j))


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


;; sevrve a static file
(defun finish-static-file (conn static-file)
  (format t "printing static file: ~a~%" static-file)
  (let ((in (open static-file :if-does-not-exist nil)))
	(when in 
	  (loop for line = (read-line in nil)
		   while line do
		   (format (slot-value conn 'stream) "~a~%" line)))
	(close in)
	(http-networking-cleanup conn)))


(defparameter *char-mapping*
  '(
	(" " "+")
	(" " "%20")
	("!" "%21")
	("\"" "%22")
	("#" "%23")
	("$" "%24")
	("&" "%26")
	("'" "%27")
	("(" "%28")
	(")" "%29")
	("*" "%2A")
	("+" "%2B")
	("," "%2C")
	("-" "%2D")
	("." "%2E")
	("/" "%2F")
	("0" "%30")
	("1" "%31")
	("2" "%32")
	("3" "%33")
	("4" "%34")
	("5" "%35")
	("6" "%36")
	("7" "%37")
	("8" "%38")
	("9" "%39")
	(":" "%3A")
	(";" "%3B")
	("<" "%3C")
	("=" "%3D")
	(">" "%3E")
	("?" "%3F")
	("@" "%40")
	("A" "%41")
	("B" "%42")
	("C" "%43")
	("D" "%44")
	("E" "%45")
	("F" "%46")
	("G" "%47")
	("H" "%48")
	("I" "%49")
	("J" "%4A")
	("K" "%4B")
	("L" "%4C")
	("M" "%4D")
	("N" "%4E")
	("O" "%4F")
	("P" "%50")
	("Q" "%51")
	("R" "%52")
	("S" "%53")
	("T" "%54")
	("U" "%55")
	("V" "%56")
	("W" "%57")
	("X" "%58")
	("Y" "%59")
	("Z" "%5A")
	("[" "%5B")
	("\\" "%5C")
	("]" "%5D")
	("^" "%5E")
	("_" "%5F")
	("`" "%60")
	("a" "%61")
	("b" "%62")
	("c" "%63")
	("d" "%64")
	("e" "%65")
	("f" "%66")
	("g" "%67")
	("h" "%68")
	("i" "%69")
	("j" "%6A")
	("k" "%6B")
	("l" "%6C")
	("m" "%6D")
	("n" "%6E")
	("o" "%6F")
	("p" "%70")
	("q" "%71")
	("r" "%72")
	("s" "%73")
	("t" "%74")
	("u" "%75")
	("v" "%76")
	("w" "%77")
	("x" "%78")
	("y" "%79")
	("z" "%7A")
	("{" "%7B")
	("|" "%7C")
	("}" "%7D")
	("~" "%7E")))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

;; FIXME: make me work and move me
(defun urldecode (str) 
  (dolist (map *char-mapping*)
	(setq str (replace-all str (car (cdr map)) (car map) :test 'equal)))
  str)

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
					 (subseq line (+ 1 (search " " line)) (search " HTTP/1.1" line)))

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
								 (urldecode value)))))))

			   ) ;; should match up to ((search " HTTP ...

			  ;; do we know? are we in the body?
			  (in-body
			   (format t "body: '~a'~%" line))

			  ;; ahh, end of headers
			  ((string= line "")

			   ;; fixme: clean this up to be (method-is-get conn)
			   (when (string= "GET" (slot-value conn 'request-method))
;;				 (format t "GET IS DONE~%")
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
		(progn 
		  (format t "end of file!")
		  (http-networking-cleanup conn))))))

	  
(defun serve (conn)
  (let ((stream (socket-make-stream (slot-value conn 'socket) :output t :input t))
        (fd (socket-file-descriptor (slot-value conn 'socket))))

	(setf (slot-value conn 'stream) stream)
    (sb-impl::add-fd-handler fd
                             :input
                             (make-http-responder conn))))

(defun http-server (&key request-handler (port *default-http-port*))
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp))
        (counter 0))
	(format t "Using port ~a~%" port)
    (socket-bind socket *socket-listen-ip* port)
    (socket-listen socket *socket-listen-backlog*)
	(format t "Server Running...~%")
    (sb-impl::add-fd-handler (socket-file-descriptor socket)
                             :input
                             (lambda (_)
                               (declare (ignore _))
							   (setf (sb-bsd-sockets:non-blocking-mode socket) t)
                               (incf counter)
;;                               (format t "Accepted client ~A~%" counter)
                               (serve (make-instance 'http-connection :socket (socket-accept socket) :id counter :handler request-handler))))))

(provide 'http-server)