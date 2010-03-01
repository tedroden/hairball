;; to run this, start "sbcl"
;; sbcl> (load "hello-world.lisp")
;;
;; then in another terminal window:
;;
;; $ curl -i localhost:9021/ --raw  
;; HTTP/1.1 200 OK
;; Date: Sun, 28 Mar 2010 22:06:51 GMT-5
;; Server: Hairball/0.0.1
;; 
;; <h1>Hello World</h1><h2>You are connection #1</h2>
;;
;; (and again)
;;
;; $ curl -i localhost:9021/ --raw  
;; HTTP/1.1 200 OK
;; Date: Sun, 28 Mar 2010 22:06:52 GMT-5
;; Server: Hairball/0.0.1
;; 
;; <h1>Hello World</h1><h2>You are connection #2</h2>
(require "http-server.lisp" "../http-server.lisp")

(defun dump-conn-info (conn)
  (format t "METHOD: '~a'~%" (slot-value conn 'http-server::request-method))
  (format t "URI: '~a'~%" (slot-value conn 'http-server::request-uri))
  (format t "HEADERS: ~%")
  (loop for header across (slot-value conn 'http-server::request-headers)
	 do (format t " - ~a~%" header)))

(defun handle-request (conn) 

  (dump-conn-info conn)

  (if (string= "GET" (slot-value conn 'http-server::request-method))
	  (format t "received ~a request~%" (slot-value conn 'http-server::request-method)))
	  
	  (http-server::finish-http-response conn 
										 (format nil "<h1>Hello World</h1><h2>You are connection #~d</h2>" 
												 (slot-value conn 'http-server::id))))

;; setup the server and our handler
(http-server::http-server :request-handler 'handle-request)
