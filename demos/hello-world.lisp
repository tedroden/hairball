;; to run this, start "sbcl"
;; sbcl> (load "hello-world")
;; then point your browser to http://localhost:9021

(require :asdf)

;; include the parent directory (which contains our libraries)
;; not needed if you "install" hairball.
(setf asdf:*central-registry* '(*default-pathname-defaults* #p"../"))

;; load the library
(asdf:oos 'asdf:load-op 'hairball)

;; handle the HTTP request
(defun handle-request (conn) 
  ;;  (print "hello")
  ;; show "Hello Visitor #1" where 1 is the nth connection
  (finish-http-response conn (format nil "<h1>Hello Visitor #~a</h1>"
									 (slot-value conn 'id))))

;; start the server
(http-server :request-handler 'handle-request) 
