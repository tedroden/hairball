(defpackage hairball-system
  (:use :cl :asdf))

(in-package :hairball-system)

(defsystem "hairball"
  :description "hairball: an HTTP server and web framework for realtime web develoment."
  :version "0.0.1b"
  :author "Ted Roden <tedroden@gmail.com>"
  :licence "BSD"
  :components ((:file "packages")
			   (:file "http-server")
			   (:file "hairball-utils"))
  :depends-on (sb-bsd-sockets))

