;; Controller admin definition of colt
;; Please add your license header here.
;; This file is generated automatically by GNU Artanis.
(define-artanis-controller admin) ; DO NOT REMOVE THIS LINE!!!

(use-modules (colt config))

(define (->admin what)
  (format #f "~a/~a" (colt-conf-get 'admin-url) what))

(get (->admin "login")
  (lambda (rc)
    #t))

(post (->admin "auth")
  ;;#:auth (todo)
  #:session #t
  (lambda (rc)
    #t))

(get (->admin "") #:session #t
  (lambda (rc)
    #t))
