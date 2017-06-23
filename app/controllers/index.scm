;; Controller index definition of colt
;; Please add your license header here.
;; This file is generated automatically by GNU Artanis.
(define-artanis-controller index) ; DO NOT REMOVE THIS LINE!!!

(use-modules (app model posts))

(get "/" #:cache `(public ,(gen-cache-file "/index"))
  (lambda (rc)
    (or (try-to-get-page-from-cache rc)
        (get-index-content))))
