;; Controller articles definition of colt
;; Please add your license header here.
;; This file is generated automatically by GNU Artanis.
(define-artanis-controller articles) ; DO NOT REMOVE THIS LINE!!!

(get "/"

  )

(articles-define show
  (lambda (rc)
  "<h1>This is articles#show</h1><p>Find me in app/views/articles/show.html.tpl</p>"
  ;; TODO: add controller method `show'
  ;; uncomment this line if you want to render view from template
  ;; (view-render "show" (the-environment))
  ))

