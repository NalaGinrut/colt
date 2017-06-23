;; Model posts definition of colt
;; Please add your license header here.
;; This file is generated automatically by GNU Artanis.
(create-artanis-model posts) ; DO NOT REMOVE THIS LINE!!!

(use-modules (colt git)
             (colt config)
             (artanis utils)
             (artanis irregex)
             (srfi srfi-1))

(export try-to-get-page-from-cache
        get-index-conetent)

(define-syntax-rule (index-render e)
  (let ((file (format #f "~a/app/views/index.html.tpl"
                      (current-toplevel))))
    (cond
     ((file-exists? file)
      (let ((html ((@@ (artanis tpl) tpl-render-from-file) file e)))
        (response-emit html)))
     (else (throw 'artanis-err 500 'index-render
                  "BUG: index.html.tpl doesn't exist!")))))

(define (post-title p)
  (meta-data-title (post-meta-data p)))

(define (post-timestamp p)
  (meta-data-timestamp (post-meta-data p)))

(define (post-tags p)
  (meta-data-tags (post-meta-data p)))

(define (post-status p)
  (meta-data-status (post-meta-data p)))

(define (post-title p)
  (meta-data-title (post-meta-data p)))

(define (post-comentable? p)
  (let ((check (meta-data-comment-status (post-meta-data p))))
    (string=? check open)))

(define (gen-cache-file path)
  (define-syntax-rule (-> str)
    (string-trim-both
     (irregex-replace/all "[/]+" str "_")
     (lambda (x) (memv x '(#\sp #\_)))))
  (format #f "~a/tmp/cache/~a" (current-toplevel) (-> path)))

(define (get-all-posts)
  (git/get-posts))

(define (get-posts-from-n-to-m n m posts-list)
  (list-head (list-tail posts-lists n) m))

(define (get-post-abstract posts-list)
    )

(define (catch-this-page content)
  )

(define (timestamp->rfc822-date timestamp)
  (date->string (timestamp->date timestamp)
                "~a, ~d ~b ~Y ~H:~M:~S GMT"))

(define (generate-index-content)
  (define-syntax-rule (->url url-name)
    (format #f "/articles/~a" (url-decode url-name)))
  (define-syntax-rule (->title title url)
    `(h2 (@ (class "title"))
         (a (@ (href ,url)) ,title)))
  (define-syntax-rule (->author author)
    `(a (@ (href ,(colt-conf-get 'github-url))
           (id "author"))
        ,author))
  (define-syntax-rule (->timestamp timestamp)
    (timestamp->readable-date (string->number timestamp)))
  (define-syntax-rule (->tags tags)
    (let lp ((t tags) (ret '(") ")))
      (cond
       ((null? t) (cons " (" ret))
       (else
        (lp (cdr t)
            (list (if (null? (cdr t)) "" " | ")
                  `(a (@ (href ,(format #f "/tags/~a" (car t)))))
                  ret))))))
  (fold (lambda (post prev)
          (let ((title (post-title post))
                (timestamp (post-timestamp post))
                (author (post-author post))
                (status (post-status post))
                (url-name (post-url-name post))
                (content (post-content post)))
            (cond
             ((string=? status "publish")
              (cons `(div (@ (class "post"))
                          ,(->title title (->url url-name))
                          (h3 (@ (class "meta"))
                              "Posted by " ,(colt-conf-get 'blog-author)
                              ,(->timestamp timestamp)
                              ,(->author author)
                              ,(->tags tags))
                          ,(->content content))
                    prev))
             (else prev))))
        '()
        (get-all-posts)))

(define (get-index-conetent)
  (catch-this-page
   content
   (index-render (tpl->html (generate-index-content))
                 (the-environment))))

(define (try-to-get-page-from-cache rc)
  (catch 'artanis-err
    (lambda () (:cache rc))
    (lambda (k status . args)
      (if (= status 404)
          #f
          (apply throw k status args)))))
