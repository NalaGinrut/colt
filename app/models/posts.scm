;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2017
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Colt is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License
;;  published by the Free Software Foundation, either version 3 of
;;  the License, or (at your option) any later version.

;;  Colt is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Model posts definition of colt
;; This file is generated automatically by GNU Artanis.
(create-artanis-model posts) ; DO NOT REMOVE THIS LINE!!!

(use-modules (colt git)
             (colt config)
             (colt utils)
             (artanis utils)
             (artanis env)
             (artanis irregex)
             (srfi srfi-1)
             (rnrs))

(export try-to-get-page-from-cache
        get-index-content
        gen-cache-file)

(define-syntax-rule (index-render e)
  (let ((file (format #f "~a/app/views/index.html.tpl"
                      (current-toplevel))))
    (cond
     ((file-exists? file)
      (let ((html ((@@ (artanis tpl) tpl-render-from-file) file e)))
        (response-emit html)))
     (else (throw 'artanis-err 500 'index-render
                  "BUG: ~a/index.html.tpl doesn't exist!"
                  "app/views")))))

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

;; NOTE: We use 2-level caching here:
;; * The 1st level is to cache content to a static HTML file.
;;   Each time git repo changed, the cache file could be changed depends on situation.
;;   It's reasonable to do so, because git-DB is differrent, it's half-static page,
;;   which means the content in git-DB are static most of the time. Then we can make
;;   the content as a static HTML file, and use Artanis' cache for static file caching.
;;   It's NOT suitable to use Artanis' dynamic content caching.
;; * The 2ed level is to take advantage of Artanis' cache by ETag.
(define (gen-cache-file path)
  (define-syntax-rule (-> str)
    (string-trim-both
     (irregex-replace/all "[/]+" str "_")
     (lambda (x) (memv x '(#\sp #\_)))))
  (let ((p (-> path)))
    (if (string-null? p)
        (format #f "~a/tmp/cache/index.html" (current-toplevel))
        (format #f "~a/tmp/cache/~a.html" (current-toplevel) (-> path)))))

(define* (get-all-posts #:key (latest-top? #f))
  ((if latest-top? reverse identity) (git/get-posts)))

(define (get-posts-from-n-to-m n m posts-list)
  (list-head (list-tail posts-list n) m))

(define (get-post-abstract posts-list)
  (define (->abstract p)
    (call-with-input-string
     (post-content p)
     (lambda (port)
       (get-string-n port (colt-conf-get 'abstract-size)))))
  (map ->abstract posts-list))

(define (cache-this-page rc content)
  (let* ((cache-file (gen-cache-file (rc-path rc)))
         (fp (open-file cache-file "w")))
    (display content fp)
    (force-output fp)
    (close fp)
    content))

(define* (gen-one-post post #:optional (need-abstract? #f))
  (define-syntax-rule (->url url-name)
    (format #f "/articles/~a" (uri-decode url-name)))
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
                  `(a (@ (href ,(format #f "/tags/~a" (car t)))) ,(car t))
                  ret))))))
  (define-syntax-rule (->content content url-name)
    `(p (@ (id "post-content"))
        ,content
        (a (@ (href ,(->url url-name))) "More")))
  (let ((title (post-title post))
        (timestamp (post-timestamp post))
        (author (colt-conf-get 'blog-author))
        (url-name (post-url-name post))
        (tags (post-tags post))
        (content (post-content post)))
    `(div (@ (class "post"))
          ,(->title title (->url url-name))
          (h3 (@ (class "meta"))
              "Posted by " ,(->author author)
              ,(->timestamp timestamp)
              ,(->tags tags))
          ,(->content content url-name))))

(define (generate-index-content)
  (fold (lambda (post prev)
          (cond
           ((string=? (post-status post) "publish")
            (cons (gen-one-post post 'abstracted) prev))
           (else prev)))
        '()
        (get-all-posts #:latest-top? #t)))

(define (get-index-content rc)
  (let ((blog-name (colt-conf-get 'blog-name))
        (post-content (tpl->html (generate-index-content))))
    (cache-this-page rc (index-render (the-environment)))))

(define (try-to-get-page-from-cache rc)
  (catch 'artanis-err
    (lambda () (:cache rc))
    (lambda (k status . args)
      (if (= status 404)
          #f
          (apply throw k status args)))))
