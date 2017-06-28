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

;; Controller articles definition of colt
;; This file is generated automatically by GNU Artanis.
(define-artanis-controller articles) ; DO NOT REMOVE THIS LINE!!!

(use-modules (app models posts)
             (artanis irregex)
             (web uri)
             (colt config))


(define *url-name-re* (string->irregex "/articles/(.*)"))
(define *edit-re* (string->irregex "/articles/edit/(.*)"))
(define (->article-url-name re path)
  (define (normalized-path str)
    (string-downcase (uri-encode str)))
  (let ((m (irregex-search re path)))
    (if m
        (normalized-path (irregex-match-substring m 1))
        (throw 'artanis-err 500 ->article-url-name
               "BUG: If it's not matched then it shouldn't be here!~%~a~%"
               path))))

(get (colt-conf-get 'admin-url)
  ;;#:auth (post)
  (lambda (rc)
    #t))

(define-syntax-rule (view-render method e)
  (let ((file (format #f "~a/app/views/~a/~a.html.tpl"
                      (current-toplevel) 'articles method)))
    (cond
     ((file-exists? file)
      (let ((html ((@@ (artanis tpl) tpl-render-from-file) file e)))
        (response-emit html)))
     (else (response-emit "" #:status 404)))))

(get "/articles/edit/(.*)"
  ;;#:auth (todo)
  (lambda (rc)
    (let* ((url-name (->article-url-name *edit-re* (rc-path rc)))
           (content (get-article-content-by-name url-name))
           (blog-name (colt-conf-get 'blog-name))
           (action "/article/modify"))
      ((@@ (app controllers articles) view-render) "edit" (the-environment)))))

(post "/articles/modify"
  ;;#:auth (todo)
  (lambda (rc)
    #t))

(post "/articles/new/"
  ;;#:auth (todo)
  (lambda (rc)
    #t))

(get "/articles/(.*)" #:cache #t
  (lambda (rc)
    (let ((url-name (->article-url-name *url-name-re* (rc-path rc))))
      (:cache rc (get-one-article url-name)))))
