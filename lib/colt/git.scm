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

(define-module (colt git)
  #:use-module (artanis env)
  #:use-module (artanis irregex)
  #:use-module (colt cmd)
  #:use-module (colt config)
  #:use-module ((rnrs) #:select (define-record-type get-string-all))
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (current-blog-repo
            git-ls-tree
            git/get-posts
            get-post-by-url-name
            get-object-by-oid
            get-object-by-name

            post-meta-data
            post-content
            post-comment
            post-url-name

            meta-data-timestamp
            meta-data-tags
            meta-data-status
            meta-data-title
            meta-data-name
            meta-data-comment-status

            comment-timestamp
            comment-author
            comment-email
            comment-site
            comment-content

            enter-blog-repo))

;; NOTE: Must be absolute path
(define (current-blog-repo)
  (format #f "~a/~a" (current-toplevel) (colt-conf-get 'blog-repo)))

(define-record-type git-object
  (fields mode type oid file))

(define-record-type post
  (fields meta-data content comment url-name))

(define-record-type meta-data
  (fields timestamp tags status title name comment-status))

(define-record-type comment
  (fields timestamp author email site content))

(define-syntax-rule (-> l)
  (string-trim-both l))

(define-syntax-rule (rdline port)
  (-> (read-line port)))

(define *meta-data-res*
  `((timestamp . ,(string->irregex "^timestamp:(.*)"))
    (tags      . ,(string->irregex "^tags:(.*)"))
    (status    . ,(string->irregex "^status:(.*)"))
    (title     . ,(string->irregex "^title:(.*)"))
    (name      . ,(string->irregex "^name:(.*)"))
    (comment-status . ,(string->irregex "^comment_status:(.*)"))))

(define *comment-res*
  `((timestamp . ,(string->irregex "^timestamp:(.*)"))
    (author    . ,(string->irregex "^author:(.*)"))
    (email     . ,(string->irregex "^author_email:(.*)"))
    (site      . ,(string->irregex "^author_url:(.*)"))
    (content   . ,(string->irregex "^content:(.*)"))))

(define (get-re re-table key)
  (let ((re (assoc-ref re-table key)))
    (if re
        re
        (throw 'artanis-err 500 get-re
               "Invalid meta-data regex: ~a~%" re))))

(define (get-value re-table key str)
  (let* ((re (get-re re-table key))
         (m (irregex-search re str)))
    (if m
        (irregex-match-substring m 1)
        (throw 'artanis-err 500 get-value
               "~a has parsed an invalid line ~a~%" key str))))

(define (get-meta-data-value key port)
  (-> (get-value *meta-data-res* key (read-line port))))

(define (get-comment-value key port)
  (-> (get-value *comment-res* key (read-line port))))

(define (new-comment oid)
  (define (get-comment-content port)
    (read-line port)
    (get-string-all port))
  (call-with-input-string
   (raw-cmd git show ,oid)
   (lambda (port)
     (let* ((timestamp (get-comment-value 'timestamp port))
            (author (get-comment-value 'author port))
            (email (get-comment-value 'email port))
            (site (get-comment-value 'site port))
            (content (get-comment-content port)))
       (make-comment timestamp
                     author
                     email
                     site
                     content)))))

(define (get-object-by-oid oid obj-list)
  (any (lambda (o) (and (string=? oid (git-object-oid o)) o)) obj-list))

(define (get-object-by-name name obj-list)
  (any (lambda (o) (and (string=? name (git-object-file o)) o)) obj-list))

(define (get-post-by-url-name url-name post-list)
  (any (lambda (p) (and (string=? url-name (post-url-name p)) p)) post-list))

(define* (git-ls-tree #:key (branch "master"))
  (cmd-result-map
   (lambda (o)
     (match o
       ((mode type oid file)
        (make-git-object mode type oid file))
       (else (throw 'artanis-err 500 git-ls-tree
                    "BUG: Invalid git object format ~a" o))))
   (cmd git ls-tree ,branch)))

(define (get-all-comment-oids coid)
  (catch
      'artanis-err
    (lambda ()
      (fold (lambda (x p)
              (match x
                (() p)
                (("tree" _) p)
                ((oid) (cons oid p))
                (else (throw 'artanis-err 500 get-all-comment-oids
                             "Invalid comments tree ~a" x))))
            '()
            (cmd-result-contents (cmd git show ,coid))))
    ;; If any error, then there's no comments (just assuming)
    ;; FIXME: Do it more elegantly.
    (lambda e '())))

(define (git/get-content oid)
  (let ((cid (format #f "~a:content" oid)))
    (raw-cmd git show ,cid)))

(define (git/get-comments oid)
  (let* ((coid (format #f "~a:comments" oid))
         (comment-oids (get-all-comment-oids coid)))
    (map new-comment comment-oids)))

(define (git/get-meta-data oid)
  (define-syntax-rule (->list l)
    (map string-trim-both (string-split l #\,)))
  (define (parse-meta-data moid)
    (call-with-input-string
     (raw-cmd git show ,moid)
     (lambda (port)
       (let* ((timestamp (get-meta-data-value 'timestamp port))
              (tags (->list (get-meta-data-value 'tags port)))
              (status (get-meta-data-value 'status port))
              (title (get-meta-data-value 'title port))
              (name (get-meta-data-value 'name port))
              (comment-status (get-meta-data-value 'comment-status port)))
         (make-meta-data timestamp
                         tags
                         status
                         title
                         name
                         comment-status)))))
  (let ((moid (format #f "~a:metadata" oid)))
    (parse-meta-data moid)))

(define (get-post gobj)
  (let* ((oid (git-object-oid gobj))
         (url-name (git-object-file gobj))
         (content (git/get-content oid))
         (comments (git/get-comments oid))
         (meta-data (git/get-meta-data oid)))
    (make-post meta-data content comments url-name)))

(define (git/get-posts)
  (let ((ol (git-ls-tree)))
    (map get-post ol)))

(define (git-db-init)
  (cmd git init --bare --quiet))

(define (enter-blog-repo)
  (let ((blog-repo (current-blog-repo)))
    (cmd mkdir -p ,blog-repo)
    (chdir blog-repo)
    (git-db-init)))

(define (git/commit-change)
  #t)
