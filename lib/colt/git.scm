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
  #:use-module (colt cmd)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export ())

;; NOTE: Must be absolute path
(define current-blog-repo
  (make-parameter (format #f "~a/prv/blog.git" (current-toplevel))))

(define-record-type git-object
  (fields mode type oid file))

(define-record-type comment
  (fields timestamp author email site content))

(define-syntax-rule (-> l)
  (string-trim-both (cdr (string-split l #\:))))

(define-syntax-rule (rdline port)
  (-> (read-line port)))

(define (new-comment oid)
  (call-with-input-string
   (raw-cmd git show ,oid)
   (lambda (port)
     (let ((timestamp (rdline port))
           (author (rdline port))
           (email (rdline port))
           (site (rdline port))
           (content (-> (read-string-all port))))
       (make-comment timestamp
                     author
                     email
                     site
                     content)))))

(define (get-object-by-oid oid obj-list)
  (any (lambda (o) (and (string=? oid (git-object-oid o)) o)) obj-list))

(define (get-object-by-name name obj-list)
  (any (lambda (o) (and (string=? name (git-object-name o)) o)) obj-list))

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
  (fold (lambda (x p)
          (match x
            (("tree" _) p)
            ((oid) (cons x p))
            (else (throw 'artanis-err 500 git/get-comments
                         "Invalid comments tree ~a" x))))
        (cmd-result-contents (cmd git show coid))))

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
       (let ((timestamp (rdline port))
             (tags (->list (rdline port)))
             (title (rdline port))
             (name (rdline port))
             (comment-status (rdline port)))
         (make-meta-data timestamp
                         tags
                         title
                         name
                         comment-status)))))
  (let ((moid (format #f "~a:metadata" oid)))
    (parse-meta-data moid)))

(define (get-post oid)
  (let ((content (git/get-content oid))
        (comments (git/get-comments oid))
        (meta-data (git/get-meta-data oid)))
    (make-post meta-data content comments)))

(define (git/get-posts)
  (let ((ol (git-ls-tree)))
    (map get-post ol)))

(define (git-db-init)
  (cmd git init --bare))

(define (enter-blog-repo)
  (cmd mkdir -p (current-blog-repo))
  (chdir (current-blog-repo)))

(define (git/commit-change)
  #t)
