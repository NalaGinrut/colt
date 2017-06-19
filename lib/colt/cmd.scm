;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2017
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Colt is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License published
;;  by the Free Software Foundation, either version 3 of the License,
;;  or (at your option) any later version.

;;  Colt is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

(define-module (colt cmd)
  #:use-module (artanis irregex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-1)
  #:use-module ((rnrs) #:select (define-record-type
                                 get-string-all))
  #:export (cmd
            wcmd
            raw-cmd
            print-cmd-result
            cmd-result-map
            cmd-result-for-each
            cmd-result-any))

(define-record-type cmd-result
  (fields pipe status contents))

(define (parse-from-pipe pp read-only?)
  (let lp ((ret '()))
    (cond
     ((eof-object? (peek-char pp))
      (if read-only?
          (make-cmd-result #f (close-pipe pp) ret)
          (make-cmd-result pp #f ret)))
     (else
      (lp (cons (irregex-split "[ \t]" (read-line pp)) ret))))))

(define-syntax-rule (%cmd read-only? args ...)
  (let* ((cmd (pk (format #f "~{~a~^ ~}" `(args ...))))
         (result (parse-from-pipe (open-pipe cmd OPEN_BOTH) read-only?)))
    (when (not (zero? (status:exit-val (cmd-result-status result))))
      (format #t "[ERROR] ~a~%" cmd)
      (throw 'artanis-err 500 '%cmd "error: ~a" (cmd-result-status result)))
    result))

;; TODO: handle exceptions
(define-syntax-rule (%raw-cmd args ...)
  (let* ((cmd (pk (format #f "~{~a~^ ~}" `(args ...))))
         (result (get-string-all (open-pipe cmd OPEN_BOTH))))
    result))

(define-syntax-rule (wcmd args ...) (%cmd #f args ...))
(define-syntax-rule (cmd args ...) (%cmd #t args ...))
(define-syntax-rule (raw-cmd args ...) (%raw-cmd args ...))

(define* (print-cmd-result cr #:key (port (current-output-port)) (width 10)
                           (right-align? #t))
  (define (align) (if right-align? "@" ""))
  (let ((contents (cmd-result-contents cr))
        (fmt (string-append "~{~" (number->string width) (align) "a~^ ~}~%")))
    (for-each
     (lambda (line)
       (format port fmt line))
     contents)))

(define (lst-ref ll n)
  (if (>= (length ll) n)
      (list-ref ll n)
      #f))

(define (cmd-result-column-ref cr columns)
  (define (get-colums contents cols) ; cols is a list contains columns
    (map (lambda (x) (filter-map (cut lst-ref <> (1- x)) contents)) cols))
  (let ((contents (cmd-result-contents cr))
        (cn (sort columns <)))
    (get-colums contents cn)))

(define (cmd-result-line-ref cr lines)
  (define (get-lines contents lines)
    (map (lambda (x) (filter-map (cut lst-ref <> (1- x)) contents)) lines))
  (let ((contents (cmd-result-contents cr))
        (ln (sort lines <)))
    (get-lines contents ln)))

(define (cmd-result-map proc cr)
#t  
)

(define (cmd-result-for-each proc cr)
  #t)

(define (cmd-result-any proc init cr)
  #t)
