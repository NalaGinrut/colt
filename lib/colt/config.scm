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

(define-module (colt config)
  #:use-module (artanis env)
  #:use-module (artanis utils)
  #:export (colt-conf-get
            colt-init-config))

(define *config-file*
  (format #f "~a/conf/colt.scm" (current-toplevel)))

(define *config* #f)

(define (colt-init-config)
  (if (file-exists? *config-file*)
      (set! *config* (call-with-input-file *config-file* read))
      (error (format #f "Fatal error: ~a is missing!" *config-file*))))

(define (colt-conf-get k)
  (or (and=> (assoc-ref *config* k) car)
      (throw 'artanis-err 500 colt-conf-get "Invalid config item `~a'" k)))
