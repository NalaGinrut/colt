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

(define-module (colt utils)
  #:use-module (srfi srfi-19)
  #:export (date-comparator
            date-before?
            date-after?
            rfc822-date->timestamp
            timestamp->date
            date->timestamp
            timestamp->atom-date
            timestamp->rfc822-date
            timestamp->readable-date))

(define (date-comparator date comp)
  (let ((this (time-second (date->time-utc date))))
    (lambda (that)
      (comp that this))))

(define (date-before? date)
  (date-comparator date <))

(define (date-after? date)
  (date-comparator date >))

(define (rfc822-date->timestamp str)
  (+ (time-second (date->time-utc
                   (string->date str "~a, ~d ~b ~Y ~H:~M:~S GMT")))
     (date-zone-offset (current-date))))

(define (timestamp->date timestamp)
  (time-utc->date (make-time time-utc 0 timestamp) 0))

(define (date->timestamp date)
  (time-second (date->time-utc date)))

(define (timestamp->atom-date timestamp)
  (date->string (timestamp->date timestamp)
                "~Y-~m-~dT~H:~M:~SZ"))

(define (timestamp->rfc822-date timestamp)
  (date->string (timestamp->date timestamp)
                "~a, ~d ~b ~Y ~H:~M:~S GMT"))

(define (timestamp->readable-date timestamp)
  (let ((date (time-utc->date
               (make-time time-utc 0 timestamp))))
    (date->string date "~e ~B ~Y ~l:~M ~p")))
