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

;; Controller index definition of colt
;; This file is generated automatically by GNU Artanis.
(define-artanis-controller index) ; DO NOT REMOVE THIS LINE!!!

(use-modules (app models posts))

(get "/" #:cache #t
  (lambda (rc)
    (:cache rc (get-index-content rc))))
