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

;; NOTE: We may use more efficient half-static cache, which saves dynamic
;;       content to a static file, and use send-file for sending it. This
;;       reduces half time for each request, but cache missed. So we just
;;       use dynamic cache here, the first time fetching is half time slower
;;       but then the cache works for client.
(get "/" #:cache #t
  (lambda (rc)
    (:cache rc (get-index-content rc))))
