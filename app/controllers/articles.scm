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

(articles-define edit/:name
  (lambda (rc)
   "<h1>This is articles#edit</h1><p>Find me in app/views/articles/edit.html.tpl</p>"
  ;; TODO: add controller method `show'
  ;; uncomment this line if you want to render view from template
  ;; (view-render "show" (the-environment))
  ))

#;
(get "/articles/:article-name" #:cache #t
     (lambda (rc)
    
       )
     )
