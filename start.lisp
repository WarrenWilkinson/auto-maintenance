
;;; -*- Mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
    
;;; Copyright (c) 2012, Warren Wilkinson.  All rights reserved.

;;; BEGIN_LICENSE:LGPL2
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Library General Public License as published by
;;; the Free Software Foundation; version 2.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public License
;;; along with this library; see the file COPYING.LIB.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301, USA.
;;;
;;; END_LICENSE

(format t "~%Downloading required dependencies...~%")

;; Load quicklisp
(load "quicklisp" :if-does-not-exist :error)
(eval `(handler-bind ((error
                       #'(lambda (e)
                           (declare (ignore e))
                           (invoke-restart 'quicklisp-quickstart::load-setup))))
         (quicklisp-quickstart::install)))

;; Now that it's ready, use ASDF to load our program, 
;; and quicklisp to download missing dependencies.
(eval `(handler-bind ((asdf::missing-dependency
                       #'(lambda (e)
                           (quicklisp:quickload (slot-value e 'asdf::requires))
                           (invoke-restart 'asdf::retry))))
         (asdf:oos 'asdf:load-op :auto-maintenance)))

(format t "~%All dependencies loaded... Running program.")
(funcall (find-symbol "MAIN" :auto-maintenance))
