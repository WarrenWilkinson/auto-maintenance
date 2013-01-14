
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
  
(defsystem :data-model
  :name "data model"
  :version "1.0.0"
  :author "Warren Wilkinson <warrenwilkinson@gmail.com>"
  :license "lgpl2"
  :description "The data-model built for the test."
  :components ((:file "data-model"))
  :in-order-to ((test-op (load-op data-model.test))))

(defsystem :data-model.test
  :name "data-model tests"
  :version "1.0.0"
  :author "Warren Wilkinson <warrenwilkinson@gmail.com>"
  :description "Testing code for the data-model package."
  :licence "LGPL2"
  :depends-on (:fiveam :data-model)
  :components ((:file "test")))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :data-model))))
  (funcall (intern "RUN-TESTS" :data-model.test)))
