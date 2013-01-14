
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

(defpackage :data-model
   (:use :common-lisp)
   (:export perform automobile gas-auto electric-auto diesel-auto operation oil-change tire-change
            all-cars get-car make-automobile delete-automobile operations-on delete-operation
            id model year odometer))

(in-package :data-model)
 
(defvar *serial* 0)
(deftype id () `(integer 0 ,most-positive-fixnum))

(defclass automobile ()
  ((id       :initform (incf *serial*) :reader id :type 'id)
   (model    :initarg :model    :accessor model)
   (year     :initarg :year     :accessor year)
   (odometer :initarg :odometer :accessor odometer)))

(defclass gas-auto (automobile) ())
(defclass electric-auto (automobile) ())
(defclass diesel-auto (automobile) ())

 
(defclass operation   () 
  ((id       :initform (incf *serial*) :reader id)))
(defclass oil-change  (operation) ())
(defclass tire-change (operation) ())
(defmethod perform (operation automobile)
   (format t "~%Performing ~a on ~a" operation automobile))
(defmethod perform ((op oil-change) (g electric-auto))
   (error "You can't change the gas on an electric car.")) 

(defvar *operations* (make-hash-table))
 
(defmethod perform :around (operation automobile)
   (prog1 (call-next-method)
          (push operation (gethash automobile *operations*))))

(defun make-automobile (type &rest args &key model year odometer)
  (declare (ignore model year odometer))
  (let ((car (apply #'make-instance type args)))
    (setf (gethash car *operations*) nil)
    car))

(defun all-cars (&aux (store nil))
  "Returns all car objects"
  (maphash #'(lambda (k v) (declare (ignore v)) (push k store)) *operations*)
  store)

(defun get-car (id) 
  "Returns the car object with the corresponding ID"
  (declare (type id id))
  (find id (all-cars) :key #'id))

(defun delete-automobile (id)
  "Deletes the car object with the corresponding ID."
  (declare (type id id))
  (remhash (get-car id) *operations*))

(defun operations-on (id)
  "Fetches all operations performed on car with ID"
  (declare (type id id))
  (gethash (get-car id) *operations*))

(defun delete-operation (id)
  "Delete operation with the correspondind ID"
  (declare (type id id))
  (maphash #'(lambda (k v) (setf (gethash k *operations*) (remove id v :key #'id)))
           *operations*))
