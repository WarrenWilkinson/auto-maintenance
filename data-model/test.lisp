
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
  
(defpackage :data-model.test
  (:use :common-lisp :data-model :fiveam)
  (:shadow deftest)
  (:export run-tests))

(in-package :data-model.test)

(def-suite data-model-suite :description "The example test suite.")
(in-suite data-model-suite)

;; Our deftest makes *operations* a new hash table for the duration
;; of the tests.
(defmacro deftest (name &rest code)
  `(test ,name 
         (let ((data-model::*operations* (make-hash-table)))
           (declare (special *operations*))
           ,@code)))

(deftest can-create-vehicles ()
   (let ((gas    (make-automobile 'gas-auto :model "gas"))
         (elec   (make-automobile 'electric-auto :model "electric"))
         (diesel (make-automobile 'diesel-auto :model "diesel")))
     (is (not (null gas)))
     (is (not (null elec)))
     (is (not (null diesel)))))

(deftest can-get-vehicle-by-id ()
  (let ((car1  (make-automobile 'gas-auto :model "gas"))
        (car2  (make-automobile 'electric-auto :model "gas")))
    (is (eq (get-car (id car1)) car1))
    (is (eq (get-car (id car2)) car2))))

(deftest can-delete-vehicle ()
  (let* ((car1  (make-automobile 'gas-auto :model "gas"))
         (id1 (id car1))
         (car2  (make-automobile 'electric-auto :model "gas")))
    (delete-automobile id1)
    (is (null (get-car id1)))
    (is (eq (get-car (id car2)) car2))))

(deftest can-perform-operation ()
  (let ((car1  (make-automobile 'gas-auto :model "gas"))
        (car2  (make-automobile 'gas-auto :model "gas"))
        (op (make-instance 'tire-change)))
    (finishes (perform op car1))
    (is (member op (operations-on (id car1))))
    (is (not (member op (operations-on (id car2)))))))

(deftest cannot-gas-electric-car ()
  (let ((gas  (make-automobile 'gas-auto))
        (electric  (make-automobile 'electric-auto))
        (oil-change (make-instance 'oil-change)))
    (signals error (perform oil-change electric))
    (finishes (perform oil-change gas))))

(deftest can-delete-operation ()
  (let ((gas  (make-automobile 'gas-auto))
        (diesel (make-automobile 'diesel-auto))
        (tire-change (make-instance 'tire-change))
        (oil-change (make-instance 'oil-change)))

    (finishes (perform oil-change gas))
    (finishes (perform oil-change diesel))  ;; Same op can be applied to many vehicles
    (finishes (perform tire-change gas))    ;; API doesn't restrict this at low levels
    (is (member tire-change (operations-on (id gas))))
    (is (member oil-change (operations-on (id diesel))))
    (is (member oil-change (operations-on (id gas))))
   
    (finishes (delete-operation (id tire-change)))
    (is (not (member tire-change (operations-on (id gas)))))
    (is (member oil-change (operations-on (id diesel))))
    (is (member oil-change (operations-on (id gas))))

    (finishes (delete-operation (id oil-change)))
    (finishes (delete-operation (id tire-change))) ;; Deleting already-deleted things
    (is (null (operations-on (id gas))))           ;; is a no-op. 
    (is (null (operations-on (id diesel))))))      ;; Deletion removes operation
                                                   ;; from all cars.
   
   
  


(deftest deleting-car-deletes-operations ()
  (let ((gas  (make-automobile 'gas-auto))
        (diesel (make-automobile 'diesel-auto))
        (tire-change (make-instance 'tire-change))
        (oil-change (make-instance 'oil-change)))
  
    (finishes (perform oil-change gas))
    (finishes (perform oil-change diesel))  ;; Same op can be applied to many vehicles
    (finishes (perform tire-change gas))    ;; API doesn't restrict this at low levels

    (is (member tire-change (operations-on (id gas))))
    (is (member oil-change (operations-on (id diesel))))
    (is (member oil-change (operations-on (id gas))))

    (delete-automobile (id gas))
    (is (null (operations-on (id gas))))  ;; Deleted vehicles have no operations.
    (is (member oil-change (operations-on (id diesel)))))) ;; But it didn't
                                                           ;; delete shared ops.


(deftest api-uses-ids-and-nothing-else ()
  (let ((car1  (make-automobile 'gas-auto :model "gas"))
        (op (make-instance 'tire-change)))

    ;; Perform and Make-automobile use the actual objects.
    ;; Rest of API uses IDs.
    ;; Rationale: Operation doesn't exist until performed,
    ;; Thus perform method should save it.
    ;; And we'd be DB backed, so it makes sense to 
    ;; pass IDs rather than objects.
    (finishes (perform op car1)) 

    (signals type-error (get-car car1))
    (finishes (get-car (id car1)))

    (signals type-error (operations-on car1))
    (finishes (operations-on (id car1)))

    (signals type-error (delete-automobile car1))
    (signals type-error (delete-automobile "string"))
    (signals type-error (delete-automobile 'car1))
    (finishes (delete-automobile (id car1)))))

(defun run-tests () (run! 'data-model-suite))
