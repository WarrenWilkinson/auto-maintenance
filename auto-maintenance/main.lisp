
;;; -*- Mode: LISP; Syntax: COMMON-LISP; package: auto-maintenance; Base: 10 -*-
  
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

(defpackage :auto-maintenance
  (:use :common-lisp :data-model :cl-who :hunchentoot)
  (:export main))

(in-package :auto-maintenance)

(defvar *swank-server*)
(defvar *hunchentoot-server*)

(defun print-car-row (stream car)
  (with-html-output (stream stream)
    (:tr 
     (:td (:a :href (format nil "/delete?id=~a" (id car))
              "X") "&nbsp;&nbsp;&nbsp;")
     (:td (:a :href (format nil "/car?id=~a" (id car))
              (str (class-name (class-of car)))))
     (:td (str (model car)))
     (:td (str (year car)))
     (:td (str (odometer car))))))

(define-easy-handler (index :uri "/") ()
  (with-html-output-to-string (stream nil :prologue t)
    (:html 
     (:head (:title "Auto-Maintenance"))
     (:body 
      (:h1 "Auto-Maintenance")
      (:p "Welcome to auto maintenance.")
      (:hr)
      (:h2 "Add New Car")
      (:form :method "post" :action "/newcar"
        (:dl (:dt (:label :for "model"    "Model"))
             (:dd (:input :type "text" :name "model"))
  
             (:dt (:label :for "year"     "Year")) 
             (:dd (:input :type "text" :name "year"))
  
             (:dt (:label :for "odometer" "Odometer")) 
             (:dd (:input :type "text" :name "odometer"))
  
             (:dt (:label :for "type" "type")
                  (:dd
                   (:input :type "radio" :id "gas" :name "type"
                           :value "GAS-AUTO" :checked "t") 
                   (:label :for "gas" "Gas") (:br)

                   (:input :type "radio" :id "electric" 
                           :name "type" :value "ELECTRIC-AUTO") 
                   (:label :for "electric" "Electric") (:br)

                   (:input :type "radio" :id "diesel" 
                           :name "type" :value "DIESEL-AUTO")
                   (:label :for "diesel" "Diesel") (:br))))
        (:input :type "submit"))
      (:p "The following cars are defined:")
      (:table
       (:thead (:tr (:th) (:th "Type") (:th "Model")
                    (:th "Year") (:th "Odometer")))
       (:tbody (dolist (car (all-cars))
                 (print-car-row stream car))))))))


(define-easy-handler (newcar :uri "/newcar")
    ((model :init-form "Unnamed"  :parameter-type 'string)
     (year                        :parameter-type 'integer)
     (Odometer                    :parameter-type 'integer)
     (type  :init-form "GAS-AUTO" :parameter-type 'string))
  (make-automobile (find-symbol type :data-model)
                   :model model :year year :odometer odometer)
  (redirect "/"))

(define-easy-handler (deletecar :uri "/delete")
    ((id :parameter-type 'integer))
  (and id (delete-automobile id))
  (redirect "/"))
 
(define-easy-handler (viewcar :uri "/car")
    ((id  :parameter-type 'integer)
     (msg :parameter-type 'string))
  (unless id (redirect "/"))
  (let ((car (get-car id)))
    (with-html-output-to-string (stream nil :prologue t)
      (:html 
       (:head (:title "Auto-Maintenance"))
       (:body 
        (:h1 "Car " (str id) ": " (str (class-name (class-of car))))
        (when msg (htm (:b (esc (url-decode msg)))))
        (:hr)
        (:a :href "/" "back") (:br)
        (:h2 "Operations")
        (:a :href (format nil "/oilchange?id=~d" id) "Oil Change") (:br)
        (:a :href (format nil "/tirechange?id=~d" id) "Tire Change") (:br)
        (:br)          
        (:ol (dolist (op (operations-on (id car)))
               (htm (:li (:a :href (format nil "/delete-operation?id=~d&car=~d"
                                           (id op) (id car)) "X") 
                         "&nbsp;&nbsp;"
                         (esc (princ-to-string op)))))))))))

(defun do-operation (op car)
  (url-encode (with-output-to-string (*standard-output*)
                (handler-case (perform op car)
                  (error (e) (princ e))))))

(define-easy-handler (oilchange :uri "/oilchange")
    ((id :parameter-type 'integer))
  (unless id (redirect "/"))
  (let* ((car (get-car id))
         (msg (do-operation (make-instance 'oil-change) car)))
    (redirect (format nil "/car?id=~d&msg=~a" id msg))))

(define-easy-handler (tirechange :uri "/tirechange")
    ((id :parameter-type 'integer))
  (unless id (redirect "/"))
  (let* ((car (get-car id))
         (msg (do-operation (make-instance 'tire-change) car)))
    (redirect (format nil "/car?id=~d&msg=~a" id msg))))
  

(define-easy-handler (del-operation :uri "/delete-operation")
    ((id  :parameter-type 'integer)
     (car :parameter-type 'integer))
  (when id (delete-operation id))
  (redirect (if car (format nil "/car?id=~d&msg=operation%20deleted" car) "/")))      

(defun main () 
  (format t "~%Starting Auto Maintenance...")
  
  (format t "~%  1. Starting Swank (port: 4005, coding system: utf-8-unix)...")
  (setf *swank-server* (swank:create-server :dont-close t :port 4005 :coding-system "utf-8-unix"))
  
  (format t "~%  2. Starting Hunchentoot (port: 8080)...")
  (setf *hunchentoot-server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080)))
  
  (format t "~%Welcome to Auto Maintenance!~%"))
