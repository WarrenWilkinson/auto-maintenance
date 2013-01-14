
;;; -*- Mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
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
         (asdf:oos 'asdf:test-op :data-model)))
(quit)
