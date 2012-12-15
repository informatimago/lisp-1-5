;;;;**************************************************************************
;;;;FILE:               lisp15-driver.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A driver to read LISP 1.5 card decks and run them in Common Lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-10-04 <PJB> Added header comment.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2006
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(defpackage "LISP15-DRIVER"
  (:shadow "TRACE" "UNTRACE" "EVAL" "COMPILE"
           "MAP" "SET")
  (:use "COMMON-LISP"))

(in-package "LISP15-DRIVER")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-lines (text)
  (flet ((subtext (string start end)
           (make-array (- end start)
                       :element-type (array-element-type string)
                       :displaced-to string
                       :displaced-index-offset start)))
    (loop
       :with results = '()
       :with start   = 0
       :for  newline = (position #\newline text :start start)
       :do (cond
             (newline       (push (subtext text start newline) results)
                            (setf start (1+ newline)))
             ((zerop start) (return-from split-lines (list text)))
             (t             (unless (= start (length text))
                              (push (subtext text start (length text)) results))
                            (return-from split-lines (reverse results)))))))

(defmacro tracef (fctrl &rest args)
  `(progn (format *trace-output* "~{~&;;; ~A~}~%"
                  (split-lines (format nil ,fctrl ,@args)))
          (force-output *trace-output*)))

(defmacro show (&body expressions)
  (let ((width (reduce (function max)
                       (mapcar (lambda (expr) (length (format nil "~S" expr)))
                               expressions)
                       :initial-value 0)))
    `(progn
       ,@(mapcar
          (lambda (expr) `(tracef ,(format nil "~~~DS = ~~S~~%" width) ',expr ,expr))
          expressions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eval (form &optional environment)
  (declare (ignore environment))
  (cl:eval form))

(defun trace   (functions) (eval `(cl:trace   ,@functions)))
(defun untrace (functions) (eval `(cl:untrace ,@functions)))

(defun test   (arguments) (princ 'test)   (princ arguments) (terpri) (values))
(defun setset (arguments) (princ 'setset) (princ arguments) (terpri) (values))
(defun stop   (arguments) (throw 'driver-end-of-deck nil))
(defun fin    (arguments) (throw 'driver-end-of-deck nil))

(defun define (definitions)
  (mapcar (lambda (def)
            (eval (if (and (consp (second def)) (eq 'lambda (car (second def))))
                      `(progn
                         ()
                         (defun        ,(first def) ,@(cdr (second def)))
                         (defparameter ,(first def) ,(second def)))
                      `(defparameter ,(first def) ,(second def)))))
          definitions))

(defun compile (fun-or-list &optional definition)
  (cond
    (definition          (cl:compile fun-or-list definition))
    ((listp fun-or-list) (mapcar (function cl:compile) fun-or-list))
    (t                   (cl:compile fun-or-list))))

(defun map (list function) (mapcar function list))
(defun set (symbol value)  (setf (symbol-value symbol) value))
(defun sub1 (x) (1- x))

(defun driver (path &key (verbose nil))
  (with-open-file (cards path)
    (catch 'driver-end-of-deck
      (loop
         (let ((first-char (loop
                              :for ch = (read-char cards nil :eof)
                              :while (eql #\newline ch)
                              :finally (return ch))))
           (cond
             ((eql :eof first-char) (throw 'driver-end-of-deck nil))
             ((char= #\* first-char)
              (read-line cards))        ; comment
             (t
              (unread-char first-char cards)
              (let* ((command   (read cards nil 'fin))
                     (arguments
                      (if (member command '(stop fin test setset))
                          (list (read-line cards nil ""))
                          (read cards nil nil))))
                (when verbose
                  (show command arguments))
                (format t "~{~%~S~}~%"
                        (multiple-value-list
                         (eval `(,command ,@(mapcar (lambda (arg) `(quote ,arg))
                                                    arguments)))))))))))))
(print '(in-package "LISP15-DRIVER"))
(print '(driver "wang.job"))
(print '(driver "meteor.job"))

;;;; THE END ;;;;
