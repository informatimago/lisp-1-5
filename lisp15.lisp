;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               lisp15.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A LISP 1.5 implementation written in Common Lisp.
;;;;    (Far from completed yet).
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-12-15 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IBM-7090 words and characters encoding.
;;;

(defpackage "IBM-7090"
  (:use "COMMON-LISP")
  (:shadow "CODE-CHAR" "CHAR-CODE")
  (:export "+CHARSET+"  "CODE-CHAR" "CHAR-CODE"
   "CONVERT-STRING-TO-WORDS" "CONVERT-STRING-FROM-WORDS"
   "CONVERT-BYTES-TO-WORDS"  "CONVERT-BYTES-FROM-WORDS")
  (:documentation "Implements the IBM-7090 words and character encodings."))
(in-package "IBM-7090")


;; Normal encoding:
;;
;; (defparameter +charset+
;;   "0123456789#=\"###+ABCDEFGHI%.)###-JKLMNOPQRØ'*### /STUVWXYZ±,(###"
;;   "Maps lisp characters to IBM-7090 character code (position of the character
;;    in the string.  Invalid character codes are denoted by '#'.")


;; LISP1.5 encoding:
(defparameter +charset+
  "0123456789#=\"###+ABCDEFGHI%.)###-JKLMNOPQRØ$*### /STUVWXYZ±,(###"
  "Maps lisp characters to IBM-7090 character code (position of the character
   in the string.  Invalid character codes are denoted by '#'.")


(defun char-code (ch)   (unless (char= ch (character "#"))
                          (position (character ch) +charset+)))

(defun code-char (code) (let ((ch (aref +charset+ code)))
                          (unless (char= ch (character "#")) ch)))


(defun convert-string-to-words (string &key start end (filler #o77))
  "
DO:     Convert a string to words; a vector of (unsigned-byte 36).
NOTE:   An error is raised if not (every (function char-code)
                                         (subseq string start end))
"
  (setf start (or start 0)
        end   (or end (length string)))
  (flet ((safe-char-code
          (ch)
          (let ((result (char-code ch)))
            (unless result
              (error "Character '~C' cannot be encoded to IBM-7090." ch))
            result)))
    (loop with 6filler = (* filler #o010101010101)
          with result = (make-array (ceiling (- end start) 6)
                                    :element-type '(unsigned-byte 36)
                                    :initial-element 6filler)
          with j = -1
          for p = 30 then (if (zerop p) 30 (- p 6))
          for i from start below end
          for w = (dpb (safe-char-code (aref string i))
                       (byte 6 p) (if (= 30 p) 6filler w))
          when (zerop p) do (setf (aref result (incf j)) w)
          finally (progn (when (/= 30 p) (setf (aref result (incf j)) w))
                         (return result)))))


(defun convert-string-from-words (words &key start end)
  "
START and END are expressed in characters, not in words.
DO:     Convert words, a vector of (unsigned-byte 36) to a string.
NOTE:   An error is raised if not (every (function code-char)
                                         (convert-list-from-words words
                                                   :start start :end end))
        This means that client must specify :end when there's an invalid
        character code as filler, such as #o77.
"
  (setf start (or start 0)
        end   (or end (* 6 (length words))))
  (assert (<= 0 start (* 6 (length words)))
          (start)
          "START is out of bounds, should be between 0 and ~D"
          (* 6 (length words)))
  (assert (<= start end (* 6 (length words)))
          (end)
          "END is out of bounds, should be between ~D and ~D"
          start (* 6 (length words)))
  (if (= start end)
    ""
    (loop with result = (make-string (- end start)
                                     :initial-element (character " "))
          for i below (- end start)
          for code = (ldb (byte 6 (- 30 (* 6 (mod (+ start i) 6))))
                                   (aref words (truncate (+ start i) 6)))
          for ch = (code-char code)
          do (if ch
               (setf (aref result i) ch)
               (error "Invalid character code #o~O at position ~D" i))
          finally (return result))))


(defun convert-bytes-to-words (bytes &key start end (filler #o77))
  "
DO:     Packs a sequence of (unsigned-byte 6) into a vector of
        words (unsigned-byte 36).
"
  (setf start (or start 0)
        end   (or end (length bytes)))
  (loop with 6filler = (* filler #o010101010101)
        with get-byte = (if (listp bytes)
                          (let ((current (nthcdr start bytes)))
                            (lambda (i) (declare (ignore i)) (pop current)))
                          (lambda (i) (aref bytes i)))
        with result = (make-array (ceiling (- end start) 6)
                                  :element-type '(unsigned-byte 36)
                                  :initial-element 6filler)
        with j = -1
        for p = 30 then (if (zerop p) 30 (- p 6))
        for i from start below end
        for w = (dpb (funcall get-byte i) (byte 6 p) (if (= 30 p) 6filler w))
        when (zerop p) do (setf (aref result (incf j)) w)
        finally (progn (when (/= 30 p) (setf (aref result (incf j)) w))
                       (return result))))


(defun convert-bytes-from-words (words &key start end)
  "
DO:      Unpacks the vector of words (unsigned-byte 36)
         into a list of (unsigned-byte 6).
START and END are expressed in bytes, not in words.
"
  (setf start (or start 0)
        end   (or end (* 6 (length words))))
  (assert (<= 0 start (* 6 (length words)))
          (start)
          "START is out of bounds, should be between 0 and ~D"
          (* 6 (length words)))
  (assert (<= start end (* 6 (length words)))
          (end)
          "END is out of bounds, should be between ~D and ~D"
          start (* 6 (length words)))
  (when (< start end)
    (loop for i below (- end start)
          collect (ldb (byte 6 (- 30 (* 6 (mod (+ start i) 6))))
                                   (aref words (truncate (+ start i) 6))))))



;; AC (accumulator):
;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;; |S|Q|P| | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;; |s  |  1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5|
;; |0  |  0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3|
;; |   |                                                                       |

;; MQ (multiplier,quotient):
;;     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;     | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
;;     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;     |s 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5|
;;     |0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3|
;;     |                                                                       |

;; SI (sense indicator):
;;     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;     | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
;;     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;     |0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5|
;;     |0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3|
;;     |                                                                       |

;; floating point:
;;     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;     |S| characteristic|   fraction                                          |
;;     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;     |s 1 2 3 4 5 6 7 8|9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5|
;;     |0 0 0 0 0 0 0 0 0|0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3|
;;     |                 |                                                     |
;; value = (-1)^sign * (fraction*2^-27) * 2^(chacteristic+128)

;; fixed point:
;;     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;     |S|   magnitude                                                         |
;;     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;     |s 1 2 3 4 5 6 7 8|9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5|
;;     |0 0 0 0 0 0 0 0 0|0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3|
;;     |                 |                                                     |
;; value = (-1)^sign * magnitude



;;     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;     | | | |   decrement (15-bit)        | tags|   address (15-bit)          |
;;     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;     |s 1 2|3 4 5 6 7 8 9 0 1 2 3 4 5 6 7|8 9 0|1 2 3 4 5 6 7 8 9 0 1 2 3 4 5|
;;     |0 0 0|0 0 0 0 0 0 0 1 1 1 1 1 1 1 1|1 1 2|2 2 2 2 2 2 2 2 2 3 3 3 3 3 3|
;;     |     |                             |     |                             |


;;;base = (tags_18 * C)|(tags_19 * B)|(tags_20 * A) +(c2) address
;;;addr = (if (= 3 flg) (car base) base)

(defun op-code    (instruction)
  (let ((code1 (ldb (byte 24 12))))
    (if (member code1 '(#o4760))        ; RND
      (dpb (ldb (byte 0 15)) (byte 0 15) (ash code1 15))
      code1)))
            

(defun op-flags   (instruction) (ldb (byte 22  2)))
(defun op-count   (instruction) (ldb (byte 18  6)))
(defun op-tags    (instruction) (ldb (byte 15  3)))
(defun op-tag-a   (instruction) (ldb (byte 17  1)))
(defun op-tag-b   (instruction) (ldb (byte 16  1)))
(defun op-tag-c   (instruction) (ldb (byte 15  1)))
(defun op-address (instruction) (ldb (byte  0 15)))




(defparameter +op-codes+
  ;;  http://www.frobenius.com/instruction-formats.htm
  '(

    ;; page 20.
    (cla +0500 f t y "Clear and Add")
    (cal -0500 f t y "Clear and Add Logical Word")
    (cls +0502 f t y "Clear and Substract")
    (add +0400 f t y "Add")
    (adm +0401 f t y "Add Magnitude")
    ;; page 21.
    (sub +0402 f t y "Substract")
    (sbm -0400 f t y "Substract Magnitude")
    (acl +0361 f t y "Add and Carry Logical Word")
    ;; page 22.
    (mpy +0200 f t y "Multiply")
    (mpr -0200 f t y "Multiply and Round")
    (rnd +0760 t 00010 "Round" )
    (vlm +0204 c t t "Variable Length Multiply")
    ;; page 24.
    (dvh +0220 f t y "Divide or Halt")
    (dvp +0221 f t y "Divide or Proceed")
    (vdh +0224 c t y "Variable Lengh Divide or Halt")
    (vdp +0225 c t y "Variable Lengh Divide or Proceed")
    ;; page 27.
    (fad +0300 f t y "Floating Add")
    (fam +0304 f t y "Floating Add Magnitude")
    (ufa -0300 f t y "Unnormalized Floating Add")
    (fsb +0302 f t y "Floating Substract")
    ;; page 28.
    (uam -0304 f t y "Unnormalized Add Magnitude")
    (fsm +0306 f t y "Floating Substract Magnitude")
    (ufs -0302 f t y "Unnormalized Floating Substract")
    (usm -0306 f t y "Unnormalized Floating Substract Magnitude")
    (frn +0760 t 00011 "Floating Round")
    ;; page 29.
    (fmp +0260 f t y "Floating Multiply")
    (ufm -0260 f t y "Unnormalized Floating Multiply")
    ;; page 30.
    (fdh +0240 f t y "Floating Divide or Halt")
    (fdp +0241 f t y "Floating Divide or Proceed")
    ;; page 31.
    (als +0767 t y "Accumulator Left Shift")
    (ars +0771 t y "Accumulator Right Shift")
    (lls +0763 t y "Long Left Shift")
    (lrs +0765 t y "Long Right Shift")
    (lgl -0763 t y "Logical Left Shift")
    (lgr -0765 t y "Logical Right Shift")
    (rql -0773 t y "Rotate MQ Left")
    (ldq +0560 f t y "Load MQ")
    (stq -0600 f t y "Store MQ")
    (slq -0620 f t y "Store Left Half MQ")    ; (setcdr! y (cdr mq))
    (sto +0601 f t y "Store")
    (slw +0602 f t y "Store Logical Word")
    (stp +0630 f t y "Store Prefix")
    (std +0622 f t y "Store Decrement")       ; (setcdr! y (cdr a))
    (stt +0625 f t y "Store Tag")
    (sta +0621 f t y "Store Address")         ; (setcar! y (car a))
    (stl -0625 f t y "Store Instruction Location Counter")
    (str -1 "Store Location and Trap")
    (stz +0600 f t y "Store Zero")
    (xca +0131 "Exchange AC and MQ")
    (xcl -0130 "Exclange Logical AC and MQ")
    (enk +0760 t 00004 "Enter Keys")
    (htr +0000 f t y "Halt and Transfer")
    (nop +0761 "No Operation")
    (hpr +0420 "Halt and Proceed")
    (xec +0522 f t y "Execute")
    (tra +0020 f t y "Transfer")
    (etm +0760 t 00007 "Enter Trap Mode")
    (ltm -0760 t 00007 "Leave Trap Mode")
    (ttr +0021 f t y "Trap Transfer")
    (tze +0100 f t y "Transfer on Zero")
    (tnz -0100 f t y "Transfer on No Zero")
    (tpl +0120 f t y "Transfer on Plus")
    (tmi -0120 f t y "Transfer on Minus")
    (tov +0140 f t y "Transfer on Overflow")
    (tno -0140 f t y "Transfer on No Overflow")
    (tqp +0162 f t y "Transfer on MQ Plus")
    (tqo +0161 f t y "Transfer on MQ Overflow" "704 floating point mode")
    ;; page 39.
    (tlq +0040 f t y "Transfer on Low MQ")
    (tsx +0074 t y "Transfer and Set Index")
    (txi +1 d t y "Transer with Index Incremented")
    (txh +3 d t y "Transfer on Index High")
    (txl -3 d t y "Transfer on Index Low or Equal")
    (tix +2 d t y "Transfer on Index")
    (tnx -2 d t y "Transfer on No Index")
    (pse +0760 t (or 00140 ; slf
                  (integer 00141 00144) ; sln
                  (integer 00161 00166) ; swt
                  (integer 01341 01342) ; spu
                  (integer 02341 02342)
                  (integer 03341 03342)
                  (integer 04341 04342)
                  (integer 05341 05342)
                  (integer 06341 06342)
                  (integer 07341 07342)
                  (integer 10341 10342)
                  01360 02360 03360 04360 05360 06360 07360 10360 ; spt
                  (integer 01361 01362) ; spr
                  (integer 02361 02362)
                  (integer 03361 03362)
                  (integer 04361 04362)
                  (integer 05361 05362)
                  (integer 06361 06362)
                  (integer 07361 07362)
                  (integer 10361 10362))  "Plus Sense")
    (mse -0760 t (integer 00141 00144) "Minus Sense")
    (btt -0760 t (or 01000 02000 03000 04000 05000 06000 07000 10000)
     "Beginning of Tape test")
    (ett -0760 t (or 01000 02000 03000 04000 05000 06000 07000 10000)
     "End of Tape test")
    (iot +0760 t 00005 "Input-Output Check Test")
    (pbt -0760 t 00001 "P-Bit Test")
    (lbt +0760 t 00001 "Low-Order Bit Test")
    (dct +0760 t 00012 "Divide Check Test")
    ;; page 43.
    (zet +0520 f t y "Storage Zero Test")
    (nzt -0520 f t y "Storage not Zero Test")
    (cas +0340 f t y "Compare Accumulator with Storage")
    (las -0340 f t y "Logical Compare Accumulator with Storage")
    (tcoa +0060 f t y "Transfer on Channel A in Operation")
    (tcob +0061 f t y "Transfer on Channel B in Operation")
    (tcoc +0062 f t y "Transfer on Channel C in Operation")
    (tcod +0063 f t y "Transfer on Channel D in Operation")
    (tcoe +0064 f t y "Transfer on Channel E in Operation")
    (tcof +0065 f t y "Transfer on Channel F in Operation")
    (tcog +0066 f t y "Transfer on Channel G in Operation")
    (tcoh +0067 f t y "Transfer on Channel H in Operation")
    (tcna -0060 f t y "Transfer on Channel A not in Operation")
    (tcnb -0061 f t y "Transfer on Channel B not in Operation")
    (tcnc -0062 f t y "Transfer on Channel C not in Operation")
    (tcnd -0063 f t y "Transfer on Channel D not in Operation")
    (tcne -0064 f t y "Transfer on Channel E not in Operation")
    (tcnf -0065 f t y "Transfer on Channel F not in Operation")
    (tcng -0066 f t y "Transfer on Channel G not in Operation")
    (tcnh -0067 f t y "Transfer on Channel H not in Operation")
    (trca +0022 f t y "Transfer on Channel A Redundancy Check")
    (trcb -0022 f t y "Transfer on Channel B Redundancy Check")
    (trcc +0024 f t y "Transfer on Channel C Redundancy Check")
    (trcd -0024 f t y "Transfer on Channel D Redundancy Check")
    (trce +0026 f t y "Transfer on Channel E Redundancy Check")
    (trcf -0026 f t y "Transfer on Channel F Redundancy Check")
    (trcg +0027 f t y "Transfer on Channel G Redundancy Check")
    (trch -0027 f t y "Transfer on Channel H Redundancy Check")
    (tefa +0030 f t y "Transfer on Channel A End of File")
    (tefb +0031 f t y "Transfer on Channel B End of File")
    (tefc +0032 f t y "Transfer on Channel C End of File")
    (tefd +0033 f t y "Transfer on Channel D End of File")
    (tefe +0034 f t y "Transfer on Channel E End of File")
    (teff +0035 f t y "Transfer on Channel F End of File")
    (tefg +0036 f t y "Transfer on Channel G End of File")
    (tefh +0037 f t y "Transfer on Channel H End of File")
    (tch  +1 f y "Transfer in Channel")
    (lxa +0534 t y "Load Index from Address")
    (lac +0535 t y "Load Complement of Address in Index")
    (lxd -0534 t y "Load Index from Decrement")
    (ldc -0535 t y "Load Complement of Decrement in Index")
    (axt +0774 t y "Address to Index True")
    (axc -0774 t y "Address to Index Complemented")
    (pax +0734 t "Place Address in Index")
    (pac +0737 t "Place Complement of Address in Index")
    (pdx -0734 t "Place Decrement in Index")
    (pdc -0737 t "Place Complement of Decrement in Index")
    (sxa +0634 t y "Store Index in Address")
    (sxd -0634 t y "Store Index in Decrement")
    (pxa +0754 t "Place Index in Address")
    (pxd -0754 t "Place Index in Decrement")
    (ora -0501 f t y "OR to Accumulator")
    (ors -0602 f t y "OR t Storage")
    (ana -0320 f t y "AND to Accumulator")
    (ans +0320 f t y "AND to Storage")
    (era +0322 f t y "Exclusive OR to Accumulator")
    (com +0760 t 00006 "Complement Magnitude")
    (clm +0760 t 00000 "Clear Magnitude")
    (chl +0760 t 00002 "Change Sign")
    (ssp +0760 t 00003 "Set Sign Plus")
    (ssm -0760 t 00003 "Set Sign Minus")
    (pai +0044 "Place Accumulator in Indicators")
    (pia -0046 "Place Indicators in Accumulator")
    ;; page 51.
    (ldi +0441 f t y "Load Indicators")
    (sti +0604 f t y "Store Indicators")
    (oai +0043 "OR Accumulator to Indicators")
    (osi +0442 f t y  "OR Storage to Indicators")
    (sil -0055 r "Set Indicators of Left Half")
    ;; page 52.
    (sir +0055 r "Set Indicators of Right Half")
    (ria -0042 "Reset Indicators from Accumulators")
    (ris +0445 f t y "Reset Indicators from Storage")
    (ril -0057 r "Reset Indicators of Left Half")
    (rir +0057 r "Reset Indicators of Right Half")
    (iia +0041 "Invert Indicators from Accumulator")
    (iis +0440 f t y "Invert Indicators from Storage")
    (iil -0051 r "Invert Indicators of Left Half")
    (iir +0051 r "Invert Indicators of Right Half")
    (tio +0042 f t y "Transfer when Indicators On")
    (tif +0046 f t y "Transfer when Indicators Off")
    ;; page 54.
    (ont +0446 f t y "On Test for Indicators")
    (oft +0444 f t y "Off Test for Indicators")
    (lnt -0056 r "Left Half Indicators On Test")
    (rnt +0056 r "Right Half Indicators On Test")
    ;; page 55.
    (lft -0054 r "Left Half Indicators Off Test")
    (rft +0054 r "Right Half Indicators Off Test")
    ;; page 56.
    (cvr +0114 c v y "Convert by Replacement from the AC")
    (crq -0154 c v y "Convert by Replacement from the MQ")
    (caq -0114 c v y "Convert by Addition from the MQ")
    ;; page 57.  Cf. device codes.
    ;; page 58.
    (rds +0762 t y "Read Select")
    (wrs +0766 t y "Write Select")
    (bsr +0764 t y "Backspace Record")
    (bsf -0764 t y "Backspace File")
    (wef +0770 t y "Write End-of-File")
    (rew +0772 t y "Rewind")
    (run -0772 t y "Rewind and Unload")
    (sdn +0776 y "Set Density") ;; see page 59  for channels / density codes
    ;; page 60.
    (rdca +0760 t 01352 "Reset Data Channel A")
    (rdcb +0760 t 02352 "Reset Data Channel A")
    (rdcc +0760 t 03352 "Reset Data Channel A")
    (rdcd +0760 t 04352 "Reset Data Channel A")
    (rdce +0760 t 05352 "Reset Data Channel A")
    (rdcf +0760 t 06352 "Reset Data Channel A")
    (rdcg +0760 t 07352 "Reset Data Channel A")
    (rdch +0760 t 10352 "Reset Data Channel A")
    (scha +0640 f t y "Store Channel A")
    (schb -0640 f t y "Store Channel B")
    (schc +0641 f t y "Store Channel C")
    (schd -0641 f t y "Store Channel D")
    (sche +0642 f t y "Store Channel E")
    (schf -0642 f t y "Store Channel F")
    (schg +0643 f t y "Store Channel G")
    (schh -0643 f t y "Store Channel H")
    (rcha +0540 f t y "Reset and Load Channel A")
    (rchb -0540 f t y "Reset and Load Channel B")
    (rchc +0541 f t y "Reset and Load Channel C")
    (rchd -0541 f t y "Reset and Load Channel D")
    (rche +0542 f t y "Reset and Load Channel E")
    (rchf -0542 f t y "Reset and Load Channel F")
    (rchg +0543 f t y "Reset and Load Channel G")
    (rchh -0543 f t y "Reset and Load Channel H")
    (lcha +0544 f t y "Load Channel A")
    (lchb -0544 f t y "Load Channel B")
    (lchc +0545 f t y "Load Channel C")
    (lchd -0545 f t y "Load Channel D")
    (lche +0546 f t y "Load Channel E")
    (lchf -0546 f t y "Load Channel F")
    (lchg +0547 f t y "Load Channel G")
    (lchh -0547 f t y "Load Channel H")
    ;; page 65.
    (enb +0564 f t y "Enable Traps from Y")
    (rct +0760 t 00014 "Restore Channel Traps")
    (esnt -0021 f t y "Enter Storage Nullification and Transfer"
     "Enters 709 mode")
    (lsnm -0760 t 00010 "Leave Storage Nullification Mode"
     "Leaves 709 mode")
    (estm -0760 t 00005 "Enter Select Trap Mode")
    (ectm -0760 t 00006 "Enter Copy Trap Mode")
    (eftm -0760 t 00002 "Enter Floating Trap Mode")
    (lftm -0760 t 00004 "Leave Floating Trap Mode")
    ;; page 67.  Add Instructions for the IBM 7909 Data Channel
    
    
    ))


(defparameter +data-channel-commands+
  '(
    ;; page 62.
    (iocd +0 c f n y "Input-Output under Count Control and Disconnect")
    (iocp +4 c f n y "Input-Output under Count Control and Proceed")
    (iorp +2 c f n y "Input-Output of a Record and Proceed")
    (ioct +5 c f n y "Input-Output under Count Control and Transfer")
    (iort +3 c f n y "Input-Output of a Record and Transfer")
    (iosp +6 c f n y "Input-Output until Signal, then Proceed")
    (iost +7 c f n y "Input-Output until Signal, then Transfer")
    ;; page 68.  Add Commands for the IBM 7909 Data Channel
    ))
    
        
;;; codop(1) x(11)
;;;    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;    |opcod|x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x|
;;;    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;    |s 1 2|3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5|
;;;    |0 0 0|0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3|
;;;    |     |                                                                 |
;;;     3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
;;;     5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;;;
;;; codop(4) x(8)
;;;    +-+-+-|-+-+-|-+-+-|-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;    | |  op-code            |x x x x x x x x x x x x x x x x x x x x x x x x|
;;;    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;    |s 1 2 3 4 5 6 7 8 9 0 1|2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5|
;;;    |0 0 0 0 0 0 0 0 0 0 1 1|1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3|
;;;    |                       |                                               |
;;;
;;; codop(1) d(5) t(1) y(5)
;;;    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;    |opcod|    decrement ( 15-bit)      | tags|   address ( 15-bit)         |
;;;    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;    |s 1 2|3 4 5 6 7 8 9 0 1 2 3 4 5 6 7|8 9 0|1 2 3 4 5 6 7 8 9 0 1 2 3 4 5|
;;;    |0 0 0|0 0 0 0 0 0 0 1 1 1 1 1 1 1 1|1 1 2|2 2 2 2 2 2 2 2 2 3 3 3 3 3 3|
;;;    |     |                             |     |                             |
;;;     3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
;;;     5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;;; codop(4) x(2) t(1) codop(5)
;;;    +-+-+-|-+-+-|-+-+-|-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;    | |  op-code-1          |x x x x x x| tags|   op-code-2                 |
;;;    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;    |s 1 2 3 4 5 6 7 8 9 0 1|2 3 4 5 6 7|8 9 0|1 2 3 4 5 6 7 8 9 0 1 2 3 4 5|
;;;    |0 0 0 0 0 0 0 0 0 0 1 1|1 1 1 1 1 1|1 1 2|2 2 2 2 2 2 2 2 2 3 3 3 3 3 3|
;;;    |                       |           |     |                             |
;;;     3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
;;;     5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;;;op-code can be changed at run-time by indexing!
;;;
;;; codop(4) x(2) t(1) y(5)
;;;    +-+-+-|-+-+-|-+-+-|-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;    | |  op-code-1          |x x x x x x| tags|   address ( 15-bit)         |
;;;    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;    |s 1 2 3 4 5 6 7 8 9 0 1|2 3 4 5 6 7|8 9 0|1 2 3 4 5 6 7 8 9 0 1 2 3 4 5|
;;;    |0 0 0 0 0 0 0 0 0 0 1 1|1 1 1 1 1 1|1 1 2|2 2 2 2 2 2 2 2 2 3 3 3 3 3 3|
;;;    |                       |           |     |                             |
;;;     3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
;;;     5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;;;
;;; codop(4) c(2) t(1) y(5)
;;;    +-+-+-|-+-+-|-+-+-|-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;    | |  op-code-1          | count     | tags|   address ( 15-bit)         |
;;;    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;    |s 1 2 3 4 5 6 7 8 9 0 1|2 3 4 5 6 7|8 9 0|1 2 3 4 5 6 7 8 9 0 1 2 3 4 5|
;;;    |0 0 0 0 0 0 0 0 0 0 1 1|1 1 1 1 1 1|1 1 2|2 2 2 2 2 2 2 2 2 3 3 3 3 3 3|
;;;    |                       |           |     |                             |
;;;     3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
;;;     5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;;;
;;; codop(4) F(2/3) x(4/3) T(1) Y(5)
;;;    +-+-+-|-+-+-|-+-+-|-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;    | |  op-code            |flg|x x x x| tags|   address ( 15-bit)         |
;;;    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;    |s 1 2 3 4 5 6 7 8 9 0 1|2 3|4 5 6 7|8 9 0|1 2 3 4 5 6 7 8 9 0 1 2 3 4 5|
;;;    |0 0 0 0 0 0 0 0 0 0 1 1|1 1|1 1 1 1|1 1 2|2 2 2 2 2 2 2 2 2 3 3 3 3 3 3|
;;;    |                       |   |       |     |                             |
;;;     3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
;;;     5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;;;






;; #define SYMSTART 0		/* Start of name field */
;; #define OPCSTART 7		/* Start of opcode field */
;; #define VARSTART 15		/* Start of variable field */
;; #define NOOPERAND 16		/* If we get this far, no variable field */
;; #define RIGHTMARGIN 71		/* End of variable field */







;; * CONSW        PUTS FILL WORDS IN FULL WORD SPACE
;; 
;;  CONSW SXA     CSWX,4             SAVE LINK IR
;;                                   (setf cswx ir4)
;; 
;;  FWLOR LXD     FWORDL,4           PICK UP FULL WORD LIST
;;                                   (setf ir4 (cdr fwordl))
;; 
;;        TXL     FWLOUT,4,0         TEST FOR NO MORE
;;                                   (unless (< 0 ir4) (fwlout)) ; calls gc
;;                                       
;;        STQ     CSWQ               SAVE MQ
;;                                   (setf cswq mq)
;; 
;;        LDQ     0,4                PICK UP POINTER TO NEXT WORD ON FWL
;;                                   (setf mq (deref ir4))
;; 
;;        SLQ     FWORDL             UP DATE FULL WORD LIST POINTER
;;                                   (setf fwordl (cdr mq))
;;                                   
;;        STO     0,4                PUT AC IN FULL WORD AREA
;;                                   (setf (deref ir4) ac)
;; 
;;        PXD     0,4                POINTER TO AC
;;                                   (setf (cdr ac) ir4)
;; 
;;        LXD     FWORDL,4           POINTER TO NEXT AVAILABLE WORD
;;                                   (setf ir4 (cdr fwordl))
;; 
;; LOWARY TXH     CSWO,4,**          BOTTOM FULL WORD SPACE, TEST FOR ARY
;;                                   (if (< lowary ir4) (go :cswo))
;; 
;;        SXD     *-1,4              AVAILABLE LOCATION AND UPDATE SAME
;;                                   (setf lowary ir4)
;; 
;;  CSWO  LDQ     CSWQ               RESTORE MQ
;;                                   (setf mq cswq)
;; 
;;  CSWX  AXT     **,4               RESTORE LINK IR
;;                                   (setf ir4 cswx)
;; 
;;        TRA     1,4                EXIT
;;                                   (return)
;; 
;;  CSWQ                             TEMPORARY STORAGE FOR MQ
;; FWORDL                            POINTER TO FULL WORD LIST


;; *
;; * CONS         BASIC LISP FUNCTION PUTS A WORD IN FREE STORAGE
;; *
;;  CONS  SXA     CNSX,4             SAVE LINK IR
;;        LXD     $FREE,4            GET FREE STORAGE LIST POINTER
;;                                   (setf ir4 (cdr free))
;; 
;;        TXH     *+2,4,0            SKIP IF NOT OUT OF FREE STORAGE
;;        TSX     FROUT,4            OUT OF FREE STORAGE
;;                                   (when (null ir4)  (frout))
;; 
;;        ARS     18                 DECREMENT TO ADDRESS
;;                                   (setf (car ac) (cdr ac)
;;                                         (cdr ac) nil)
;; 
;;        STA     0,4                PUT ADDRESS AWY
;;                                   (setf (car (deref ir4)) (car ac))
;; 
;;        CLA     0,4                GET POINTER TO NEXT WORD IN FREE
;;                                   (setf ac (deferf ir4))
;; 
;;        STD     FREE               PUT IN FREE
;;                                   (setf (cdr free) (cdr ac))
;;                                                       
;;        SLQ     0,4                PUT DECREMENT AWAY
;;                                   (setf (cdr (deref ic)) (cdr mq))
;; 
;;        PXD     0,4                POINTER TO WORD
;;                                   (setf (cdr ac) ir4)
;; 
;;  CNTR1 AXT     **,4               LOW ORDER 15 BITS OF CONS COUNTER KEPT
;;        TIX     *+3,4,1            DECREMENT COUNT BY 1
;;        TSX     ARREST,4           COUNT EXHAUSTED, RELOAD OR STOP
;;        AXT     -1,4               RELOAD NUMBER
;;        SXA     CNTR1,4            PUT IN COUNTER
;; 
;;                                   (incf cntr1)
;; 
;;  CNSX  AXT     **,4               RESTORE LINK IR
;;        TRA     1,4                EXIT
;;  FREE                             POINTER TO FREE STORAGE LIST
;; 
;; 
;; 
;; (cdr ac) := (cons (cdr ac) (cdr mq))
;; (cdr mq) unchanged.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  LISP15-PRIMITIVES 
;;;

(defpackage "LISP15-PRIMITIVES"
  (:use "COMMON-LISP")
  (:shadow "ERROR")
  (:export
   "COND"
   "NULL" "NIL"
   "CONSP" "CONS" "CAR" "CDR"
   "ATOM" "T"
   "DEFUN"
   "EQL"
   "ERROR"
   "SETF" "TAGBODY" "GO" "LET" "LAMBDA"
   "DEFPARAMETER" "DEFCONSTANT"
   "AREF" "TRUNCATE" "MOD" "+" "FORMAT"
   ))
(in-package "LISP15-PRIMITIVES")


(defmacro error (message &optional in-extenso)
  `(common-lisp:error message))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  LISP15
;;;

(defpackage "LISP15"
  (:use "LISP15-PRIMITIVES"))
(in-package "LISP15")

  
(defun consw (word)
  "
WORD can be a cons cell, nil or a fixnum, or a char, or a float,
or a pointer to a symbol or an array,
or, a pointer to something else
"
  (cond
   ((consp word) (cons (car word) (cdr word)))
   (word)))


;; * AC := CONSW(AC)
;; * AC := CP1(AC)


;;            FUNCTION CP1
;;            CP1(L)=(L=0 YIELDS 0.
;;                   OTHERWISE CONS(CONSW(CWR(CAR(L)))),CP1(CDR(L))))

 
(defun cp1 (l)
  (cond ((null l) nil)
        (t (cons (consw (car l)) (cp1 (cdr l))))))



;;     SUBST(L,V,M) =
;;     (M = 0 YIELDS 0,
;;        EQUAL(M,V) YIELDS COPY(L),
;;        CAR(M)=-1 YIELDS M 
;;        1 YIELDS CONS(SUBST(L,V,CAR(M)),SUBSTL,V,CDR(M))))

(defun subst (l v m)
  (cond
   ((null m)    nil)
   ((equal m v) (copy l))
   ((atom m)    m)
   (t           (cons (subst l v (car m)) (subst l v (cdr m))))))


;; SUBLISP

;;            APPEND(L1,L2)=
;;            (L1=0 YIELDS L2,1 YIELDS CONS(CAR(L1),APPEND(CDR(L1),L2))

(defun append (l1 l2)
  (cond ((null l1) l2)
        (t (cons (car l1) (append (cdr l1) l2)))))


(defun pair (key data)
  (let ((result
         (maplis
          (lambda (key)
            (cond
             ((null data) (error "*F  3*" "PAIR: SECOND ARG LIST TOO SHORT"))
             (t           (let ((tem (car data)))
                            (setf data (cdr data))
                            (cons (car key) tem))))) key)))
    (cond (data (error "*F  2*" "PAIR: FIRST ARG LIST TOO SHORT"))
          (t   result))))


;;            MAPCAR(L,F) = (L=0 YIELDS 0,
;;                           F(L) YIELDS 0,
;;                           1 YIELDS MAPAR(CDR(L),F))

(defun mapcar (l f)
  (cond ((null l)      nil)
        ((funcall f l) nil)
        (t             (mapcar (cdr l) f))))
          

;;            MAPCON(L,F)=
;;            (L=0 YIELDS 0,,1 YIELDS NCONC(F(L),MAPCON(CDR(L),F)))

(defun mapcon (l f)
  (cond ((null l) nil)
        (t        (nconc (funcall f l) (mapcon (cdr l) f)))))


;;            FUNCTION NCONC
;;            /    L1=0 YIELDS RETURN(L2)
;;                 M=L1
;;            A2   CDR(M)=0 YIELDS GO A1
;;                 M=CDR(M)
;;                 GO A2
;;            A1   CDR(M)=L2
;;            //   RETURN(L1)

(defun nconc (l1 l2)
  (tagbody
   (cond ((null l1) l2))
   :a2
   (cond ((null (cdr l1)) 
          (go :a1))
         (t
          (setf l (cdr l1))
          (go :a2)))
   :a1
   (setf (cdr m) l2)
   l1))


;;            REMPRP REMOVES THE PROPERTY GIVEN BY THE MQ FROM THE
;;            OBJECT GIVEN BY THE AC

(defun remprop (object property)
  (error "remprop not implemented yet"))


;;            PROP(O,P,U)
;;             = (NULL(O) YIELDS U, CAR(O) = P YIELDS CDR(O),
;;                 T YIELDS PROP(CDR(O),P,U))

(defun prop (o p u)
  (cond ((null o)       u)
        ((eq (car o) p) (cdr o))
        (t              (prop (cdr o) p u))))


(defun maplist (fun l)
  (cond ((null l) nil)
        (t (cons (funcall fun l) (maplist fun (cdr l))))))




;;            OBJECT GENERATOR
;; 
;; GENSYM SXA     GENX,4             SAVE LINK IR
;;        CLA     DIGIT              GET DIGITS                            PAGE 097
;;        CVR     BCDAD1,,6          ADD 1 IN BCD
;;        STO     DIGITS
;;        ORA     LETTR
;;        TSX $CONSW,4
;;        LDQ GENZ
;;        TSX $CONS,4
;;        LDQ GENZ
;;        TSX $CONS,4
;;        XCA
;;        CLA GENPN
;;        TSX $CONS,4
;;        XCA
;;        CLA     GENC
;;        TSX     $CONS,4
;;  GENX  AXT     **,4               RESTORE LINK IR
;;        TRA 1,4
;;  GENZ  SYN     $ZERO
;;  GENPN SYN     PNAMED
;;  GENC  SYN     $DMASK
;;  LETTR BCI     1,G00000
;;  DIGIT BCI     1,000000

(defconstant +genz+ 0)
(defconstant +genpn+ 'PNAMED)
(defconstant +genc+  +dmask+)




(defconstant +dmask+ #o000000077777)
(defconstant  lettr 23)
(defparameter digit 0)


(defparameter ac 0)
(defparameter mq 0)

(defun gensym ()
  (setf digit (+ digit 1))
  (cons
   +dmask+
   (cons
    'pname
    (cons
     (cons
      (cons
       (format nil "~C~5,'0O"
               (aref ibm-7090:+charset+ (or 23 (truncate digit #o100000)))
               (mod digit #o100000))
       mq) nil) nil))))

(shadow 'gensym)

(defconstant +dmask+ #o000000077777)
(defvar digits 0)
(defparameter mq 0)
(defun gensym ()
  (cons +dmask+
        (cons 'pname
              (cons
               (cons
                (cons
                 (format nil "G~5,,,'0A" (mod (incf digits) 100000.))
                 mq)
                nil)
               nil))))
(gensym)
(32767 pname (("G3.0" . 0)))
(32767 pname (("G2.0" . 0)))













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :common-lisp-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun generate-oblb ()
  (let ((line -1))
    (dolist
        (item
         `(("    -II14" add 1)
           ("    -)ALST")
           and f1 f18 apval
           ("    -II1" array)
           atom f29 car cdr caar cdar cadr cddr caaar caadr cadar caddr
           cdaar cdadr cddar cdddr cond consn copyn
           ("    DUMP")
           f12 f35
           ("    -IJ01" differ)
           ("    -IJ02" divide)
           eq f8 f21 f19 evlisl expr f32 fexpr
           fix    ("    -II11" fix p)
           float  ("    -II12" float p)
           fsubr funarg funct symgen go
           ("    -II3" greater than p)
           f16 label lambda 
           ("    LAP")
           ("    -II4" less than p)
           list
           ("    LOADA" loader object)
           pmapca
           ("-)069B")
           ("-)069A")
           ("    -II7" maximum)
           ("    -II8" minimum)
           minus
           ("    -II16" minus p)
           f3 nil not null
           ("    -II13" number p)
           ("    OBLBA" oblist object)
           ("    -II9" one p)
           or f2 pause plb plus pname f4 prog propo 
           ("    -IJ05" punch)
           quote
           ("    -IJ03" quotient)
           f13
           ("    -II18" recip)
           rclam prplca prplcd
           ("    -IJ04" remainder)
           ("    RETATM" return)
           sasco srch set setq f34 stop subr
           ("    TRACE")
           ("    SMOVE")
           ("    SRETUR")
           ("    SLIST")
           ("    SPECAL")
           ("    -II15" subtract 1)
           f17 f30
           ("    1" *t* binary true atom)
           f27
           ("    SYM")
           times f36
           ("    -II10" zero p)
           cget
           ("    REMPP")
           ,@(loop for i below 64
                   collect (format nil "H~2,'0O" (case i
                                                   ((#o15) #o14) ((#o14) #o15)
                                                   (otherwise i))))
           ,@(loop for i from 1 to 39
                   unless (member i '(3 13 20 22 29))
                   collect (format nil "~APJ~D" (if (< i 24) "" "    ") i))
           ("    ERSETO" errorset)
           ("    PVW1" "LAST OBJECT   - LEFTSHIFT")
           ))
      (format t "~72AGENER~3,'0D~%" 
              (if (atom  item)
                (format nil "           ~A"
                        (format nil "~A,,-*-1" item))
                (format nil "           ~22A~{ ~A~}"
                        (format nil "~A,,-*-1" (car item)) (cdr item)))

              (incf line)))))


(defun string-to-list (string)
  (coerce (ibm-7090:convert-string-to-words string) 'list))



(defparameter *code-output* t "Stream where code is output")
(defparameter *code* nil)
(defparameter *line* -1)
(defparameter *id* "")

(defun geninit (id)
  (setf *code* (make-array (list 50) :adjustable t :fill-pointer 1)
        *id*   id))

(defun generate (label cop car tag cdr comment)
  (vector-push-extend (list label cop car tag cdr comment) *code*))

(defun punch (line &optional (id *id*))
  (format *code-output* "~72A~4A~4,'0D~%" line id (incf *line*)))


(defun print-code (addr code)
  (punch 
   (with-output-to-string (*standard-output*)
     (labels ((print-address (target) (unless (zerop target)
                                        (format t "-*-~A" (- addr target))))
              (addressp (item) (and (consp item) (eq 'address (car item))))
              (nullp (item) (or (null item)
                                (and (addressp item) (zerop (second item))))))
       (destructuring-bind (label cop car tag cdr comment) code
         (format t "~:[      ~;~:*~6A~] " label)
         (format t "~:[       ~;~:*~7A~] " (unless (eq 'pze cop) cop))
         (format t "~18A " 
                 (case cop
                   ((oct) (format nil "~O" car))
                   ((dec) (format nil "~D" car))
                   ((hex) (format nil "~H" car))
                   ((bcd bci) (error "BCD/BCI Not implemented yet."))
                   (otherwise
                    (with-output-to-string (*standard-output*)
                      (if (addressp car)
                        (print-address (second car))
                        (format t "~A" car))
                      (when (or tag (not (nullp cdr))) (princ ","))
                      (when tag (princ tag))
                      (unless (nullp cdr)
                        (princ ",")
                        (if (addressp cdr)
                          (print-address (second cdr))
                          (format t "~A" cdr)))))))
         (format t "~:[~;~:*~A~]" comment))))))


(defun generate-item (item next &key label comment)
  (cond
   ((integerp item) (generate label 'pze item nil `(address ,next) comment))
   ((eq 'nil  item) (generate label 'pze 0    nil `(address ,next) comment))
   ((eq 't    item) (generate label 'pze 1    nil `(address ,next) comment))
   ((symbolp  item) (generate label 'pze item nil `(address ,next) comment))
   ((stringp  item) (generate label
                              'pze `(address ,(generate-string item next))
                              nil `(address ,next) (or comment item)))
   ((consp    item) 
    (case (first item)
      ((function)
       (generate label
                 'pze `(address
                         ,(generate nil 'txl (second item)
                                    nil (third item) nil))
                 nil `(address ,next) comment))
      ((asm)
       (generate label (second item) (third item) (fourth item) (fifth item)
                 comment))
      (otherwise
       (generate label
                 'pze `(address ,(generate-list item))
                 nil `(address ,next) comment))))
   (t (error "Invalid item ~S type ~S in plist" item (type-of item)))))


(defun generate-word (word next)
  (generate nil 'pze `(address ,(generate nil 'oct word nil nil nil))
            nil `(address ,next) nil))

(defun generate-string (string next)
  (loop for next = 0 then addr
        for word in (reverse (string-to-list string))
        for addr = (generate-word word next)
        finally (return addr)))

(defun generate-list (list &key comment)
  (loop for next = 0 then addr
        for (item . remains) on (reverse list)
        for addr = (generate-item item next :comment (unless remains comment))
        finally (return addr)))

(defun generate-plist (item &key (id *id*) comment)
  (let ((label (first item))
        (plist (second item)))
    (geninit id)
    (generate-list plist :comment comment)
    (setf (first (aref *code* (1- (fill-pointer *code*)))) label)
    (loop for pc from (1- (fill-pointer *code*)) downto 1
          for line = (aref *code* pc)
          do (print-code pc line))))

(defparameter +symbols+
  `((ii14       (-1  $subr (function add1   1) $pname "ADD1"))
    (")PJ2"     (-1   subr (function advanc 0)  pname "ADVANCE"))
    (")ALST"    (-1   pname"$ALIST" sym -c$alst))
    (")002"     (-1  fsubr (function $evand 0) $pname "AND"))
    (")003"     (-1   subr (function append 2)  pname "APPEND"))
    (")004"     (-1   subr (function apply  3)  pname "APPLY"))
    (")005"     (-1   pname "APVAL"))
    ("II1"      (-1   subr (function arymak 1)  pname "ARRAY"))
    (")007"     (-1   subr (function atomp  1)  pname "ATOM"))
    (")008"     (-1   subr (function attrib 2)  pname "ATTRIB"))
    (")PJ12"    (-1   pname "BLANK" apval1 (h60)))
    (")011"     (-1   subr (function carp   1)  pname "CAR"))
    (")012"     (-1   subr (function cdrp   1)  pname "CDR"))
    (")201"     (-1   subr (function caarxx 1)  pname "CAAR"))
    (")202"     (-1   subr (function cdarxx 1)  pname "CDAR"))
    (")203"     (-1   subr (function cadrxx 1)  pname "CADR"))
    (")204"     (-1   subr (function cddrxx 1)  pname "CDDR"))
    (")205"     (-1   subr (function caaarx 1)  pname "CAAAR"))
    (")206"     (-1   subr (function caadrx 1)  pname "CAADR"))
    (")207"     (-1   subr (function cadarx 1)  pname "CADAR"))
    (")208"     (-1   subr (function caddrx 1)  pname "CADDR"))
    (")209"     (-1   subr (function cdaarx 1)  pname "CDAAR"))
    (")210"     (-1   subr (function cdadrx 1)  pname "CDADR"))
    (")211"     (-1   subr (function cddarx 1)  pname "CDDAR"))
    (")212"     (-1   subr (function cdddrx 1)  pname "CDDDR"))
    (")PJ32"    (-1   pname "CHARCOUNT" apval1 (((asm mze -1 1 -chact)))))
    (")PJ27"    (-1   subr (function clear  0)  pname "CLEARBUFF"))
    (")PJ6"     (-1   pname "COMMA" apval1 (h73)))
    (")016"     (-1  fsubr (function $evcon 0)  pname "COND"))
    (")017"     (-1   subr (function cons   2)  pname "CONS"))
    (")019"     (-1   subr (function cp1    1)  pname "CP1"))
    (")020"     (-1   subr (function $copy  1)  pname "COPY"))
    (")021"     (-1   subr (function count  0)  pname "COUNT"))
    (")PJ1"     (-1   apval1 -curc1  pname "CURCHAR" specal -curc))
    (")PJ16"    (-1   apval1 (h40)   pname "DASH"))
    (ij01       (-1  $subr (function differ 2) $pname "DIFFERENCE"))
    (")PJ19"    (-1   subr (function digit  1)  pname "DIGIT"))
    (ij02       (-1  $subr (function divide 2) $pname "DIVIDE"))
    (")PJ10"    (-1   pname "DOLLAR" apval1 (h53)))
    (dmpcb      (-1   subr (function dumpxx 4)  pname "DUMP"))
    (")PJ30"    (-1   subr (function endred 0)  pname "ENDREAD"))
    (")PJ34"    (-1   apval1 (h12)  pname "EOF"))
    (")PJ35"    (-1   apval1 (h72)  pname "EOR"))
    (")030"     (-1   subr (function eq     2)  pname "EQ"))
    (")PJ5"     (-1   pname "EQSIGN" apval1 (h13)))
    (")032"     (-1   subr (function equal  2)  pname "EQUAL"))
    (")034"     (-1   subr (function error1 1)  pname "ERROR"))
    (")PJ4"     (-1   subr (function eror1  0)  pname "ERROR1"))
    (")PJ41"    (-1   subr (function errset 3)  pname "ERRORSET"))
    (")035"     (-1   subr (function eval   2)  pname "EVAL"))
    (")036"     (-1  $subr (function evlis  2) $pname "EVLIS"))
    (")037"     (-1   pname "EXPR"))
    (")038"     (-1   subr (function expt   2)  pname "EXPT"))
    (")040"     (-1   pname "FEXPR"))
    (")041"     (-1   pname "FIX"))
    (ii11       (-1  $subr (function fixp   1) $pname "FIXP"))
    (")042"     (-1   pname "FLOAT"))
    (ii12       (-1  $subr (function floatp 1) $pname "FLOATP"))         
    (")043"     (-1   pname "FSUBR"))
    (")044"     (-1   pname "FUNARG"))
    (")045"     (-1  fsubr (function $lamp  0)  pname "FUNCTION"))
    (")046"     (-1  $subr (function gensym 0) $pname "GENSYM"))         
    (")231"     (-1   subr (function c$get  2)  pname "GET"))
    (")047"     (-1 $fsubr (function gogogo 1)  pname "GO"))
    (ii3        (-1  $subr (function grtrtp 2) $pname "GREATERP"))         
    (")052"     (-1   subr (function intrn1 1)  pname "INTERN"))
    (")054"     (-1  fsubr (function labp   0)  pname "LABEL"))
    (")055"     (-1   pname "LABEL"))
    (")LAP"     (-1   subr (function c$lap  2)  pname "LAP"))
    (pvv1       (-1   subr (function lshift 2)  pname "LEFTSHIFT"))
    (ii4        (-1  $subr (function lesstp 2) $pname "LESSP"))         
    (")057"     (-1  fsubr (function evlis  0)  pname "LIST"))
    (")PJ17"    (-1   subr (function liter  1)  pname "LITER"))
    (")234A"    (-1   subr (function loader 0)  pname "LOAD"))
    (")PJ37"    (-1  fsubr (function logand 0)  pname "LOGAND"))
    (")PJ36"    (-1  fsubr (function logor  0)  pname "LOGOR"))
    (")PJ38"    (-1  fsubr (function logxor 0)  pname "LOGXOR"))
    (")PJ7"     (-1   pname "LPAR" apval1 (h74)))
    (")065"     (-1   subr (function mapcar 2)  pname "MAP"))
    (")069B"    (-1   subr (function mapcon 2)  pname "MAPCON"))
    (")069A"    (-1   subr (function maplis 2)  pname "MAPLIST"))
    (ii7        (-1 $fsubr (function max    2) $pname "MAX"))
    (ii8        (-1 $fsubr (function min    2) $pname "MIN"))
    (")070"     (-1  $subr (function mnsprg 1) $pname "MINUS"))         
    (ii16       (-1  $subr (function minusp 1) $pname "MINUSP"))         
    (")PJ26"    (-1   subr (function mknam  0)  pname "MKNAM"))
    (")071"     (-1   subr (function nconc  2)  pname "NCONC"))
    (")074"     (-1  $subr (function nots   1) $pname "NOT"))         
    (")075"     (-1   subr (function nullp  1)  pname "NULL"))
    (ii13       (-1  $subr (function numbrp 1) $pname "NUMBERP"))         
    (")PJ25"    (-1   subr (function numob  0)  pname "NUMOB"))
    (")079A"    (-1   apval1 (-oblist) pname "OBLIST"))
    (")PJ28"    (-1   pname "OCTAL" ))
    (ii9        (-1  $subr (function onep   1) $pname "ONEP"))
    (")PJ18"    (-1   subr (function opchar 1)  pname "OPCHAR"))
    (")079"     (-1  fsubr (function $evor  0) $pname "OR"))
    (")PJ24"    (-1   subr (function pack   1)  pname "PACK"))
    (")080"     (-1   subr (function pair   2)  pname "PAIR"))
    (")234C"    (-1   subr (function pausef 0)  pname "PAUSE"))
    (")PJ9"     (-1   pname "PERIOD" apval1 (h33)))
    (")234B"    (-1   subr (function pshldb 0)  pname "PLB"))
    (")081"     (-1 $fsubr (function addp   2) $pname "PLUS"))
    (")PJ11"    (-1   pname "PLUSS" apval1 (h20)))
    (")083"     (-1   pname "PNAME"))
    (")PJ33"    (-1   subr (function $prin1 1)  pname "PRIN1"))
    (")087"     (-1   subr (function print  1)  pname "PRINT"))
    (")PJ39"    (-1   subr (function $prin2 1)  pname "PRINT2"))
    (")089"     (-1  fsubr (function inter  0)  pname "PROG"))
    (ij05       (-1  $subr (function $punch 1) $pname "PUNCH"))         
    (")090"     (-1   subr (function aprop  3)  pname "PROP"))
    (")094"     (-1  fsubr (function carp   0)  pname "QUOTE"))
    (ij03       (-1  $subr (function quoten 2) $pname "QUOTIENT"))         
    (")096"     (-1   subr (function read   0)  pname "READ"))
    (ii18       (-1  $subr (function rcpprg 1) $pname "RECIP"))         
    (")234D"    (-1   subr (function reclam 0)  pname "RECLAIM"))
    (ij04       (-1  $subr (function remain 2) $pname "REMAINDER"))         
    (")250"     (-1   subr (function remprp 2)  pname "REMPROP"))
    (")I02"     (-1  $subr (function return 1) $pname "RETURN"))         
    (")100"     (-1   subr (function rplaca 0)  pname "RPLACA"))
    (")101"     (-1   subr (function rplacd 0)  pname "RPLACD"))
    (")PJ8"     (-1   pname "RPAR" apval1 (h34)))
    (")SPCL"    (-1   pname "SPECIAL"))
    (")MOV"     (-1   pname "*MOVE"   sym (asm mze -c$mov)))
    (")RTRN"    (-1   pname "*RETURN" sym (asm mze -c$rtrn)))
    (")LST"     (-1   pname "*LIST"   sym (asm mze -c$lstr)))
    (")I06"     (-1   subr (function apssoc 3)  pname "SASSOC"))
    (")236"     (-1   subr (function search 4)  pname "SEARCH"))
    (")107"     (-1  $subr (function setp   2) $pname "SET"))         
    (")108"     (-1 $fsubr (function setqp  0)  pname "SETQ"))
    (")PJ14"    (-1   pname "SLASH" apval1 (h61)))
    (")109"     (-1   subr (function speak 4)  pname "SPEAK"))
    (")111"     (-1   pname "STOP"))
    (")PJ15"    (-1   pname "STAR" apval1 (h54)))
    (")PJ21"    (-1   subr (function stread 0)  pname "STARTREAD"))
    (ii15       (-1  $subr (function sub1   2) $pname "SUB1"))         
    (")113"     (-1   pname "SUBR"))
    (")114"     (-1   subr (function sublis 2)  pname "SUBLIS"))
    (")115"     (-1   subr (function subst  3)  pname "SUBST"))
    (")SYM"     (-1   pname "SYM"))
    (")PJ23"    (-1   subr (function terpri 0)  pname "TERPRI"))
    (")122"     (-1   subr (function $time  0)  pname "TEMPUS-FUGIT"))
    (")124"     (-1 $fsubr (function mult   2) $pname "TIMES"))
    (")213"     (-1  $pname "TRACE"))
    (")127"     (-1   subr (function uncont 0)  pname "UNCOUNT"))
    (")PJ31"    (-1   subr (function unpack 1)  pname "UNPACK"))
    (ii10       (-1  $subr (function zerop  1) $pname "ZEROP"))
    ))


          
(defun generate-plists ()
  (setf *line* -1)
  (dolist (item +symbols+)
    (generate-plist item :id "GPLI" :comment nil)
    (punch "*")))




(defparameter +alphabet+
  '( ;;                                    PNAME
    (#o00  digit         "0"              "0")
    (#o01  digit         "1"              "1")
    (#o02  digit         "2"              "2")
    (#o03  digit         "3"              "3")
    (#o04  digit         "4"              "4")
    (#o05  digit         "5"              "5")
    (#o06  digit         "6"              "6")
    (#o07  digit         "7"              "7")
    (#o10  digit         "8"              "8")
    (#o11  digit         "9"              "9")
    (#o12  other         "END OF FILE"    "$EOF$"  apval1 (h12))
    (#o13  operation     "="              "=")
    (#o14  operation     "8-4 MINUS"      "\"")
    (#o15  illegal       "ILLEGAL"        "$IL15$")
    (#o16  illegal       "ILLEGAL"        "$IL16$")
    (#o17  illegal       "ILLEGAL"        "$IL17$")
    (#o20  operation     "+"              "+")
    (#o21  letter        "A"              "A")
    (#o22  letter        "B"              "B")
    (#o23  letter        "C"              "C")
    (#o24  letter        "D"              "D")
    (#o25  letter        "E"              "E")
    (#o26  letter        "F"              "F"      apval (0))
    (#o27  letter        "G"              "G")
    (#o30  letter        "H"              "H")
    (#o31  letter        "I"              "I")
    (#o32  other         "+0"             "$IL32$")
    (#o33  other         "."              ".")
    (#o34  other         ")"              ")")
    (#o35  illegal       "ILLEGAL"        "$IL35$")
    (#o36  illegal       "ILLEGAL"        "$IL36$")
    (#o37  illegal       "ILLEGAL"        "$IL37$")
    (#o40  operation     "11 MINUS"       "-")
    (#o41  letter        "J"              "J")
    (#o42  letter        "K"              "K")
    (#o43  letter        "L"              "L")
    (#o44  letter        "M"              "M")
    (#o45  letter        "N"              "N")
    (#o46  letter        "O"              "O")
    (#o47  letter        "P"              "P")
    (#o50  letter        "Q"              "Q")
    (#o51  letter        "R"              "R")
    (#o52  other         "-0"             "$IL52$")
    (#o53  other         "$"              "$")
    (#o54  operation     "*"              "*"      sym -c$star)
    (#o55  illegal       "ILLEGAL"        "$IL55$")
    (#o56  illegal       "ILLEGAL"        "$IL56$")
    (#o57  illegal       "ILLEGAL"        "$IL57$")
    (#o60  other         "BLANK"          " ")
    (#o61  operation     "/"              "/")
    (#o62  letter        "S"              "S")
    (#o63  letter        "T"              "T"      apval (1))
    (#o64  letter        "U"              "U")
    (#o65  letter        "V"              "V")
    (#o66  letter        "W"              "W")
    (#o67  letter        "X"              "X")
    (#o70  letter        "Y"              "Y")
    (#o71  letter        "Z"              "Z")
    (#o72  other         "END OF RECORD"  "$EOR$"  apval1 (h72))
    (#o73  other         ","              ",")
    (#o74  other         "("              "(")
    (#o75  illegal       "ILLEGAL"        "$IL75$")
    (#o76  illegal       "ILLEGAL"        "$IL76$")
    (#o77  illegal       "ILLEGAL"        "$IL77$")))


(defparameter +stars-sep+
  "**         * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *")

(generate-oblb)
(generate-plists)

(defun generate-plao ()
  (loop initially (setf *line* -1)
        for (code type comment pname . plist) in +alphabet+
        for label = (format nil "HH~2,'0O" code)
        do (if (eq type 'digit)
             (progn
               (punch (format nil "~6a ~7a ~o"
                              label (if (< code 8) "" "OCT") code) "GPLA")
               (punch +stars-sep+ "GPLA"))
             (progn
               (punch +stars-sep+ "GPLA")
               (generate-plist (list label
                                     `(,@(when (member code '(#o26 #o63)) '(-1))
                                       pname ,pname ,@plist))
                               :id "GPLA" :comment comment)
               )))
  (punch "*" "GPLA")
  (punch "*" "GPLA")
  (loop for code from #o77 downto 0 do
        (punch (format nil "~6A ~7A ~A"
                       (format nil ")H~2,'0O" code) ""
                       (format nil "-1,~A,-HH~2,'0O"
                               (if (< code 10) 1 "") code)) "GPLA"))
  (format *code-output* 
"UPERML BSS     0
       EJECT                                                            PAGE 217
       EJECT                                                            PAGE 218
       HEAD    0
*      SYN CARDS CAUSE MANY SYMBOLS TO HAVE O-HEADED EQUIVALENTS
*
")
  (loop for code from 0 upto #o77 do
        (punch (format nil "~6A ~7A ~A"
                       (format nil "H~2,'0O" code)
                       "SYN"
                       (format nil "-)H~2,'0O"  code)) "GPLA")))


(defun generate-syns ()
  (setf *line* -1)
  (dolist (item +symbols+)
    (let* ((tag (first item))
           (plist (cdr (second item)))
           (name (second (find 'function plist
                               :key (lambda (x) (when (consp x) (car x)))))))
      (when name
        (punch (format nil "~6A ~7A ~A" name "SYN" (format nil "-~A" tag))
               "GSYN")))))



(defun generate-pjs ()
  (setf *line* -1)
  (loop for n from 1 to 39
        for name = (format nil "PJ~D" n)
        do (punch  (format nil "~6A ~7A ~A" name "SYN"
                          (format nil "-)~A" name)) "")))

;; (generate-oblb)
 (generate-plists)
;; (generate-plao)
;; (generate-syns)
;; (generate-pjs)

;;;; THE END ;;;;
