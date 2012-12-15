;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl:in-package :common-lisp-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage "IBM7090"
  (:use "COMMON-LISP")
  (:SHADOW "CODE-CHAR" "CHAR-CODE")
  (:EXPORT "+CHARSET+"  "CODE-CHAR" "CHAR-CODE"
   "CONVERT-STRING-TO-WORDS" "CONVERT-STRING-FROM-WORDS"
   "CONVERT-BYTES-TO-WORDS"  "CONVERT-BYTES-FROM-WORDS"))
(cl:in-package "IBM7090")


;; Normal encoding:
;;
;; (defparameter +charset+
;;   "0123456789#=\"###+ABCDEFGHI%.)###-JKLMNOPQRØ'*### /STUVWXYZ±,(###"
;;   "Maps lisp characters to IBM7090 character code (position of the character
;;    in the string.  Invalid character codes are denoted by '#'.")


;; LISP1.5 encoding:
(defparameter +charset+
  "0123456789#=\"###+ABCDEFGHI%.)###-JKLMNOPQRØ$*### /STUVWXYZ±,(###"
  "Maps lisp characters to IBM7090 character code (position of the character
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
              (error "Character '~C' cannot be encoded to IBM7090." ch))
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



;;  http://www.frobenius.com/instruction-formats.htm

(defparameter +op-codes+
  '(

    ;; page 20.
    (CLA +0500 f t y "Clear and Add")
    (CAL -0500 f t y "Clear and Add Logical Word")
    (CLS +0502 f t y "Clear and Substract")
    (ADD +0400 f t y "Add")
    (ADM +0401 f t y "Add Magnitude")
    ;; page 21.
    (SUB +0402 f t y "Substract")
    (SBM -0400 f t y "Substract Magnitude")
    (ACL +0361 f t y "Add and Carry Logical Word")
    ;; page 22.
    (MPY +0200 f t y "Multiply")
    (MPR -0200 f t y "Multiply and Round")
    (RND +0760 t 00010 "Round" )
    (VLM +0204 c t t "Variable Length Multiply")
    ;; page 24.
    (DVH +0220 f t y "Divide or Halt")
    (DVP +0221 f t y "Divide or Proceed")
    (VDH +0224 c t y "Variable Lengh Divide or Halt")
    (VDP +0225 c t y "Variable Lengh Divide or Proceed")
    ;; page 27.
    (FAD +0300 f t y "Floating Add")
    (FAM +0304 f t y "Floating Add Magnitude")
    (UFA -0300 f t y "Unnormalized Floating Add")
    (FSB +0302 f t y "Floating Substract")
    ;; page 28.
    (UAM -0304 f t y "Unnormalized Add Magnitude")
    (FSM +0306 f t y "Floating Substract Magnitude")
    (UFS -0302 f t y "Unnormalized Floating Substract")
    (USM -0306 f t y "Unnormalized Floating Substract Magnitude")
    (FRN +0760 t 00011 "Floating Round")
    ;; page 29.
    (FMP +0260 f t y "Floating Multiply")
    (UFM -0260 f t y "Unnormalized Floating Multiply")
    ;; page 30.
    (FDH +0240 f t y "Floating Divide or Halt")
    (FDP +0241 f t y "Floating Divide or Proceed")
    ;; page 31.
    (ALS +0767 t y "Accumulator Left Shift")
    (ARS +0771 t y "Accumulator Right Shift")
    (LLS +0763 t y "Long Left Shift")
    (LRS +0765 t y "Long Right Shift")
    (LGL -0763 t y "Logical Left Shift")
    (LGR -0765 t y "Logical Right Shift")
    (RQL -0773 t y "Rotate MQ Left")
    (LDQ +0560 f t y "Load MQ")
    (STQ -0600 f t y "Store MQ")
    (SLQ -0620 f t y "Store Left Half MQ")    ; (setcdr! y (cdr mq))
    (STO +0601 f t y "Store")
    (SLW +0602 f t y "Store Logical Word")
    (STP +0630 f t y "Store Prefix")
    (STD +0622 f t y "Store Decrement")       ; (setcdr! y (cdr a))
    (STT +0625 f t y "Store Tag")
    (STA +0621 f t y "Store Address")         ; (setcar! y (car a))
    (STL -0625 f t y "Store Instruction Location Counter")
    (STR -1 "Store Location and Trap")
    (STZ +0600 f t y "Store Zero")
    (XCA +0131 "Exchange AC and MQ")
    (XCL -0130 "Exclange Logical AC and MQ")
    (ENK +0760 t 00004 "Enter Keys")
    (HTR +0000 f t y "Halt and Transfer")
    (NOP +0761 "No Operation")
    (HPR +0420 "Halt and Proceed")
    (XEC +0522 f t y "Execute")
    (TRA +0020 f t y "Transfer")
    (ETM +0760 t 00007 "Enter Trap Mode")
    (LTM -0760 t 00007 "Leave Trap Mode")
    (TTR +0021 f t y "Trap Transfer")
    (TZE +0100 f t y "Transfer on Zero")
    (TNZ -0100 f t y "Transfer on No Zero")
    (TPL +0120 f t y "Transfer on Plus")
    (TMI -0120 f t y "Transfer on Minus")
    (TOV +0140 f t y "Transfer on Overflow")
    (TNO -0140 f t y "Transfer on No Overflow")
    (TQP +0162 f t y "Transfer on MQ Plus")
    (TQO +0161 f t y "Transfer on MQ Overflow" "704 floating point mode")
    ;; page 39.
    (TLQ +0040 f t y "Transfer on Low MQ")
    (TSX +0074 t y "Transfer and Set Index")
    (TXI +1 d t y "Transer with Index Incremented")
    (TXH +3 d t y "Transfer on Index High")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl:in-package :common-lisp-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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










(DEFPACKAGE "LISP15-PRIMITIVES"
  (:USE "COMMON-LISP")
  (:shadow "ERROR")
  (:EXPORT
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
(cl:in-package "LISP15-PRIMITIVES")

(defmacro error (message &optional in-extenso)
  `(common-lisp:error message))


(DEFPACKAGE "LISP15"
  (:USE "LISP15-PRIMITIVES"))
(cl:in-package "LISP15")

  
(DEFUN CONSW (WORD)
  "
WORD can be a cons cell, nil or a fixnum, or a char, or a float,
or a pointer to a symbol or an array,
or, a pointer to something else
"
  (COND
   ((CONSP WORD) (CONS (CAR WORD) (CDR WORD)))
   (WORD)))


;; * AC := CONSW(AC)
;; * AC := CP1(AC)


;;            FUNCTION CP1
;;            CP1(L)=(L=0 YIELDS 0.
;;                   OTHERWISE CONS(CONSW(CWR(CAR(L)))),CP1(CDR(L))))

 
(DEFUN CP1 (L)
  (COND ((NULL L) NIL)
        (T (CONS (CONSW (CAR L)) (CP1 (CDR L))))))



;;     SUBST(L,V,M) =
;;     (M = 0 YIELDS 0,
;;        EQUAL(M,V) YIELDS COPY(L),
;;        CAR(M)=-1 YIELDS M 
;;        1 YIELDS CONS(SUBST(L,V,CAR(M)),SUBSTL,V,CDR(M))))

(DEFUN SUBST (L V M)
  (COND
   ((NULL M)    NIL)
   ((EQUAL M V) (COPY L))
   ((ATOM M)    M)
   (T           (CONS (SUBST L V (CAR M)) (SUBST L V (CDR M))))))


;; SUBLISP

;;            APPEND(L1,L2)=
;;            (L1=0 YIELDS L2,1 YIELDS CONS(CAR(L1),APPEND(CDR(L1),L2))

(DEFUN APPEND (L1 L2)
  (COND ((NULL L1) L2)
        (T (CONS (CAR L1) (APPEND (CDR L1) L2)))))


(DEFUN PAIR (key data)
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

(defconstant +dmask+ #o000000077777)
(DEFCONSTANT  LETTR 23)
(DEFPARAMETER DIGIT 0)


(defparameter ac 0)
(defparameter mq 0)

(DEFUN GENSYM ()
  (SETF DIGIT (+ DIGIT 1))
  (cons
   +dmask+
   (cons
    :pnamed
    (cons
     (cons
      (cons
       (FORMAT NIL "~C~5,'0O"
               (AREF ibm7090:+CHARSET+ (or 23 (truncate digit #o100000)))
               (MOD digit #o100000))
       mq) nil) nil))))
  













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl:in-package :common-lisp-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun generate-oblb ()
  (let ((line -1))
    (DOLIST
        (ITEM
         `(("    -II14" ADD 1)
           ("    -)ALST")
           AND F1 F18 APVAL
           ("    -II1" ARRAY)
           ATOM F29 CAR CDR CAAR CDAR CADR CDDR CAAAR CAADR CADAR CADDR
           CDAAR CDADR CDDAR CDDDR COND CONSN COPYN
           ("    DUMP")
           F12 F35
           ("    -IJ01" DIFFER)
           ("    -IJ02" DIVIDE)
           EQ F8 F21 F19 EVLISL EXPR F32 FEXPR
           FIX    ("    -II11" FIX P)
           FLOAT  ("    -II12" FLOAT P)
           FSUBR FUNARG FUNCT SYMGEN GO
           ("    -II3" GREATER THAN P)
           F16 LABEL LAMBDA 
           ("    LAP")
           ("    -II4" LESS THAN P)
           LIST
           ("    LOADA" LOADER OBJECT)
           PMAPCA
           ("-)069B")
           ("-)069A")
           ("    -II7" MAXIMUM)
           ("    -II8" MINIMUM)
           MINUS
           ("    -II16" MINUS P)
           F3 NIL NOT NULL
           ("    -II13" NUMBER P)
           ("    OBLBA" OBLIST OBJECT)
           ("    -II9" ONE P)
           OR F2 PAUSE PLB PLUS PNAME F4 PROG PROPO 
           ("    -IJ05" PUNCH)
           QUOTE
           ("    -IJ03" QUOTIENT)
           F13
           ("    -II18" RECIP)
           RCLAM PRPLCA PRPLCD
           ("    -IJ04" REMAINDER)
           ("    RETATM" RETURN)
           SASCO SRCH SET SETQ F34 STOP SUBR
           ("    TRACE")
           ("    SMOVE")
           ("    SRETUR")
           ("    SLIST")
           ("    SPECAL")
           ("    -II15" SUBTRACT 1)
           F17 F30
           ("    1" *T* BINARY TRUE ATOM)
           F27
           ("    SYM")
           TIMES F36
           ("    -II10" ZERO P)
           CGET
           ("    REMPP")
           ,@(LOOP FOR I BELOW 64
                   COLLECT (FORMAT NIL "H~2,'0O" (CASE I
                                                   ((#O15) #O14) ((#O14) #O15)
                                                   (OTHERWISE I))))
           ,@(LOOP FOR I FROM 1 TO 39
                   UNLESS (MEMBER I '(3 13 20 22 29))
                   COLLECT (FORMAT NIL "~APJ~D" (IF (< I 24) "" "    ") I))
           ("    ERSETO" ERRORSET)
           ("    PVW1" "LAST OBJECT   - LEFTSHIFT")
           ))
      (format t "~72AGENER~3,'0D~%" 
              (IF (ATOM  ITEM)
                (FORMAT NIL "           ~A"
                        (FORMAT NIL "~A,,-*-1" ITEM))
                (FORMAT NIL "           ~22A~{ ~A~}"
                        (FORMAT NIL "~A,,-*-1" (CAR ITEM)) (CDR ITEM)))

              (incf line)))))


(defun string-to-list (string)
  (coerce (ibm7090:convert-string-to-words string) 'list))



(defparameter *code-output* t "Stream where code is output")
(defparameter *code* nil)
(DEFPARAMETER *LINE* -1)
(defparameter *id* "")

(defun geninit (id)
  (setf *code* (make-array (list 50) :adjustable t :fill-pointer 1)
        *id*   id))

(defun generate (label cop car tag cdr comment)
  (vector-push-extend (list label cop car tag cdr comment) *code*))

(DEFUN PUNCH (LINE &optional (ID *id*))
  (FORMAT *CODE-OUTPUT* "~72A~4A~4,'0D~%" LINE ID (incf *line*)))


(defun print-code (addr code)
  (PUNCH 
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
         (format T "~:[~;~:*~A~]" comment))))))


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
    (CASE (FIRST ITEM)
      ((function)
       (generate label
                 'pze `(address
                         ,(generate nil 'txl (second item)
                                    nil (third item) nil))
                 nil `(address ,next) comment))
      ((ASM)
       (generate label (SECOND ITEM) (THIRD ITEM) (FOURTH ITEM) (FIFTH ITEM)
                 COMMENT))
      (OTHERWISE
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
  (LET ((LABEL (FIRST ITEM))
        (PLIST (SECOND ITEM)))
    (GENINIT id)
    (GENERATE-LIST PLIST :comment comment)
    (SETF (FIRST (AREF *CODE* (1- (FILL-POINTER *CODE*)))) LABEL)
    (LOOP FOR PC FROM (1- (FILL-POINTER *CODE*)) DOWNTO 1
          FOR LINE = (AREF *CODE* PC)
          DO (PRINT-CODE PC LINE))))

(defparameter +symbols+
  `((II14       (-1  $SUBR (FUNCTION ADD1   1) $PNAME "ADD1"))
    (")PJ2"     (-1   SUBR (FUNCTION ADVANC 0)  PNAME "ADVANCE"))
    (")ALST"    (-1   PNAME"$ALIST" SYM -C$ALST))
    (")002"     (-1  FSUBR (FUNCTION $EVAND 0) $PNAME "AND"))
    (")003"     (-1   SUBR (FUNCTION APPEND 2)  PNAME "APPEND"))
    (")004"     (-1   SUBR (FUNCTION APPLY  3)  PNAME "APPLY"))
    (")005"     (-1   PNAME "APVAL"))
    ("II1"      (-1   SUBR (FUNCTION ARYMAK 1)  PNAME "ARRAY"))
    (")007"     (-1   SUBR (FUNCTION ATOMP  1)  PNAME "ATOM"))
    (")008"     (-1   SUBR (FUNCTION ATTRIB 2)  PNAME "ATTRIB"))
    (")PJ12"    (-1   PNAME "BLANK" APVAL1 (H60)))
    (")011"     (-1   SUBR (FUNCTION CARP   1)  PNAME "CAR"))
    (")012"     (-1   SUBR (FUNCTION CDRP   1)  PNAME "CDR"))
    (")201"     (-1   SUBR (FUNCTION CAARXX 1)  PNAME "CAAR"))
    (")202"     (-1   SUBR (FUNCTION CDARXX 1)  PNAME "CDAR"))
    (")203"     (-1   SUBR (FUNCTION CADRXX 1)  PNAME "CADR"))
    (")204"     (-1   SUBR (FUNCTION CDDRXX 1)  PNAME "CDDR"))
    (")205"     (-1   SUBR (FUNCTION CAAARX 1)  PNAME "CAAAR"))
    (")206"     (-1   SUBR (FUNCTION CAADRX 1)  PNAME "CAADR"))
    (")207"     (-1   SUBR (FUNCTION CADARX 1)  PNAME "CADAR"))
    (")208"     (-1   SUBR (FUNCTION CADDRX 1)  PNAME "CADDR"))
    (")209"     (-1   SUBR (FUNCTION CDAARX 1)  PNAME "CDAAR"))
    (")210"     (-1   SUBR (FUNCTION CDADRX 1)  PNAME "CDADR"))
    (")211"     (-1   SUBR (FUNCTION CDDARX 1)  PNAME "CDDAR"))
    (")212"     (-1   SUBR (FUNCTION CDDDRX 1)  PNAME "CDDDR"))
    (")PJ32"    (-1   PNAME "CHARCOUNT" APVAL1 (((ASM MZE -1 1 -CHACT)))))
    (")PJ27"    (-1   SUBR (FUNCTION CLEAR  0)  PNAME "CLEARBUFF"))
    (")PJ6"     (-1   PNAME "COMMA" APVAL1 (H73)))
    (")016"     (-1  FSUBR (FUNCTION $EVCON 0)  PNAME "COND"))
    (")017"     (-1   SUBR (FUNCTION CONS   2)  PNAME "CONS"))
    (")019"     (-1   SUBR (FUNCTION CP1    1)  PNAME "CP1"))
    (")020"     (-1   SUBR (FUNCTION $COPY  1)  PNAME "COPY"))
    (")021"     (-1   SUBR (FUNCTION COUNT  0)  PNAME "COUNT"))
    (")PJ1"     (-1   APVAL1 -CURC1  PNAME "CURCHAR" SPECAL -CURC))
    (")PJ16"    (-1   APVAL1 (H40)   PNAME "DASH"))
    (IJ01       (-1  $SUBR (FUNCTION DIFFER 2) $PNAME "DIFFERENCE"))
    (")PJ19"    (-1   SUBR (FUNCTION DIGIT  1)  PNAME "DIGIT"))
    (IJ02       (-1  $SUBR (FUNCTION DIVIDE 2) $PNAME "DIVIDE"))
    (")PJ10"    (-1   PNAME "DOLLAR" APVAL1 (H53)))
    (DMPCB      (-1   SUBR (FUNCTION DUMPXX 4)  PNAME "DUMP"))
    (")PJ30"    (-1   SUBR (FUNCTION ENDRED 0)  PNAME "ENDREAD"))
    (")PJ34"    (-1   APVAL1 (H12)  PNAME "EOF"))
    (")PJ35"    (-1   APVAL1 (H72)  PNAME "EOR"))
    (")030"     (-1   SUBR (FUNCTION EQ     2)  PNAME "EQ"))
    (")PJ5"     (-1   PNAME "EQSIGN" APVAL1 (H13)))
    (")032"     (-1   SUBR (FUNCTION EQUAL  2)  PNAME "EQUAL"))
    (")034"     (-1   SUBR (FUNCTION ERROR1 1)  PNAME "ERROR"))
    (")PJ4"     (-1   SUBR (FUNCTION EROR1  0)  PNAME "ERROR1"))
    (")PJ41"    (-1   SUBR (FUNCTION ERRSET 3)  PNAME "ERRORSET"))
    (")035"     (-1   SUBR (FUNCTION EVAL   2)  PNAME "EVAL"))
    (")036"     (-1  $SUBR (FUNCTION EVLIS  2) $PNAME "EVLIS"))
    (")037"     (-1   PNAME "EXPR"))
    (")038"     (-1   SUBR (FUNCTION EXPT   2)  PNAME "EXPT"))
    (")040"     (-1   PNAME "FEXPR"))
    (")041"     (-1   PNAME "FIX"))
    (ii11       (-1  $SUBR (FUNCTION FIXP   1) $PNAME "FIXP"))
    (")042"     (-1   PNAME "FLOAT"))
    (II12       (-1  $SUBR (FUNCTION FLOATP 1) $PNAME "FLOATP"))         
    (")043"     (-1   PNAME "FSUBR"))
    (")044"     (-1   PNAME "FUNARG"))
    (")045"     (-1  FSUBR (FUNCTION $LAMP  0)  PNAME "FUNCTION"))
    (")046"     (-1  $SUBR (FUNCTION GENSYM 0) $PNAME "GENSYM"))         
    (")231"     (-1   SUBR (FUNCTION C$GET  2)  PNAME "GET"))
    (")047"     (-1 $FSUBR (FUNCTION GOGOGO 1)  PNAME "GO"))
    (II3        (-1  $SUBR (FUNCTION GRTRTP 2) $PNAME "GREATERP"))         
    (")052"     (-1   SUBR (FUNCTION INTRN1 1)  PNAME "INTERN"))
    (")054"     (-1  FSUBR (FUNCTION LABP   0)  PNAME "LABEL"))
    (")055"     (-1   PNAME "LABEL"))
    (")LAP"     (-1   SUBR (FUNCTION C$LAP  2)  PNAME "LAP"))
    (PVV1       (-1   SUBR (FUNCTION LSHIFT 2)  PNAME "LEFTSHIFT"))
    (II4        (-1  $SUBR (FUNCTION LESSTP 2) $PNAME "LESSP"))         
    (")057"     (-1  FSUBR (FUNCTION EVLIS  0)  PNAME "LIST"))
    (")PJ17"    (-1   SUBR (FUNCTION LITER  1)  PNAME "LITER"))
    (")234A"    (-1   SUBR (FUNCTION LOADER 0)  PNAME "LOAD"))
    (")PJ37"    (-1  FSUBR (FUNCTION LOGAND 0)  PNAME "LOGAND"))
    (")PJ36"    (-1  FSUBR (FUNCTION LOGOR  0)  PNAME "LOGOR"))
    (")PJ38"    (-1  FSUBR (FUNCTION LOGXOR 0)  PNAME "LOGXOR"))
    (")PJ7"     (-1   PNAME "LPAR" APVAL1 (H74)))
    (")065"     (-1   SUBR (FUNCTION MAPCAR 2)  PNAME "MAP"))
    (")069B"    (-1   SUBR (FUNCTION MAPCON 2)  PNAME "MAPCON"))
    (")069A"    (-1   SUBR (FUNCTION MAPLIS 2)  PNAME "MAPLIST"))
    (II7        (-1 $FSUBR (FUNCTION MAX    2) $PNAME "MAX"))
    (II8        (-1 $FSUBR (FUNCTION MIN    2) $PNAME "MIN"))
    (")070"     (-1  $SUBR (FUNCTION MNSPRG 1) $PNAME "MINUS"))         
    (II16       (-1  $SUBR (FUNCTION MINUSP 1) $PNAME "MINUSP"))         
    (")PJ26"    (-1   SUBR (FUNCTION MKNAM  0)  PNAME "MKNAM"))
    (")071"     (-1   SUBR (FUNCTION NCONC  2)  PNAME "NCONC"))
    (")074"     (-1  $SUBR (FUNCTION NOTS   1) $PNAME "NOT"))         
    (")075"     (-1   SUBR (FUNCTION NULLP  1)  PNAME "NULL"))
    (II13       (-1  $SUBR (FUNCTION NUMBRP 1) $PNAME "NUMBERP"))         
    (")PJ25"    (-1   SUBR (FUNCTION NUMOB  0)  PNAME "NUMOB"))
    (")079A"    (-1   APVAL1 (-OBLIST) PNAME "OBLIST"))
    (")PJ28"    (-1   PNAME "OCTAL" ))
    (II9        (-1  $SUBR (FUNCTION ONEP   1) $PNAME "ONEP"))
    (")PJ18"    (-1   SUBR (FUNCTION OPCHAR 1)  PNAME "OPCHAR"))
    (")079"     (-1  FSUBR (FUNCTION $EVOR  0) $PNAME "OR"))
    (")PJ24"    (-1   SUBR (FUNCTION PACK   1)  PNAME "PACK"))
    (")080"     (-1   SUBR (FUNCTION PAIR   2)  PNAME "PAIR"))
    (")234C"    (-1   SUBR (FUNCTION PAUSEF 0)  PNAME "PAUSE"))
    (")PJ9"     (-1   PNAME "PERIOD" APVAL1 (H33)))
    (")234B"    (-1   SUBR (FUNCTION PSHLDB 0)  PNAME "PLB"))
    (")081"     (-1 $FSUBR (FUNCTION ADDP   2) $PNAME "PLUS"))
    (")PJ11"    (-1   PNAME "PLUSS" APVAL1 (H20)))
    (")083"     (-1   PNAME "PNAME"))
    (")PJ33"    (-1   SUBR (FUNCTION $PRIN1 1)  PNAME "PRIN1"))
    (")087"     (-1   SUBR (FUNCTION PRINT  1)  PNAME "PRINT"))
    (")PJ39"    (-1   SUBR (FUNCTION $PRIN2 1)  PNAME "PRINT2"))
    (")089"     (-1  FSUBR (FUNCTION INTER  0)  PNAME "PROG"))
    (IJ05       (-1  $SUBR (FUNCTION $PUNCH 1) $PNAME "PUNCH"))         
    (")090"     (-1   SUBR (FUNCTION APROP  3)  PNAME "PROP"))
    (")094"     (-1  FSUBR (FUNCTION CARP   0)  PNAME "QUOTE"))
    (IJ03       (-1  $SUBR (FUNCTION QUOTEN 2) $PNAME "QUOTIENT"))         
    (")096"     (-1   SUBR (FUNCTION READ   0)  PNAME "READ"))
    (II18       (-1  $SUBR (FUNCTION RCPPRG 1) $PNAME "RECIP"))         
    (")234D"    (-1   SUBR (FUNCTION RECLAM 0)  PNAME "RECLAIM"))
    (IJ04       (-1  $SUBR (FUNCTION REMAIN 2) $PNAME "REMAINDER"))         
    (")250"     (-1   SUBR (FUNCTION REMPRP 2)  PNAME "REMPROP"))
    (")I02"     (-1  $SUBR (FUNCTION RETURN 1) $PNAME "RETURN"))         
    (")100"     (-1   SUBR (FUNCTION RPLACA 0)  PNAME "RPLACA"))
    (")101"     (-1   SUBR (FUNCTION RPLACD 0)  PNAME "RPLACD"))
    (")PJ8"     (-1   PNAME "RPAR" APVAL1 (H34)))
    (")SPCL"    (-1   PNAME "SPECIAL"))
    (")MOV"     (-1   PNAME "*MOVE"   SYM (ASM MZE -C$MOV)))
    (")RTRN"    (-1   PNAME "*RETURN" SYM (ASM MZE -C$RTRN)))
    (")LST"     (-1   PNAME "*LIST"   SYM (ASM MZE -C$LSTR)))
    (")I06"     (-1   SUBR (FUNCTION APSSOC 3)  PNAME "SASSOC"))
    (")236"     (-1   SUBR (FUNCTION SEARCH 4)  PNAME "SEARCH"))
    (")107"     (-1  $SUBR (FUNCTION SETP   2) $PNAME "SET"))         
    (")108"     (-1 $FSUBR (FUNCTION SETQP  0)  PNAME "SETQ"))
    (")PJ14"    (-1   PNAME "SLASH" APVAL1 (H61)))
    (")109"     (-1   SUBR (FUNCTION SPEAK 4)  PNAME "SPEAK"))
    (")111"     (-1   PNAME "STOP"))
    (")PJ15"    (-1   PNAME "STAR" APVAL1 (H54)))
    (")PJ21"    (-1   SUBR (FUNCTION STREAD 0)  PNAME "STARTREAD"))
    (II15       (-1  $SUBR (FUNCTION SUB1   2) $PNAME "SUB1"))         
    (")113"     (-1   PNAME "SUBR"))
    (")114"     (-1   SUBR (FUNCTION SUBLIS 2)  PNAME "SUBLIS"))
    (")115"     (-1   SUBR (FUNCTION SUBST  3)  PNAME "SUBST"))
    (")SYM"     (-1   PNAME "SYM"))
    (")PJ23"    (-1   SUBR (FUNCTION TERPRI 0)  PNAME "TERPRI"))
    (")122"     (-1   SUBR (FUNCTION $TIME  0)  PNAME "TEMPUS-FUGIT"))
    (")124"     (-1 $FSUBR (FUNCTION MULT   2) $PNAME "TIMES"))
    (")213"     (-1  $PNAME "TRACE"))
    (")127"     (-1   SUBR (FUNCTION UNCONT 0)  PNAME "UNCOUNT"))
    (")PJ31"    (-1   SUBR (FUNCTION UNPACK 1)  PNAME "UNPACK"))
    (II10       (-1  $SUBR (FUNCTION ZEROP  1) $PNAME "ZEROP"))
    ))


          
(DEFUN GENERATE-PLISTS ()
  (setf *line* -1)
  (DOLIST (ITEM +symbols+)
    (generate-plist item :id "GPLI" :comment nil)
    (PUNCH "*")))




(defparameter +alphabet+
  '( ;;                                    PNAME
    (#o00  DIGIT         "0"              "0")
    (#o01  DIGIT         "1"              "1")
    (#o02  DIGIT         "2"              "2")
    (#o03  DIGIT         "3"              "3")
    (#o04  DIGIT         "4"              "4")
    (#o05  DIGIT         "5"              "5")
    (#o06  DIGIT         "6"              "6")
    (#o07  DIGIT         "7"              "7")
    (#o10  DIGIT         "8"              "8")
    (#o11  DIGIT         "9"              "9")
    (#o12  OTHER         "END OF FILE"    "$EOF$"  APVAL1 (H12))
    (#o13  OPERATION     "="              "=")
    (#o14  OPERATION     "8-4 MINUS"      "\"")
    (#o15  ILLEGAL       "ILLEGAL"        "$IL15$")
    (#o16  ILLEGAL       "ILLEGAL"        "$IL16$")
    (#o17  ILLEGAL       "ILLEGAL"        "$IL17$")
    (#o20  OPERATION     "+"              "+")
    (#o21  LETTER        "A"              "A")
    (#o22  LETTER        "B"              "B")
    (#o23  LETTER        "C"              "C")
    (#o24  LETTER        "D"              "D")
    (#o25  LETTER        "E"              "E")
    (#o26  LETTER        "F"              "F"      APVAL (0))
    (#o27  LETTER        "G"              "G")
    (#o30  LETTER        "H"              "H")
    (#o31  LETTER        "I"              "I")
    (#o32  OTHER         "+0"             "$IL32$")
    (#o33  OTHER         "."              ".")
    (#o34  OTHER         ")"              ")")
    (#o35  ILLEGAL       "ILLEGAL"        "$IL35$")
    (#o36  ILLEGAL       "ILLEGAL"        "$IL36$")
    (#o37  ILLEGAL       "ILLEGAL"        "$IL37$")
    (#o40  OPERATION     "11 MINUS"       "-")
    (#o41  LETTER        "J"              "J")
    (#o42  LETTER        "K"              "K")
    (#o43  LETTER        "L"              "L")
    (#o44  LETTER        "M"              "M")
    (#o45  LETTER        "N"              "N")
    (#o46  LETTER        "O"              "O")
    (#o47  LETTER        "P"              "P")
    (#o50  LETTER        "Q"              "Q")
    (#o51  LETTER        "R"              "R")
    (#o52  OTHER         "-0"             "$IL52$")
    (#o53  OTHER         "$"              "$")
    (#o54  OPERATION     "*"              "*"      SYM -C$STAR)
    (#o55  ILLEGAL       "ILLEGAL"        "$IL55$")
    (#o56  ILLEGAL       "ILLEGAL"        "$IL56$")
    (#o57  ILLEGAL       "ILLEGAL"        "$IL57$")
    (#o60  OTHER         "BLANK"          " ")
    (#o61  OPERATION     "/"              "/")
    (#o62  LETTER        "S"              "S")
    (#o63  LETTER        "T"              "T"      APVAL (1))
    (#o64  LETTER        "U"              "U")
    (#o65  LETTER        "V"              "V")
    (#o66  LETTER        "W"              "W")
    (#o67  LETTER        "X"              "X")
    (#o70  LETTER        "Y"              "Y")
    (#o71  LETTER        "Z"              "Z")
    (#o72  OTHER         "END OF RECORD"  "$EOR$"  APVAL1 (H72))
    (#o73  OTHER         ","              ",")
    (#o74  OTHER         "("              "(")
    (#o75  ILLEGAL       "ILLEGAL"        "$IL75$")
    (#o76  ILLEGAL       "ILLEGAL"        "$IL76$")
    (#o77  ILLEGAL       "ILLEGAL"        "$IL77$")))


(DEFPARAMETER +stars-sep+
  "**         * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *")

(generate-oblb)
(GENERATE-PLISTS)

(defun generate-plao ()
  (LOOP initially (setf *line* -1)
        FOR (code type comment pname . plist) IN +ALPHABET+
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
  (FORMAT *CODE-OUTPUT* 
"UPERML BSS     0
       EJECT                                                            PAGE 217
       EJECT                                                            PAGE 218
       HEAD    0
*      SYN CARDS CAUSE MANY SYMBOLS TO HAVE O-HEADED EQUIVALENTS
*
")
  (loop for code from 0 UPTO #o77 do
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
 (GENERATE-PLISTS)
;; (generate-plao)
;; (generate-syns)
;; (generate-pjs)