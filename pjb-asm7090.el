;;****************************************************************************
;;FILE:               7090.el
;;LANGUAGE:           emacs lisp
;;SYSTEM:             POSIX
;;USER-INTERFACE:     NONE
;;DESCRIPTION
;;    
;;    asm7090 stuff.
;;    
;;AUTHORS
;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;MODIFICATIONS
;;    2005-06-04 <PJB> Created.
;;BUGS
;;LEGAL
;;    GPL
;;    
;;    Copyright Pascal Bourguignon 2005 - 2005
;;    
;;    This program is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU General Public License
;;    as published by the Free Software Foundation; either version
;;    2 of the License, or (at your option) any later version.
;;    
;;    This program is distributed in the hope that it will be
;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;    PURPOSE.  See the GNU General Public License for more details.
;;    
;;    You should have received a copy of the GNU General Public
;;    License along with this program; if not, write to the Free
;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;    Boston, MA 02111-1307 USA
;;****************************************************************************

(eval-when-compile
  (require 'cl)
  (load "cl-seq"  t t)
  (load "cl-macs" t t))
(require 'cl)
(require 'pjb-cl)
(require 'asm-mode)
(require 'font-lock)

;; ASM 7090 mode

(defconst +asm7090-ibsys-char+   ?$)
(defconst +asm7090-comment-char+ ?*)
(defconst +space+                32)


(defun POSITION-IF (item sequence &rest keys)
  ;; (message "item=%S seq=%S keys=%S"  item sequence keys)
  (let ((start (getf keys :start))
        (end   (getf keys :end)))
    (cond
      ((and start (= start (length sequence))) nil)
      ((and end (< (length sequence) end))
       (setf keys (copy-seq keys))
       (setf (getf keys :end) (length sequence))
       (apply (function position-if) item sequence keys))
      (t
       (apply (function position-if) item sequence keys)))))


;; (defun POSITION (item sequence &rest keys)
;;   (message "item=%S seq=%S keys=%S"  item sequence keys)
;;   (apply (function position) item sequence keys))


(defun search-asm7090-fields (limit)
  (macrolet ((sel (len min max sma bet big)
               (let ((vlen (gensym)) (vmin (gensym)) (vmax (gensym)))
                 `(let ((,vlen ,len) (,vmin ,min) (,vmax ,max))
                    (cond ((< ,vlen ,min)  ,sma)
                          ((<= ,vlen ,max) ,bet)
                          (t               ,big))))))
    (let* ((start (progn (beginning-of-line) (point)))
           (end   (progn (end-of-line)       (point)))
           (line (buffer-substring-no-properties start end)))
      ;; (message "search-asm7090-fields %d %S\n%S" (point) limit line)
      ;; (message "search-asm7090-fields start=%d  end=%d\n" start end)
      (setf limit (1+ end))
      (beginning-of-line) 
      (cond
        ((= 0 (length line))
         ;; (message "  exit 0")
         (re-search-forward 
          "^\\(\\)\\(\\)\\(\\)\\(\\)\\(\\)\\(\\)\n" limit t))
        ((= (aref line 0) +asm7090-ibsys-char+)
         ;; (message "  exit 1")
         (re-search-forward 
          "^\\(\\)\\(\\)\\(\\)\\(\\)\\(.\\{1,72\\}\\)\\(.\\{0,8\\}\\)\n" limit t))
        ((= (aref line 0) +asm7090-comment-char+)
         ;; (message "  exit 2")
         (re-search-forward 
          "^\\(\\)\\(\\)\\(\\)\\(.\\{1,72\\}\\)\\(\\)\\(.\\{0,8\\}\\)\n" limit t))
        ((re-search-forward             ; only spaces
          "^ +\\(\\)\\(\\)\\(\\)\\(\\)\\(\\)\\(\\)\n" limit t))
        ;; (message "  exit 3") t
        ((< (length line) 7)
         ;; (message "  exit 4")
         (re-search-forward 
          "^\\(.\\{0,6\\}\\)\\(\\)\\(\\)\\(\\)\\(\\)\\(\\)\n" limit t))
        (t (let ((labs 0) (labe 6)
                 cods code
                 args arge
                 coms come
                 nums nume)
             ;; (message "  exit 5")
             (setf cods (POSITION-IF (lambda (ch) (/= ch +space+)) line
                                     :start 6 :end 15))
             ;; (message "cods=%S" cods)
             (if cods
                 (setf code (min 72 (or (POSITION +space+ line :start cods)
                                        (length line))))
                 (setf cods 6
                       code 6))
             ;; (message "cods=%S code=%S" cods code)
             (setf args (POSITION-IF (lambda (ch) (/= ch +space+)) line
                                     :start code :end 16))
             ;; (message "args=%S" args)
             (if args
                 (setf args (min 72 args)
                       arge (min 72 (or (POSITION +space+ line :start args)
                                        (length line))))
                 (setf args code
                       arge code))
             ;; (message "OPCODE= %S"(subseq line cods code))
             (when (STRING-EQUAL (subseq line cods code) "BCI")
               (let* ((comma (POSITION (CHARACTER ",") line
                                       :start args :end arge))
                      (size  (HANDLER-CASE
                                 (parse-integer line args
                                                (min arge (or comma arge)))
                               (error () nil))))
                 ;; (message "SIZE=%S" size)
                 (when (and comma size)
                   (setf arge (min 72 (+ comma 1 (* 6 size)) (length line))))))
             ;; (message "args=%S arge=%S" args arge)
             (setf coms (POSITION-IF (lambda (ch) (/= ch +space+)) line
                                     :start arge))
             ;; (message "coms=%S" coms)
             (if coms
                 (setf coms (min 72 coms)
                       come (min 72 (length line)))
                 (setf coms arge
                       come arge))
             ;; (message "coms=%S come=%S" coms come)
             (if (<= 72 (length line))
                 (setf nums 72
                       nume (min 80 (length line)))
                 (setf nums (length line)
                       nume (length line)))
             ;; (message "nums=%S nume=%S" nums nume)
             (prog1
                 (re-search-forward
                  (apply (function concat)
                         (list "^"
                               (format "\\(.\\{%d\\}\\)" (- labe labs))
                               (format " \\{%d\\}"       (- cods labe))
                               (format "\\(.\\{%d\\}\\)" (- code cods))
                               (format " \\{%d\\}"       (- args code))
                               (format "\\(.\\{%d\\}\\)" (- arge args))
                               (format " \\{%d\\}"       (- coms arge))
                               (format "\\(.\\{%d\\}\\)" (- come coms))
                               (format " \\{%d\\}"       (- nums come))
                               "\\(\\)" ; IBSYS
                               (format "\\(.\\{%d\\}\\)" (- nume nums))
                               "\n"))
                  limit t)
               (when nil
                 (message "line=%S\nlabel=%3d %S\nopcod=%3d %S\nargum=%3d %S\ncomnt=%3d %S\nibsys=%3d %S\nnmber=%3d %S\n"
                          line
                          (length (match-string-no-properties 1))
                          (match-string-no-properties 1)
                          (length (match-string-no-properties 2))
                          (match-string-no-properties 2)
                          (length (match-string-no-properties 3))
                          (match-string-no-properties 3)
                          (length (match-string-no-properties 4))
                          (match-string-no-properties 4)
                          (length (match-string-no-properties 5))
                          (match-string-no-properties 5)
                          (length (match-string-no-properties 6))
                          (match-string-no-properties 6)))
               )))))))


(defun split-asm7090-fields ()
  (interactive)
  (search-asm7090-fields (point-max))
  (message "label=%3d %S\nopcod=%3d %S\nargum=%3d %S\ncomnt=%3d %S\nibsys=%3d %S\nnmber=%3d %S\n"
           (length (match-string-no-properties 1))
           (match-string-no-properties 1)
           (length (match-string-no-properties 2))
           (match-string-no-properties 2)
           (length (match-string-no-properties 3))
           (match-string-no-properties 3)
           (length (match-string-no-properties 4))
           (match-string-no-properties 4)
           (length (match-string-no-properties 5))
           (match-string-no-properties 5)
           (length (match-string-no-properties 6))
           (match-string-no-properties 6)))
  

(defun asm7090-font-lock ()
  (interactive)
  (setq font-lock-defaults nil
        font-lock-keywords nil)
  (font-lock-add-keywords 
   nil
   (list
    (list
     (function search-asm7090-fields)
     '(1 font-lock-function-name-face)       ; labels
     '(2 font-lock-keyword-face)             ; operation codes
     '(3 font-lock-reference-face)           ; arguments
     '(4 font-lock-comment-face)             ; comments
     '(5 font-lock-preprocessor-face)        ; ibsys
     '(6 font-lock-type-face)                ; cols 72-80
     ))))



(DEFPARAMETER +codop-7090+
  '(

    ;; page 20.
    (CLA +0500 F T Y "Clear and Add")
    (CAL -0500 F T Y "Clear and Add Logical Word")
    (CLS +0502 F T Y "Clear and Substract")
    (ADD +0400 F T Y "Add")
    (ADM +0401 F T Y "Add Magnitude")
    ;; page 21.
    (SUB +0402 F T Y "Substract")
    (SBM -0400 F T Y "Substract Magnitude")
    (ACL +0361 F T Y "Add and Carry Logical Word")
    ;; page 22.
    (MPY +0200 F T Y "Multiply")
    (MPR -0200 F T Y "Multiply and Round")
    (RND +0760 T 00010 "Round" )
    (VLM +0204 C T T "Variable Length Multiply")
    ;; page 24.
    (DVH +0220 F T Y "Divide or Halt")
    (DVP +0221 F T Y "Divide or Proceed")
    (VDH +0224 C T Y "Variable Lengh Divide or Halt")
    (VDP +0225 C T Y "Variable Lengh Divide or Proceed")
    ;; page 27.
    (FAD +0300 F T Y "Floating Add")
    (FAM +0304 F T Y "Floating Add Magnitude")
    (UFA -0300 F T Y "Unnormalized Floating Add")
    (FSB +0302 F T Y "Floating Substract")
    ;; page 28.
    (UAM -0304 F T Y "Unnormalized Add Magnitude")
    (FSM +0306 F T Y "Floating Substract Magnitude")
    (UFS -0302 F T Y "Unnormalized Floating Substract")
    (USM -0306 F T Y "Unnormalized Floating Substract Magnitude")
    (FRN +0760 T 00011 "Floating Round")
    ;; page 29.
    (FMP +0260 F T Y "Floating Multiply")
    (UFM -0260 F T Y "Unnormalized Floating Multiply")
    ;; page 30.
    (FDH +0240 F T Y "Floating Divide or Halt")
    (FDP +0241 F T Y "Floating Divide or Proceed")
    ;; page 31.
    (ALS +0767 T Y "Accumulator Left Shift")
    (ARS +0771 T Y "Accumulator Right Shift")
    (LLS +0763 T Y "Long Left Shift")
    (LRS +0765 T Y "Long Right Shift")
    (LGL -0763 T Y "Logical Left Shift")
    (LGR -0765 T Y "Logical Right Shift")
    (RQL -0773 T Y "Rotate MQ Left")
    (LDQ +0560 F T Y "Load MQ")
    (STQ -0600 F T Y "Store MQ")
    (SLQ -0620 F T Y "Store Left Half MQ") ; (setcdr! y (cdr mq))
    (STO +0601 F T Y "Store")
    (SLW +0602 F T Y "Store Logical Word")
    (STP +0630 F T Y "Store Prefix")
    (STD +0622 F T Y "Store Decrement") ; (setcdr! y (cdr a))
    (STT +0625 F T Y "Store Tag")
    (STA +0621 F T Y "Store Address")   ; (setcar! y (car a))
    (STL -0625 F T Y "Store Instruction Location Counter")
    (STR -1 "Store Location and Trap")
    (STZ +0600 F T Y "Store Zero")
    (XCA +0131 "Exchange AC and MQ")
    (XCL -0130 "Exclange Logical AC and MQ")
    (ENK +0760 T 00004 "Enter Keys")
    (HTR +0000 F T Y "Halt and Transfer")
    (NOP +0761 "No Operation")
    (HPR +0420 "Halt and Proceed")
    (XEC +0522 F T Y "Execute")
    (TRA +0020 F T Y "Transfer")
    (ETM +0760 T 00007 "Enter Trap Mode")
    (LTM -0760 T 00007 "Leave Trap Mode")
    (TTR +0021 F T Y "Trap Transfer")
    (TZE +0100 F T Y "Transfer on Zero")
    (TNZ -0100 F T Y "Transfer on No Zero")
    (TPL +0120 F T Y "Transfer on Plus")
    (TMI -0120 F T Y "Transfer on Minus")
    (TOV +0140 F T Y "Transfer on Overflow")
    (TNO -0140 F T Y "Transfer on No Overflow")
    (TQP +0162 F T Y "Transfer on MQ Plus")
    (TQO +0161 F T Y "Transfer on MQ Overflow" "704 floating point mode")
    ;; page 39.
    (TLQ +0040 F T Y "Transfer on Low MQ")
    (TSX +0074 T Y "Transfer and Set Index")
    (TXI +1 D T Y "Transer with Index Incremented")
    (TXH +3 D T Y "Transfer on Index High")
    (TXL -3 D T Y "Transfer on Index Low or Equal")
    (TIX +2 D T Y "Transfer on Index")
    (TNX -2 D T Y "Transfer on No Index")
    (PSE +0760 T (OR 00140                 ; slf
                  (INTEGER 00141 00144)    ; sln
                  (INTEGER 00161 00166)    ; swt
                  (INTEGER 01341 01342)    ; spu
                  (INTEGER 02341 02342)
                  (INTEGER 03341 03342)
                  (INTEGER 04341 04342)
                  (INTEGER 05341 05342)
                  (INTEGER 06341 06342)
                  (INTEGER 07341 07342)
                  (INTEGER 10341 10342)
                  01360 02360 03360 04360 05360 06360 07360 10360 ; spt
                  (INTEGER 01361 01362) ; spr
                  (INTEGER 02361 02362)
                  (INTEGER 03361 03362)
                  (INTEGER 04361 04362)
                  (INTEGER 05361 05362)
                  (INTEGER 06361 06362)
                  (INTEGER 07361 07362)
                  (INTEGER 10361 10362))  "Plus Sense")
    (MSE -0760 T (INTEGER 00141 00144) "Minus Sense")
    (BTT -0760 T (OR 01000 02000 03000 04000 05000 06000 07000 10000)
     "Beginning of Tape test")
    (ETT -0760 T (OR 01000 02000 03000 04000 05000 06000 07000 10000)
     "End of Tape test")
    (IOT +0760 T 00005 "Input-Output Check Test")
    (PBT -0760 T 00001 "P-Bit Test")
    (LBT +0760 T 00001 "Low-Order Bit Test")
    (DCT +0760 T 00012 "Divide Check Test")
    ;; page 43.
    (ZET +0520 F T Y "Storage Zero Test")
    (NZT -0520 F T Y "Storage not Zero Test")
    (CAS +0340 F T Y "Compare Accumulator with Storage")
    (LAS -0340 F T Y "Logical Compare Accumulator with Storage")
    (TCOA +0060 F T Y "Transfer on Channel A in Operation")
    (TCOB +0061 F T Y "Transfer on Channel B in Operation")
    (TCOC +0062 F T Y "Transfer on Channel C in Operation")
    (TCOD +0063 F T Y "Transfer on Channel D in Operation")
    (TCOE +0064 F T Y "Transfer on Channel E in Operation")
    (TCOF +0065 F T Y "Transfer on Channel F in Operation")
    (TCOG +0066 F T Y "Transfer on Channel G in Operation")
    (TCOH +0067 F T Y "Transfer on Channel H in Operation")
    (TCNA -0060 F T Y "Transfer on Channel A not in Operation")
    (TCNB -0061 F T Y "Transfer on Channel B not in Operation")
    (TCNC -0062 F T Y "Transfer on Channel C not in Operation")
    (TCND -0063 F T Y "Transfer on Channel D not in Operation")
    (TCNE -0064 F T Y "Transfer on Channel E not in Operation")
    (TCNF -0065 F T Y "Transfer on Channel F not in Operation")
    (TCNG -0066 F T Y "Transfer on Channel G not in Operation")
    (TCNH -0067 F T Y "Transfer on Channel H not in Operation")
    (TRCA +0022 F T Y "Transfer on Channel A Redundancy Check")
    (TRCB -0022 F T Y "Transfer on Channel B Redundancy Check")
    (TRCC +0024 F T Y "Transfer on Channel C Redundancy Check")
    (TRCD -0024 F T Y "Transfer on Channel D Redundancy Check")
    (TRCE +0026 F T Y "Transfer on Channel E Redundancy Check")
    (TRCF -0026 F T Y "Transfer on Channel F Redundancy Check")
    (TRCG +0027 F T Y "Transfer on Channel G Redundancy Check")
    (TRCH -0027 F T Y "Transfer on Channel H Redundancy Check")
    (TEFA +0030 F T Y "Transfer on Channel A End of File")
    (TEFB +0031 F T Y "Transfer on Channel B End of File")
    (TEFC +0032 F T Y "Transfer on Channel C End of File")
    (TEFD +0033 F T Y "Transfer on Channel D End of File")
    (TEFE +0034 F T Y "Transfer on Channel E End of File")
    (TEFF +0035 F T Y "Transfer on Channel F End of File")
    (TEFG +0036 F T Y "Transfer on Channel G End of File")
    (TEFH +0037 F T Y "Transfer on Channel H End of File")
    (TCH  +1 F Y "Transfer in Channel")
    (LXA +0534 T Y "Load Index from Address")
    (LAC +0535 T Y "Load Complement of Address in Index")
    (LXD -0534 T Y "Load Index from Decrement")
    (LDC -0535 T Y "Load Complement of Decrement in Index")
    (AXT +0774 T Y "Address to Index True")
    (AXC -0774 T Y "Address to Index Complemented")
    (PAX +0734 T "Place Address in Index")
    (PAC +0737 T "Place Complement of Address in Index")
    (PDX -0734 T "Place Decrement in Index")
    (PDC -0737 T "Place Complement of Decrement in Index")
    (SXA +0634 T Y "Store Index in Address")
    (SXD -0634 T Y "Store Index in Decrement")
    (PXA +0754 T "Place Index in Address")
    (PXD -0754 T "Place Index in Decrement")
    (ORA -0501 F T Y "OR to Accumulator")
    (ORS -0602 F T Y "OR t Storage")
    (ANA -0320 F T Y "AND to Accumulator")
    (ANS +0320 F T Y "AND to Storage")
    (ERA +0322 F T Y "Exclusive OR to Accumulator")
    (COM +0760 T 00006 "Complement Magnitude")
    (CLM +0760 T 00000 "Clear Magnitude")
    (CHL +0760 T 00002 "Change Sign")
    (SSP +0760 T 00003 "Set Sign Plus")
    (SSM -0760 T 00003 "Set Sign Minus")
    (PAI +0044 "Place Accumulator in Indicators")
    (PIA -0046 "Place Indicators in Accumulator")
    ;; page 51.
    (LDI +0441 F T Y "Load Indicators")
    (STI +0604 F T Y "Store Indicators")
    (OAI +0043 "OR Accumulator to Indicators")
    (OSI +0442 F T Y  "OR Storage to Indicators")
    (SIL -0055 R "Set Indicators of Left Half")
    ;; page 52.
    (SIR +0055 R "Set Indicators of Right Half")
    (RIA -0042 "Reset Indicators from Accumulators")
    (RIS +0445 F T Y "Reset Indicators from Storage")
    (RIL -0057 R "Reset Indicators of Left Half")
    (RIR +0057 R "Reset Indicators of Right Half")
    (IIA +0041 "Invert Indicators from Accumulator")
    (IIS +0440 F T Y "Invert Indicators from Storage")
    (IIL -0051 R "Invert Indicators of Left Half")
    (IIR +0051 R "Invert Indicators of Right Half")
    (TIO +0042 F T Y "Transfer when Indicators On")
    (TIF +0046 F T Y "Transfer when Indicators Off")
    ;; page 54.
    (ONT +0446 F T Y "On Test for Indicators")
    (OFT +0444 F T Y "Off Test for Indicators")
    (LNT -0056 R "Left Half Indicators On Test")
    (RNT +0056 R "Right Half Indicators On Test")
    ;; page 55.
    (LFT -0054 R "Left Half Indicators Off Test")
    (RFT +0054 R "Right Half Indicators Off Test")
    ;; page 56.
    (CVR +0114 C V Y "Convert by Replacement from the AC")
    (CRQ -0154 C V Y "Convert by Replacement from the MQ")
    (CAQ -0114 C V Y "Convert by Addition from the MQ")
    ;; page 57.  Cf. device codes.
    ;; page 58.
    (RDS +0762 T Y "Read Select")
    (WRS +0766 T Y "Write Select")
    (BSR +0764 T Y "Backspace Record")
    (BSF -0764 T Y "Backspace File")
    (WEF +0770 T Y "Write End-of-File")
    (REW +0772 T Y "Rewind")
    (RUN -0772 T Y "Rewind and Unload")
    (SDN +0776 Y "Set Density") ;; see page 59  for channels / density codes
    ;; page 60.
    (RDCA +0760 T 01352 "Reset Data Channel A")
    (RDCB +0760 T 02352 "Reset Data Channel A")
    (RDCC +0760 T 03352 "Reset Data Channel A")
    (RDCD +0760 T 04352 "Reset Data Channel A")
    (RDCE +0760 T 05352 "Reset Data Channel A")
    (RDCF +0760 T 06352 "Reset Data Channel A")
    (RDCG +0760 T 07352 "Reset Data Channel A")
    (RDCH +0760 T 10352 "Reset Data Channel A")
    (SCHA +0640 F T Y "Store Channel A")
    (SCHB -0640 F T Y "Store Channel B")
    (SCHC +0641 F T Y "Store Channel C")
    (SCHD -0641 F T Y "Store Channel D")
    (SCHE +0642 F T Y "Store Channel E")
    (SCHF -0642 F T Y "Store Channel F")
    (SCHG +0643 F T Y "Store Channel G")
    (SCHH -0643 F T Y "Store Channel H")
    (RCHA +0540 F T Y "Reset and Load Channel A")
    (RCHB -0540 F T Y "Reset and Load Channel B")
    (RCHC +0541 F T Y "Reset and Load Channel C")
    (RCHD -0541 F T Y "Reset and Load Channel D")
    (RCHE +0542 F T Y "Reset and Load Channel E")
    (RCHF -0542 F T Y "Reset and Load Channel F")
    (RCHG +0543 F T Y "Reset and Load Channel G")
    (RCHH -0543 F T Y "Reset and Load Channel H")
    (LCHA +0544 F T Y "Load Channel A")
    (LCHB -0544 F T Y "Load Channel B")
    (LCHC +0545 F T Y "Load Channel C")
    (LCHD -0545 F T Y "Load Channel D")
    (LCHE +0546 F T Y "Load Channel E")
    (LCHF -0546 F T Y "Load Channel F")
    (LCHG +0547 F T Y "Load Channel G")
    (LCHH -0547 F T Y "Load Channel H")
    ;; page 65.
    (ENB +0564 F T Y "Enable Traps from Y")
    (RCT +0760 T 00014 "Restore Channel Traps")
    (ESNT -0021 F T Y "Enter Storage Nullification and Transfer"
     "Enters 709 mode")
    (LSNM -0760 T 00010 "Leave Storage Nullification Mode"
     "Leaves 709 mode")
    (ESTM -0760 T 00005 "Enter Select Trap Mode")
    (ECTM -0760 T 00006 "Enter Copy Trap Mode")
    (EFTM -0760 T 00002 "Enter Floating Trap Mode")
    (LFTM -0760 T 00004 "Leave Floating Trap Mode")
    ;; page 67.  Add Instructions for the IBM 7909 Data Channel
    
    ))



(defun asm7090-show-codop-description (codop)
  (let ((desc (find-if (lambda (desc) (STRING-EQUAL (first desc) codop))
                       +codop-7090+)))
    (message (if desc
                 (format "%s: %s" (first desc) (find-if (function stringp) desc))
                 (format "%s: unknown" (first desc))))))


(defun asm7090-describe-codop ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (search-asm7090-fields (save-excursion (forward-line 1) (point)))
    (asm7090-show-codop-description (match-string-no-properties 2))))


(defun asm7090 ()
  (interactive)
  (asm7090-font-lock)
  (make-local-variable 'tab-stop-list)
  (setf tab-stop-list     '(0 7 15 34 72)
        asm-comment-char   ?*)
  (local-set-key [tab] (function tab-to-tab-stop))
  (local-set-key "" (lambda ()
                        (interactive) 
                        (asm7090-describe-codop) 
                        (newline-and-indent)))
  (font-lock-mode 1)
  (message "asm7090 activated"))


(defvar *card-id-column* 72)

(defun add-card-id (start end prefix from)
  (interactive "r
sPrefix: 
nFrom:")
  (setf prefix (subseq prefix 0 (min (length prefix) 7)))
  (let ((nf (format "%%-%ds%s%%0%dd"
              *card-id-column*
              prefix
              (- 8 (length prefix)))))
    (save-excursion
      (let ((end (let ((m (make-marker))) (set-marker m end))))
        (goto-char start)
        (while (re-search-forward "\\(.*\\)" end nil)
          (let ((line (match-string 0)))
            (delete-region (match-beginning 0) (match-end 0))
            (insert (format nf
                      (subseq line 0 (min (length line) *card-id-column*))
                      from))
            (forward-char 1)
            (incf from)))))))
