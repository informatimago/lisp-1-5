;;****************************************************************************
;;FILE:               pjb-page.el
;;LANGUAGE:           emacs lisp
;;SYSTEM:             POSIX
;;USER-INTERFACE:     NONE
;;DESCRIPTION
;;    
;;    View a buffer page by page.
;;    
;;AUTHORS
;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;MODIFICATIONS
;;    2005-06-01 <PJB> Created.
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

(defvar *saved-scroll-functions* nil)
(make-local-variable '*saved-scroll-functions*)
  
(defun page-mode (&optional on)
  (interactive "p")
  (if (if on (plusp on) (not *saved-scroll-functions*))
    (progn
      (narrow-to-page)
      (unless *saved-scroll-functions*
        (setf *saved-scroll-functions*
              (list (key-binding [prior]) (key-binding [next])
                    (key-binding [home])  (key-binding [end]))))
      (local-set-key [prior] (function pm-backward-page))
      (local-set-key [next]  (function pm-forward-page))
      (local-set-key [home]  (function pm-beginning-of-buffer))
      (local-set-key [end]   (function pm-end-of-buffer)))
    (progn 
      (widen)
      (if *saved-scroll-functions*
        (progn
          (local-set-key [prior] (first  *saved-scroll-functions*))
          (local-set-key [next]  (second *saved-scroll-functions*))
          (local-set-key [home]  (third  *saved-scroll-functions*))
          (local-set-key [end]   (fourth *saved-scroll-functions*))
          (setf *saved-scroll-functions* nil))
        (progn
          (local-set-key [prior] (function scroll-down))
          (local-set-key [next]  (function scroll-up))
          (local-set-key [home]  (function beginning-of-buffer))
          (local-set-key [end]   (function end-of-buffer)))))))


(defun pm-forward-page (&optional count)
  (interactive "p")
  (setf count (or count 1))
  (widen)
  (unless (search-forward "\f" nil 'at-limit count)
    (goto-char (point-max)))
  (narrow-to-page))


(defun pm-backward-page (&optional count)
  (interactive "p")
  (setf count (or count 1))
  (widen)
  (unless (search-backward "\f" nil 'at-limit (1+ count))
    (goto-char (point-max)))
  (narrow-to-page))


(defun pm-beginning-of-buffer ()
  (interactive)
  (widen)
  (goto-char (point-min))
  (narrow-to-page))


(defun pm-end-of-buffer ()
  (interactive)
  (widen)
  (goto-char (point-max))
  (narrow-to-page))



(defun pjb-animate (speed)
  (interactive "nSpeed: ")
  (let ((delay (/ 1.0  speed))
        (done  nil))
    (widen)
    (goto-char (point-min))
    (message "Animating...")
    (while (not done)
      (widen)
      (if (search-forward "\f" nil 'at-limit)
        nil
        (goto-char (point-max))
        (setq done t))
      (narrow-to-page)
      (sit-for delay)
      (force-mode-line-update t)
      ) ;;while
    (message "Done.")))