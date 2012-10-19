;;; multi-column-mode.el --- Multi-column editing.

;; Author: Sam Bleckley <s@diiq.org>
;; Version: 0.1
;; Keywords: columns

;; Licence: WTFPL <http://sam.zoy.org/wtfpl/>

(defvar multi-column-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-|") 'multi-column-split)
    (define-key map (kbd "M-]") 'multi-column-merge-left)
    (define-key map (kbd "M-[") 'multi-column-merge-right)
    map))

(defgroup multi-column nil
  "Minor mode for editing of two-column text."
  :prefix "multi-column-"
  :group 'frames)


(defun multi-column-fill-goto-column (column)
  "Goto column, and fill with spaces if the line is too short."
  (end-of-line)
  (if (< (current-column) column)
      (insert-char ?  (- column (current-column))))
  (beginning-of-line)
  (forward-char column))
           

(defun multi-column-kill-column (width) 
  "Delete and return the first column of width width in the buffer."
  (save-excursion 
    (let ((end (progn (goto-char (point-max)) 
                      (multi-column-fill-goto-column width)
                      (point)))
          (start (point-min)))
      (delete-extract-rectangle start end))))

(defun multi-column-shift-column (width left right)
  "Where right and left are buffers, keep the first column from
left of width width in left, and shift the rest to right."
  (let ((first (multi-column-kill-column width))
        (rest  (delete-and-extract-region (point-min) (point-max))))
    (set-buffer right) (insert rest)
    (set-buffer left) (insert-rectangle first)))

(defun multi-column-next-column (window-walker)
  "Find the next window which is in multi-column-mode; use
window-walked for the order of windows to examine."
  (let ((this-window (funcall window-walker (selected-window))))
    (while (not (buffer-local-value 'multi-column-mode 
                                    (window-buffer this-window)))
      (setq this-window (funcall window-walker this-window)))
    this-window))


(defun multi-column-map-lines (function)
  "Map a function across every line in the buffer. Surely this exists?"
  (save-excursion
    (goto-char (point-min))
    (let ((col (list (funcall function))))
      (while (eq 0 (forward-line))
        (setq col (cons (funcall function) col)))
      (reverse col))))

(defun multi-column-longest-line ()
  "Returns the length of the longest line in the buffer."
  (apply 'max (multi-column-map-lines '(lambda () (end-of-line) (current-column)))))




(defun multi-column-split ()
  "Split horizontally, at point, into two buffers."
  (interactive)
  (save-excursion ; Won't work, have to roll own based on line number.
    (let* ((width (current-column))
           (new-buffer (generate-new-buffer "column"))
           (new-column (split-window-horizontally (+ 4 width))))
      (set-fill-column width)
      (setq multi-column-width width)
      (set-window-buffer new-column new-buffer)
      (multi-column-shift-column width (current-buffer) new-buffer)
      (set-buffer new-buffer) (multi-column-mode)
      (setq multi-column-width 10000))))

(defun multi-column-merge (left-window right-window)
  "Merge two windows into one, splicing the buffers horizontally."
  (set-buffer (window-buffer right-window))
  (goto-char (point-max))
  (multi-column-fill-goto-column (multi-column-longest-line))
  (let ((right-rect (delete-extract-rectangle (point-min) (point-max))))
    (set-buffer (window-buffer left-window))
    (goto-char (point-min))
    (multi-column-fill-goto-column (multi-column-longest-line))
    (insert-rectangle right-rect)
    (kill-buffer (window-buffer right-window))
    (delete-window right-window)
    (select-window left-window)
    (setq multi-column-width (multi-column-longest-line))))
    
(defun multi-column-merge-left ()
  "Merge this column with the next multi-column-mode window to the left."
  (interactive)
  (multi-column-merge (multi-column-next-column 'previous-window)
                      (selected-window)))

(defun multi-column-merge-right ()
  "Merge this column with the next multi-column-mode window to the right."
  (interactive)
  (multi-column-merge (selected-window)
                      (multi-column-next-column 'next-window)))

(define-minor-mode multi-column-mode
  "Toggle multi-column mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Multi-col"
  multi-column-map)


(provide 'multi-column-mode)