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
  (end-of-line)
  (if (< (current-column) column)
      (insert-char ?  (- column (current-column))))
  (beginning-of-line)
  (forward-char column))
           

(defun multi-column-kill-column (column) 
  (save-excursion 
    (let ((end (progn (goto-char (point-max)) 
                      (multi-column-fill-goto-column column)
                      (point)))
          (start (point-min)))
      (delete-extract-rectangle start end))))

(defun multi-column-shift-column (width current new)
  (let ((first (multi-column-kill-column width))
        (rest  (delete-and-extract-region (point-min) (point-max))))
    (set-buffer new) (insert rest)
    (set-buffer current) (insert-rectangle first)))

(defun multi-column-next-column (window-walker)
  (let ((this-window (funcall window-walker (selected-window))))
    (while (not (buffer-local-value 'multi-column-mode 
                                    (window-buffer this-window)))
      (setq this-window (funcall window-walker this-window)))
    this-window))


(defun multi-column-map-lines (function)
  (save-excursion
    (goto-char (point-min))
    (let ((col (list (funcall function))))
      (while (eq 0 (forward-line))
        (setq col (cons (funcall function) col)))
      (reverse col))))

(defun multi-column-longest-line ()
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
  "Toggle multi-column mode. "
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Multi-col"
  multi-column-map)


