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
    (define-key map (kbd "M-+") 'multi-column-set-width)
    map))

(defgroup multi-column nil
  "Minor mode for editing of two-column text."
  :prefix "multi-column-"
  :group 'frames)

(defcustom multi-column-use-gap 't "Use a custom column spacer rather than simply splicing the buffers.")
(defcustom multi-column-gap "          " "The spacer between cols")
(defvar multi-column-width -1)


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
  (apply 'max (multi-column-map-lines 
               '(lambda () (end-of-line) (current-column)))))


(defun multi-column-end-break (direction)
  "Finds the vertical end of a break between horizontal columns,
if the break consists of identical characters."
  (save-excursion
    (forward-char (length multi-column-gap))
    (if (search-backward multi-column-gap 
                         (- (point) (* 2 (length multi-column-gap))) 
                         't)
        (let ((test 't)
              (col (current-column)))
          (while (and test
                  (or (< (line-end-position)
                         (+ col (line-beginning-position)))
                      (search-backward 
                       multi-column-gap 
                       (+ col (line-beginning-position))
                       (+ col (line-beginning-position)
                          (length multi-column-gap)) 't)))
            (setq test (eq 0 (forward-line direction))))
          (if (and test (> 0 direction)) (forward-line))
          (line-beginning-position)
          't))))

(defun multi-column-narrow ()
  "Narrows a buffer to only those lines which are split into
columns at point."
  (let ((start (multi-column-end-break -1))
        (end (multi-column-end-break 1)))
    (narrow-to-region start end)))

(defun multi-column-remove-all-gaps (width)
  ""
  ;; gotta find pos, then map lines.
  (goto-char (point-min))
  (forward-char (+ width (length multi-column-gap)))
  (if (search-backward multi-column-gap (- (point) (* 2 (length multi-column-gap))) 't)
      (let ((mcg-col (current-column)))
        (print mcg-col)
        (multi-column-map-lines 
         (lambda () (multi-column-remove-gap mcg-col))))))


(defun multi-column-remove-gap (position)
  "If using gap, and on gap, remove it before splitting. Position
is a column-number (current-column) style, at which the gap starts."
  (if (>= (- (line-end-position) (line-beginning-position))
          (+ position (length multi-column-gap))) ; gotta fit on one line
      (replace-string multi-column-gap "" nil 
                      (+ (line-beginning-position) position)
                      (+ (line-beginning-position) position
                         (length multi-column-gap)))))


(defun multi-column-split ()
  "Split horizontally, at point, into two buffers."
  (interactive)
  (multi-column-narrow)
  (multi-column-remove-all-gaps (current-column))
  (save-excursion ; Won't work, have to roll own based on line number.
    (let* ((width (current-column))
           (new-buffer (generate-new-buffer "column"))
           (new-column (split-window-horizontally (+ (length multi-column-gap) width))))
      (set (make-local-variable 'multi-column-width) width)
      (set-fill-column width)
      (setq multi-column-width width)
      (set-window-buffer new-column new-buffer)
      (multi-column-shift-column width (current-buffer) new-buffer)
      (delete-trailing-whitespace)
      (set-buffer new-buffer) (multi-column-mode)
      (set (make-local-variable 'multi-column-width)
           (multi-column-longest-line)))))

(defun multi-column-merge (left-window right-window)
  "Merge two windows into one, splicing the buffers horizontally."
  ; What is this imperative nonsense? Goodness me.
  ; On the right
  (set-buffer (window-buffer right-window))
  (delete-trailing-whitespace)
  (goto-char (point-max))
  (multi-column-fill-goto-column (multi-column-longest-line))


  (let ((right-rect (delete-extract-rectangle (point-min) (point-max)))
        (right-width multi-column-width)
        (use-width (progn (set-buffer (window-buffer left-window))
                          (max (multi-column-longest-line) 
                               multi-column-width))))
    ; On the left
    (delete-trailing-whitespace)

    (goto-char (point-max))
    (multi-column-fill-goto-column use-width)

    (goto-char (point-min))
    (multi-column-fill-goto-column use-width)

    (if multi-column-use-gap
        (progn (string-rectangle (point) (point-max) multi-column-gap)
               (multi-column-fill-goto-column (+ use-width (length multi-column-gap)))))

    (insert-rectangle right-rect)
    (kill-buffer (window-buffer right-window))
    (delete-window right-window)
    (select-window left-window)
    (delete-trailing-whitespace)
    (widen)
    (setq multi-column-width (+ multi-column-width
                                right-width))))
       
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

(defun multi-column-set-width (width)
  (interactive (list (read-number "Set column width" 
                            (current-column))))
  (set (make-local-variable 'multi-column-width) width)
  (set-fill-column width))

(defun multi-column-set-gap (gap)
  (interactive (list (read-string "Set gap" "          ")))
  (set 'multi-column-gap gap))

(define-minor-mode multi-column-mode
  "Toggle multi-column mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Multi-col"
  multi-column-map)


(provide 'multi-column-mode)