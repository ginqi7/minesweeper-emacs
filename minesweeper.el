;;; minesweeper.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'eieio)

(defvar minesweeper--bomb-num 10)

(defvar minesweeper--cell-clicked-num 0)

(defvar minesweeper--buffer-name "*minesweeper*")

(defvar minesweeper--cell-matrix nil)

(defvar minesweeper--columns 9)

(defvar minesweeper--difficulty "BASE")

(defvar minesweeper--difficulty-alist '(
					("BASE" . (9 9 10))
					("MEDIUM" . (16 16 40))
					("HARD" . (30 16 99))
					"CUSTOM" . (0 0 0)))

(defvar minesweeper--image-matrix
  '(
   ("bomb-num-0" . (0 . 0))
   ("bomb-num-1" . (0 . 1))
   ("bomb-num-2" . (0 . 2))
   ("bomb-num-3" . (0 . 3))
   ("bomb-num-4" . (0 . 4))
   ("bomb-num-5" . (0 . 5))
   ("bomb-num-6" . (0 . 6))
   ("bomb-num-7" . (0 . 7))
   ("bomb-num-8" . (0 . 8))
   ("bomb-num-9" . (0 . 9))
   ("bomb-normal" . (1 . 0))
   ("bomb-wrong" . (1 . 1))
   ("bomb" . (1 . 2))
   ("unknow" . (1 . 3))
   ("flag" . (1 . 4))
   ("init" . (1 . 5))))

(defvar minesweeper--png-file (expand-file-name (file-name-concat (file-name-directory (or load-file-name (buffer-file-name))) "minesweeper.png")))

(defvar minesweeper--rows 9)

(defvar minesweeper--scale 1.5)

(defvar minesweeper--status "RUNING")

(defun minesweeper-check-status ()
  (pcase minesweeper--status
   ("ERROR" (message "Game Over!") nil)
   ("SUCCESS" (message "You are Succeed!") nil)
   ("RUNNING" t)))

(defclass minesweeper-blank (minesweeper-cell) ((round-bomb-count)))

(defclass minesweeper-bomb (minesweeper-cell) ())

(defclass minesweeper-cell ()
  ((index :initarg :index)
   (point :initarg :point)
   (clicked-p :initarg :clicked-p :initform nil)))

(cl-defmethod minesweeper-click ((cell minesweeper-blank))
  "Click Blank CELL."
  (when (minesweeper-check-status)
   (unless (eieio-oref cell 'clicked-p)
     (setq minesweeper--cell-clicked-num (1+ minesweeper--cell-clicked-num))
     (when (eq (+ minesweeper--cell-clicked-num minesweeper--bomb-num) (* minesweeper--columns minesweeper--rows))
	 (setq minesweeper--status "SUCCESS")
       )
     (eieio-oset cell 'clicked-p t)
     (save-excursion
         (let ((round-bomb-count (eieio-oref cell 'round-bomb-count)))
        (goto-char (eieio-oref cell 'point))
        (delete-char 1)
        (insert (minesweeper-draw-cell cell (format "bomb-num-%s" round-bomb-count)))
        (when (= 0 round-bomb-count)
         (minesweeper--cascade-clearing cell)))))))

(cl-defmethod minesweeper-click ((cell minesweeper-bomb))
  "Click Bomb CELL."
  (when (minesweeper-check-status)
   (save-excursion
     (goto-char (eieio-oref cell 'point))
     (delete-char 1)
     (insert (minesweeper-draw-cell cell "bomb"))
     (setq minesweeper--status "ERROR"))))

(cl-defmethod minesweeper-right-click ((cell minesweeper-cell))
  "Click Bomb CELL."
  (when (minesweeper-check-status)
   (save-excursion
     (goto-char (eieio-oref cell 'point))
     (delete-char 1)
     (insert (minesweeper-draw-cell cell "flag")))))

(cl-defmethod minesweeper-round-cells ((cell minesweeper-cell))
  "Get a cell list around the CELL."
  (let* ((index (eieio-oref cell 'index))
	(matrix-index (cons (/ index minesweeper--columns)
			     (% index minesweeper--columns)))
	(west (cons (car matrix-index) (1- (cdr matrix-index))))
	(east (cons (car matrix-index) (1+ (cdr matrix-index))))
	(north (cons (1- (car matrix-index)) (cdr matrix-index)))
	(south (cons (1+ (car matrix-index)) (cdr matrix-index)))
	(north-west (cons (1- (car matrix-index)) (1- (cdr matrix-index))))
	(north-east (cons (1- (car matrix-index)) (1+ (cdr matrix-index))))
	(south-west (cons (1+ (car matrix-index)) (1- (cdr matrix-index))))
	(south-east (cons (1+ (car matrix-index)) (1+ (cdr matrix-index)))))
   (list
    (when (minesweeper--index-valid-p west) (minesweeper--get west minesweeper--cell-matrix))
    (when (minesweeper--index-valid-p east) (minesweeper--get east minesweeper--cell-matrix))
    (when (minesweeper--index-valid-p north) (minesweeper--get north minesweeper--cell-matrix))
    (when (minesweeper--index-valid-p south) (minesweeper--get south minesweeper--cell-matrix))
    (when (minesweeper--index-valid-p north-west) (minesweeper--get north-west minesweeper--cell-matrix))
    (when (minesweeper--index-valid-p north-east) (minesweeper--get north-east minesweeper--cell-matrix))
    (when (minesweeper--index-valid-p south-west) (minesweeper--get south-west minesweeper--cell-matrix))
    (when (minesweeper--index-valid-p south-east) (minesweeper--get south-east minesweeper--cell-matrix)))))

(defmacro minesweeper--append (lst item)
  "Append ITEM to LST."
  `(setq ,lst (append ,lst (list ,item))))

(defun minesweeper() "Minesweeper."
      (interactive)
      (setq minesweeper--cell-matrix nil)
      (with-current-buffer (get-buffer-create minesweeper--buffer-name)
       (let ((buffer-read-only nil))
	 (erase-buffer)
	 (minesweeper-init)
	 (dolist (row minesweeper--cell-matrix)
	   (dolist (cell row)
	     (eieio-oset cell 'point (point))
	     (insert (minesweeper-draw-cell cell "init")))
	   (insert "\n"))
	 (setq buffer-read-only t)
	 (switch-to-buffer (current-buffer))
	 )))

(defun minesweeper--cascade-clearing (cell)
  "Minesweeper Cascade Clearing Blank CELL."
  (let ((round-cells (minesweeper-round-cells cell)))
   (dolist (round-cell round-cells)
     (when (eq 'minesweeper-blank (type-of round-cell))
       (minesweeper-click round-cell)))))

(defun minesweeper--count-around-bomb-num (cell)
  "Minesweeper Count Bomb Num Around CELL."
  (apply #'+ (mapcar
	     (lambda (cell) (if (and cell (eq (type-of cell) 'minesweeper-bomb)) 1 0))
	     (minesweeper-round-cells cell))))

(defun minesweeper--get (index matrix)
  "Minesweeper Get Cell from MATRIX by INDEX.
INDEX: An Cons represents row and column of cell in MATRIX."
  (nth (cdr index) (nth (car index) matrix)))

(defun minesweeper--image-size ()
  "Minesweeper Image Size."
  (cons (ceiling (* minesweeper--scale 16)) (ceiling (* minesweeper--scale 16))))

(defun minesweeper--index-valid-p (index)
  "Minesweeper Check if INDEX Valid."
  (and (>= (car index) 0)
      (< (car index) minesweeper--rows)
      (>= (cdr index) 0)
      (< (cdr index) minesweeper--columns)))

(defun minesweeper--max-index ()
  "Minesweeper Max Index."
  (* minesweeper--columns minesweeper--rows))

(defun minesweeper--random-bomb-index-list (region-num bumb-num)
  "Minesweeper  Random Bomb Index List.
REGION-NUM: the max number.
BUMB-NUM: the random number."
  (let ((bomb-index-list)
       (bomb-index (random region-num)))
   (while (< (length bomb-index-list) bumb-num)
     (unless (member bomb-index bomb-index-list)
       (push bomb-index bomb-index-list))
     (setq bomb-index (random region-num)))
   bomb-index-list))

(defun minesweeper-draw-cell (cell image-name)
  "Minesweeper Draw CELL with IMAGE-NAME."
  (when-let* ((position (alist-get image-name minesweeper--image-matrix nil nil #'equal))
	     (image-size (minesweeper--image-size))
	     (width (car image-size))
	     (height (cdr image-size))
	     (map (make-sparse-keymap))
	     (x (* (cdr position) width))
	     (y (* (car position) height)))
   (define-key map [mouse-1] (lambda () (interactive)(minesweeper-click cell)))
   (define-key map [mouse-3] (lambda () (interactive)(minesweeper-right-click cell)))
   (propertize "." 'display
		(list (cons 'slice (list x y width height)) (create-image minesweeper--png-file nil nil :scale minesweeper--scale))
		'mouse-face 'highlight
		'cell cell
		'keymap map)))

(defun minesweeper-init ()
  "Minesweeper Init."
  (setq minesweeper--status "RUNNING")
  (setq minesweeper--cell-clicked-num 0)
  (let* ((max-index (minesweeper--max-index))
	(random-bomb-index-list (minesweeper--random-bomb-index-list max-index minesweeper--bomb-num))
	(row))
   (dotimes (i max-index)
     (when (and (> i 0) (eq 0 (% i minesweeper--columns)))
       (minesweeper--append minesweeper--cell-matrix row)
       (setq row nil))
     (minesweeper--append row (if (member i random-bomb-index-list) (make-instance 'minesweeper-bomb :index i) (make-instance 'minesweeper-blank :index i))))
   (dolist (row minesweeper--cell-matrix)
     (dolist (cell row)
       (when (eq 'minesweeper-blank (type-of cell))
         (eieio-oset cell 'round-bomb-count (minesweeper--count-around-bomb-num cell)))))))

(defun minesweeper-select-difficulty ()
  "Minesweeper Select Difficulty."
  (interactive)
  (let* ((difficulty (completing-read "Select a difficulty: " minesweeper--difficulty-alist))
	(difficulty-properties (alist-get difficulty minesweeper--difficulty-alist nil nil #'equal))
	(column (nth 0 difficulty-properties))
	(row (nth 1 difficulty-properties))
	(bomb-num (nth 2 difficulty-properties)))
   (setq minesweeper--difficulty difficulty)
   (setq minesweeper--columns column)
   (setq minesweeper--rows row)
   (setq minesweeper--bomb-num bomb-num)
   (minesweeper)
   ))

(provide 'minesweeper)
;;; minesweeper.el ends here

