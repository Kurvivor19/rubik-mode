;;; solver.el --- find solutions for simple cube problems  -*- lexical-binding: t; -*-
(require 'rubik)

;; constants

(defconst solver-letters '((0 . ?\s)
                           (1 . ?*)
                           (2 . ?a)
                           (3 . ?b)
                           (4 . ?c)
                           (5 . ?d)
                           (6 . ?e)))

(defconst solver-transformation-groups
  '((rubik-U rubik-U2 rubik-Ui)
    (rubik-F rubik-F2 rubik-Fi)
    (rubik-R rubik-R2 rubik-Ri)
    (rubik-L rubik-L2 rubik-Li)
    (rubik-B rubik-B2 rubik-Bi)
    (rubik-D rubik-D2 rubik-Di)
    (rubik-x rubik-x2 rubik-xi)
    (rubik-y rubik-y2 rubik-yi)
    (rubik-z rubik-z2 rubik-zi)))

;; patterns

(defun solver-letter-drawer (face-index)
  "Return function that will apply color from FACE to string."
  (let ((char (alist-get face-index solver-letters)))
    (lambda (str beg end _prn)
      (when _prn
        (let ((tmp (1+ beg)))
          (while (> end (1+ tmp))
            (aset str tmp char)
            (incf tmp)))))))

(defun solver-pattern-set-gen (state pattern val test)
  "Change STATE by setting elements according to PATTERN to VAL. TEST is used to interpret PATTERN."
  (cl-loop for el across pattern
           for idx = 0 then (1+ idx)
           when (funcall test el idx) do
           (aset state idx val)
           finally return state))

(defun solver-pattern-set (state pattern val)
  "Set elements in STATE where elements in PATTERN are non zero to VAL."
  (solver-pattern-set-gen state pattern val (lambda (el idx) (> el 0))))

(defun solver-pattern-set-rev (state pattern val)
  "Set elements in STATE where elements in PATTERN are zero to VAL."
  (solver-pattern-set-gen state pattern val (lambda (el idx) (= el 0))))

(defconst solver-top-pattern
  (let ((temp-vec (cl-coerce rubik-turn-top-substitution 'vector))
        (pat (make-vector (* 9 6) 0)))
    (solver-pattern-set-gen pat temp-vec 1 (lambda (el idx)
                                             (or (< idx 9)
                                                 (not (= el idx)))))))

(defconst solver-bottom-pattern
  (let ((state (copy-sequence solver-top-pattern)))
    (rubik-apply-transformation state rubik-x2)
    state))

(defconst solver-middle-pattern
  (let ((pat (make-vector (* 6 9) 1)))
    (solver-pattern-set pat solver-top-pattern 0)
    (solver-pattern-set pat solver-bottom-pattern 0)
    pat))

;; state variables
(defvar-local solver-mark 2)

(defvar-local solver-base-state []
  "Initial state of cube that is being solved")

(defvar-local solver-target-state []
  "Final state of cube that is being solved")

;; solver functions

(defun solver-prepare-states ()
  "Generate base and target states from current cube state"
  (setq solver-base-state
        (cl-loop for el across rubik-code-state
                 with temp = 1
                 for new-el = (cl-case el
                                (0 0)
                                (1 (incf temp))
                                (t (+ el 60)))
                 vconcat new-el))
  (setq solver-target-state (copy-sequence solver-base-state))
  (let ((class (make-vector 3 0))
        indice elems)
    (cl-loop for el across solver-target-state
             for idx = 0 then (1+ idx)
             if (> el 60) do
             (progn
               (push el elems)
               (push idx indice)
               (cl-case (mod idx 9)
                 (4 (incf (aref class 0)))
                 ((0 2 6 8) (incf (aref class 1)))
                 ((1 3 5 7) (incf (aref class 2)))))
             finally
             (progn (nreverse elems)
                    (nreverse indice)))
    (if (= 0 (cl-reduce #'+ class))
        (user-error "Nothing to solve for")
      (if (> (aref class 0) 0)
          (user-error "Cannot solve for central squares")
        (if (> (* (aref class 1)
                  (aref class 2))
               0)
            (user-error "Cannot move squares between middle and corner")
          (if (< (+ (aref class 1)
                    (aref class 2))
                 2)
              (user-error "Two or more squares must be marked to have a target for solving")
            (setcdr (lastcdr indice) indice)
            (cl-loop for el in elems
                     for ind = (cdr indice) then (cdr ind)
                     do (aset solver-target-state (car ind) el))))))))

(cl-iter-defun
 solver-seeker-deep (depth used-transformations)
 "Iterator that performs depth-first search up to DEPTH among USED-TRANSFORMATIONS. If no transformations are supplied, `solver-transformation-groups' are used instead."
 (let* ((res (list (cl-loop for i from 1 to (* 9 6) vconcat (list (1- i)))))
        (transforms (list (if used-transformations
                              used-transformations
                            solver-transformation-groups)))
        (pos-upper transforms)
        (pos-lower (caar pos-upper)))
   ;; yield identity
   (cl-iter-yield res)
   ;; we stop where there is no more elements to inspect
   (while pos-upper
     (if pos-lower
         ;; yield element and go down
         (progn
           (push (cdr res (car pos-lower)))
           (setcar res (rubik-substitute (car res) (car pos-lower)))
           (cl-iter-yield res)
           ;; now go down
           (if (> (1+ depth) (length res))
               (progn
                 (push pos-lower pow-upper)
                 (if (member (car pos-lower) (car transforms))
                     (push (cdr transforms) pos-upper)
                   (push transforms pos-upper))
                 (setq pos-lower (caar pos-upper)))
             ;; remove element and move forward
             (setcar res (rubik-substract-subst (car res) (cadr res)))
             (pop (cdr res))
             (setq pos-lower (cdr pos-lower))))
       (setcar pos-upper (cdar pos-upper))
       ;; skip same command group if needed
       (when (member (cadr res) (caar pos-upper))
         (setcar pos-upper (cdar pos-upper)))
       (if (car pos-upper)
           (setq pos-lower (caar pos-upper))
         (pop pos-upper)
         (setq pos-lower (cdr pop pos-upper))
         ;; remove element as we move on
         (setcar res (rubik-substract-subst (car res) (cadr res)))
         (pop (cdr res)))))))

;; commands

(defun solver-mark (idx)
  (let ((temp-v (vconcat (cl-mapcar
                          (lambda (el) (if (= el solver-mark) 0 el))
                          rubik-cube-state))))
    (aset temp-v idx solver-mark)
    (incf solver-mark)
    (when (> solver-mark 6) (setq solver-mark 2))
    (cl-shiftf rubik-cube-state temp-v)))

(defmacro solver-define-marker (idx)
  (let ((command-name (intern (concat (symbol-name 'solver-mark)
                                      "-"
                                      (number-to-string idx)))))
    `(defun ,command-name ()
       (interactive)
       (solver-mark ,(1- idx))
       (rubik-draw-all))))

(defmacro solver-define-markers ()
  (let ((defuns
          (cl-loop for i from 1 to 9
                   collect `(solver-define-marker ,i))))
    (macroexp-progn defuns)))

(solver-define-markers)

(defun solver-slice-1-same ()
  (interactive)
  (solver-pattern-set rubik-cube-state solver-top-pattern 1)
  (rubik-draw-all))

(defun solver-slice-2-same ()
  (interactive)
  (solver-pattern-set rubik-cube-state solver-middle-pattern 1)
  (rubik-draw-all))

(defun solver-slice-3-same ()
  (interactive)
  (solver-pattern-set rubik-cube-state solver-bottom-pattern 1)
  (rubik-draw-all))

(defun solver-slice-1-any ()
  (interactive)
  (solver-pattern-set rubik-cube-state solver-top-pattern 0)
  (rubik-draw-all))

(defun solver-slice-2-any ()
  (interactive)
  (solver-pattern-set rubik-cube-state solver-middle-pattern 0)
  (rubik-draw-all))

(defun solver-slice-3-any ()
  (interactive)
  (solver-pattern-set rubik-cube-state solver-bottom-pattern 0)
  (rubik-draw-all))


;; Necessary overrides

(defun solver-make-initial-cube ()
  (make-vector (* 6 9) 0))

(defun solver-setup ()
  "Modify rubik's cube mode to work as solver"
  (advice-add 'rubik-make-initial-cube :override
              'solver-make-initial-cube)
  (setq rubik-painter-function 'solver-letter-drawer)
  (local-set-key (kbd "SPC 1") 'solver-slice-1-any)
  (local-set-key (kbd "SPC 2") 'solver-slice-2-any)
  (local-set-key (kbd "SPC 3") 'solver-slice-3-any)
  (local-set-key (kbd "* 1") 'solver-slice-1-same)
  (local-set-key (kbd "* 2") 'solver-slice-2-same)
  (local-set-key (kbd "* 3") 'solver-slice-3-same)
  (local-set-key (kbd "m 1") 'solver-mark-1)
  (local-set-key (kbd "m 2") 'solver-mark-2)
  (local-set-key (kbd "m 3") 'solver-mark-3)
  (local-set-key (kbd "m 4") 'solver-mark-4)
  (local-set-key (kbd "m 5") 'solver-mark-5)
  (local-set-key (kbd "m 6") 'solver-mark-6)
  (local-set-key (kbd "m 7") 'solver-mark-7)
  (local-set-key (kbd "m 8") 'solver-mark-8)
  (local-set-key (kbd "m 9") 'solver-mark-9))

(defun solver-teardown ()
  "Remove all advices from rubik's cube mode functions"
  (advice-remove 'rubik-make-initial-cube 'solver-make-initial-cube)
  (setq rubik-painter-function 'rubik-color-painter)
  (local-unset-key (kbd "SPC 1"))
  (local-unset-key (kbd "SPC 2"))
  (local-unset-key (kbd "SPC 3"))
  (local-unset-key (kbd "* 1"))
  (local-unset-key (kbd "* 2"))
  (local-unset-key (kbd "* 3"))
  (local-unset-key (kbd "m 1"))
  (local-unset-key (kbd "m 2"))
  (local-unset-key (kbd "m 3"))
  (local-unset-key (kbd "m 4"))
  (local-unset-key (kbd "m 5"))
  (local-unset-key (kbd "m 6"))
  (local-unset-key (kbd "m 7"))
  (local-unset-key (kbd "m 8"))
  (local-unset-key (kbd "m 9")))
