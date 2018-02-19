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

(defmacro solver-arrange-indice (elems indice)
  (let ((alist-temp (gensym))
        (el-temp (gensym))
        (idx-temp (gensym)))
    `(let ((,alist-temp
            (cl-loop for el in ,elems
                     for ind in ,indice
                     collect (cons el ind)))
           ,el-temp ,idx-temp)
       (setq ,alist-temp (cl-sort ,alist-temp #'< :key #'car))
       (cl-loop for (el . ind) in ,alist-temp
                do (progn (push el ,el-temp)
                          (push ind ,idx-temp)))
       (setq ,elems (nreverse ,el-temp))
       (setq ,indice (nreverse ,idx-temp)))))

(defun solver-prepare-states ()
  "Generate base and target states from current cube state"
  (setq solver-base-state
        (cl-loop for el across rubik-cube-state
                 with temp = 1
                 for new-el = (cl-case el
                                (0 (list 0))
                                (1 (list (incf temp)))
                                (t (list (+ el 60))))
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
             (progn
               (setf elems (nreverse elems)
                     indice (nreverse indice))))
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
            (solver-arrange-indice elems indice)
            (setcdr (last indice) indice)
            (cl-loop for el in elems
                     for ind = (cdr indice) then (cdr ind)
                     do (aset solver-target-state (car ind) el))))))))

(iter-defun
 solver-seeker-deep (depth &optional used-transformations)
 "Iterator that performs depth-first search up to DEPTH among USED-TRANSFORMATIONS. If no transformations are supplied, `solver-transformation-groups' are used instead."
 (cl-flet ((sym-val (s) (if (symbolp s) (symbol-value s) s)))
   (let* ((transforms (if used-transformations
                          used-transformations
                        solver-transformation-groups))
          (pos-upper (list (copy-tree transforms)))
          (pos-lower (caar pos-upper))
          (trans-length (length (sym-val (car pos-lower))))
          (res (cl-loop for _ from 1 to 2 collect
                        (cl-loop for i from 1 to trans-length
                                 collect (1- i)))))
     ;; yield identity
     (iter-yield res)
     (cl-flet
         ((get-expanded-result ()
                               (cons (rubik-substitute (car res) (sym-val (car pos-lower)))
                                     (cons (car pos-lower) (cdr res))))
          (go-down ()
                   (push (car pos-lower) (cdr res))
                   (setcar res (rubik-substitute (car res) (sym-val (cadr res))))
                   (push pos-lower pos-upper)
                   (if (member (cadr res) (car transforms))
                       (push (cdr transforms) pos-upper)
                     (push transforms pos-upper))
                   (setq pos-lower (caar pos-upper)))
          (go-up ()
                 (setcar res (rubik-substract-subst (car res) (sym-val (cadr res))))
                 (pop (cdr res))
                 (pop pos-upper)
                 (setq pos-lower (pop pos-upper)))
          (go-forward ()
                      (setq pos-lower (cdr pos-lower))
                      (unless pos-lower
                        (when pos-upper
                          (setcar pos-upper (cdar pos-upper))
                          (when (member (cadr res) (caar pos-upper))
                            (setcar pos-upper (cdar pos-upper)))
                          (setq pos-lower (caar pos-upper))))))
       ;; we stop where there is no more elements to inspect
       (while pos-upper
         (if pos-lower
             ;; yield element and go down
             (progn
               (iter-yield (get-expanded-result))
               ;; now go down
               (if (> (1+ depth) (length res))
                   (go-down)
                 (go-forward)))
           (go-forward)
           (unless (car pos-upper)
             (go-up)
             (go-forward))))))))

(defun solver-find-solution (depth)
  "Find sequence of transformations from `solver-base-state' to `solver-target-state' up to the length of DEPTH."
  (let (good-seq
        current-res
        (walker (solver-seeker-deep depth)))
    (iter-do (current-res walker)
      (let ((temp-state (rubik-apply-transformation solver-base-state
                                                    (car current-res))))
        (when (equal temp-state solver-target-state)
          (setq good-seq (cdr current-res))
          (iter-close walker))))
    good-seq))

;; commands

(defun solver-do-solve (depth)
  "Find solution for a given problem, searching amoung up to DEPTH states."
  (interactive "NDepth of a search: ")
  (solver-prepare-states)
  (let ((answer (solver-find-solution depth)))
    (when answer
      (with-current-buffer "*Messages*"
        (pp answer)))))

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
  (interactive)
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
  (interactive)
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
