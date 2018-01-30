;;   -*- lexical-binding: t; -*-
;;; rubik_cube.el
;; emulation and solution of rubik's cube in emacs

;; cube layout
(defconst rubik-cube-front-default '("             / \\             "
                                     "           /     \\           "
                                     "         / \\     / \\         "
                                     "       /     \\_/     \\       "
                                     "     / \\     / \\     / \\     "
                                     "   /     \\_/     \\_/     \\   "
                                     "  |\\_    / \\     / \\     /|  "
                                     "  |  \\ /     \\_/     \\_/  |  "
                                     "  |   |\\_    / \\     /|   |  "
                                     "  |\\_ |  \\ /     \\_/  | _/|  "
                                     "  |  \\|   |\\_   _/|   |/  |  "
                                     "  |   |\\_ |  \\ /  | _/|   |  "
                                     "  |\\_ |  \\|   |   |/  | _/|  "
                                     "  |  \\|   |\\  |  /|   |/  |  "
                                     "  |   |\\_ |  \\|/  | _/|   |  "
                                     "   \\_ |  \\|   |   |/  | _/   "
                                     "     \\|   |\\_ | _/|   |/     "
                                     "       \\_ |  \\|/  | _/       "
                                     "         \\|   |   |/         "
                                     "           \\_ | _/           "
                                     "             \\|/             "))
(defconst rubik-cube-back-default '("            _/|\\_            "
                                    "           /  |  \\           "
                                    "        _/|   |   |\\_        "
                                    "       /  | _/|\\_ |  \\       "
                                    "    _/|   |/  |  \\|   |\\_    "
                                    "   /  | _/|   |   |\\_ |  \\   "
                                    "  |   |/  | _/|\\_ |  \\|   |  "
                                    "  | _/|   |/  |  \\|   |\\_ |  "
                                    "  |/  | _/|   |   |\\_ |  \\|  "
                                    "  |   |/  | _/ \\_ |  \\|   |  "
                                    "  | _/|   |/     \\|   |\\_ |  "
                                    "  |/  | _/ \\_   _/ \\_ |  \\|  "
                                    "  |   |/     \\_/     \\|   |  "
                                    "  | _/ \\_    / \\_   _/ \\_ |  "
                                    "  |/     \\_/     \\_/     \\|  "
                                    "   \\_   _/ \\_   _/ \\_   _/   "
                                    "     \\ /     \\_/     \\_/     "
                                    "       \\_   _/ \\_   _/       "
                                    "         \\ /     \\_/         "
                                    "           \\_   _/           "
                                    "             \\_/             "))

(defvar-local rubik-cuve-back nil
  "Canbas for front of rubik's cube")

(defvar-local rubik-cube-front nil
  "Canvas for back of rubik's cube")

(defconst side-coord '[(0 14) (7 3) (11 17) (5 5) (1 15) (9 14)]
  "Coordinates of starting points of the cube sides")

;; faces
(defface rubik-red
  '((t . (:background "red")))
  "Red colored cube side")
(defface rubik-orange
  '((t . (:background "orange")))
  "Orange colored cube side")
(defface rubik-white
  '((t . (:background "antique white")))
  "white colored cube side")
(defface rubik-blue
  '((t . (:background "blue")))
  "Blue colored cube side")
(defface rubik-yellow
  '((t . (:background "yellow")))
  "Yellow colored cube side")
(defface rubik-green
  '((t . (:background "green")))
  "green colored cube side")

(defconst rubik-faces [rubik-red
                       rubik-white
                       rubik-green
                       rubik-yellow
                       rubik-blue
                       rubik-orange])

;; painter functions
(defun rubik-color-painter (face)
  "make function that will apply color to string"
  (lambda (str beg end prn)
    (add-face-text-property beg end face t str)))

(defun rubik-paint-diamond (cube-screen coord painter)
  "Paint diamond shape with provided paint function"
  (let ((l (first coord))
        (c (second coord)))
    (apply painter (nth l cube-screen) c (1+ c) nil nil)
    (apply painter (nth (incf l) cube-screen) (- c 2) (+ c 3) t nil)
    (apply painter (nth (incf l) cube-screen) (- c 2) (+ c 3) nil nil)
    (apply painter (nth (incf l) cube-screen) c (1+ c) nil nil)))

(defun rubik-paint-slope-down (cube-screen coord painter)
  "Paint downward slope with provided paint function"
    (let ((l (first coord))
          (c (second coord)))
      (apply painter (nth l cube-screen) c (+ c 2) nil nil)
      (apply painter (nth (incf l) cube-screen) c (+ c 3) t nil)
      (apply painter (nth (incf l) cube-screen) (1+ c) (+ c 3) nil nil)))

(defun rubik-paint-slope-up (cube-screen coord painter)
  "Paint upwards slope with provided paint function"
    (let ((l (first coord))
          (c (second coord)))
      (apply painter (nth l cube-screen) (1- c) (1+ c) nil nil)
      (apply painter (nth (incf l) cube-screen) (- c 2) (1+ c) t nil)
      (apply painter (nth (incf l) cube-screen) (- c 2) c nil nil)))

(defun rubik-make-initial-cube ()
  (loop with res = (make-vector (* 9 6) 0)
        for i from 0 to 5
        do (loop for j from 0 to 8
                 do (aset res (+ j (* i 9)) i))
        finally return res))

(defun rubik-displace-diamond (coord local-number)
  "Calculate displacement of the cell of top and bottom sides"
  (let ((l (first coord))
        (c (second coord))
        (row (/ local-number 3))
        (col (mod local-number 3)))
    (setq l (+ l (* 2 row)))
    (setq c (- c (* 4 row)))
    (setq l (+ l (* 2 col)))
    (setq c (+ c (* 4 col)))
    (list l c)))

(defun rubik-displace-downslope (coord local-number)
  "Calculate displacements of te cell of front and left sides"
  (let ((l (first coord))
        (c (second coord))
        (row (/ local-number 3))
        (col (mod local-number 3)))
    (setq l (+ l (* 3 row)))
    (setq l (+ l (* 2 col)))
    (setq c (+ c (* 4 col)))
    (list l c)))

(defun rubik-displace-upslope (coord local-number)
  "Calculate displacements of te cell of front and left sides"
  (let ((l (first coord))
        (c (second coord))
        (row (/ local-number 3))
        (col (mod local-number 3)))
    (setq l (+ l (* 3 row)))
    (setq l (- l (* 2 col)))
    (setq c (+ c (* 4 col)))
    (list l c)))

(defun rubik-get-local-number (cell-number)
  (list (/ cell-number 9) (mod cell-number 9)))

(defun rubik-paint-cell (front back number color)
  (destructuring-bind (side local-number) (rubik-get-local-number number)
    (let ((canvas (if (< side 3) front back))
          (painter (rubik-color-painter (aref rubik-faces color)))
          (filler (case side
                    ((0 5) 'rubik-paint-diamond)
                    ((1 4) 'rubik-paint-slope-down)
                    ((2 3) 'rubik-paint-slope-up)))
          (translator (case side
                        ((0 5) 'rubik-displace-diamond)
                        ((1 4) 'rubik-displace-downslope)
                        ((2 3) 'rubik-displace-upslope))))
      (apply filler canvas (apply translator (aref side-coord side) local-number nil) painter nil))))


(defun rubik-paint-cube (cube-state)
  (setq rubik-cube-front (mapcar #'copy-sequence rubik-cube-front-default))
  (setq rubik-cube-back (mapcar #'copy-sequence rubik-cube-back-default))
  (loop for i from 1 to (* 6 9) do
        (rubik-paint-cell rubik-cube-front rubik-cube-back (1- i) (aref cube-state (1- i)))))

;; definitions for rotational notation
(require 'calc)
(defconst rubik-i '(cplx 0 1))
(defconst rubik-neg-i '(cplx 0 -1))

(defconst rubik-desc-rot-identity
  '((0 1) (1 1) (2 1) (3 1) (4 1) (5 1)))

(defconst rubik-desc-rot-1-2
  `((1 1) (5 ,rubik-i) (2 ,rubik-neg-i) (0 -1) (4 ,rubik-i) (3 ,rubik-i)))

(defconst rubik-desc-rot-1-3
  `((2 ,rubik-i) (1 ,rubik-i) (5 -1) (3 ,rubik-neg-i) (0 ,rubik-i) (4 1)))

(defconst rubik-desc-rot-2-3
  `((0 ,rubik-neg-i) (2 1) (3 1) (4 1) (1 1) (5 ,rubik-i)))

(defun rubik-compose-rot (rot-1 rot-2)
  "Make rotation from two defined ones"
  (loop for (n1 r1) in rot-1
        for (n2 r2) = (nth n1 rot-2)
        collect (list n2 (math-mul r1 r2))))

(defun rubik-rotate (&rest turns)
  "Compose as many rotations as needed"
  (reduce 'rubik-compose-rot turns))

(defconst rubikside-renumbering-clockwise
  '(6 3 0 7 4 1 8 5 2))

(defun rubik-compose-subst (sub1 sub2)
  (loop for i1 in sub1
        for i2 = (nth i1 sub2)
        collect i2))

(defun rubik-substitute (&rest subs)
  (reduce 'rubik-compose-subst subs))

(defconst rubik-side-rotations
  (list (cons rubik-i rubikside-renumbering-clockwise)
        (cons -1 (rubik-substitute rubikside-renumbering-clockwise
                                   rubikside-renumbering-clockwise))
        (cons rubik-neg-i (rubik-substitute rubikside-renumbering-clockwise
                                            rubikside-renumbering-clockwise
                                            rubikside-renumbering-clockwise))
        ;; we could call with 4 rotations or define a constant for identity. But therte is no need, really
        (cons 1 (list 0 1 2 3 4 5 6 7 8))))

(defun rubik-expand-substitution (sub)
  "Get substitution for entire cube state"
  (loop with res = (make-vector (* 9 6) 0)
        for i from 0 to 5
        for (side-number rot) = (nth i sub)
        for side-reorder = (cdr (assoc rot rubik-side-rotations))
        for base-offset = (* i 9)
        for target-offset = (* side-number 9)
        do (loop for j from 0 to 8
                 do
                 (setf (aref res (+ j target-offset))
                       (+ base-offset (nth j side-reorder))))
        finally return (append res nil)))

(defconst rubik-turn-top-substitution
  (let ((temp (coerce (loop for i from 0 to (1- (* 9 6)) collect i) 'vector))
        (side-offsets '((9 36) (18 9) (27 18) (36 27)))
        (rotation (rubik-substitute rubikside-renumbering-clockwise
                                    rubikside-renumbering-clockwise
                                    rubikside-renumbering-clockwise)))
    (loop for i from 0 to 8 do (aset temp i (nth i rotation)))
    (loop for (loc num) in side-offsets do
          (loop for i from 0 to 2 do
                (aset temp (+ loc i) (+ num i))))
    (append temp nil)))

(defun rubik-apply-transformation (state transform)
  (let ((second (copy-sequence state)))
    (loop for i from 0 to (1- (length second)) do
          (aset state (nth i transform) (aref second i)))))

;; definitions of common transformations
(defconst rubik-U
  rubik-turn-top-substitution)

(defconst rubik-U2
  (rubik-substitute rubik-turn-top-substitution
                    rubik-turn-top-substitution))

(defconst rubik-Ui
  (rubik-substitute rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution))

(defconst rubik-F
  (rubik-substitute (rubik-expand-substitution rubik-desc-rot-1-2)
                    rubik-turn-top-substitution
                    (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2))))

(defconst rubik-F2
  (rubik-substitute (rubik-expand-substitution rubik-desc-rot-1-2)
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2))))

(defconst rubik-Fi
  (rubik-substitute (rubik-expand-substitution rubik-desc-rot-1-2)
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2))))

(defconst rubik-R
  (rubik-substitute (rubik-expand-substitution rubik-desc-rot-1-3)
                    rubik-turn-top-substitution
                    (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-3
                                   rubik-desc-rot-1-3
                                   rubik-desc-rot-1-3))))

(defconst rubik-R2
  (rubik-substitute (rubik-expand-substitution rubik-desc-rot-1-3)
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-3
                                   rubik-desc-rot-1-3
                                   rubik-desc-rot-1-3))))

(defconst rubik-Ri
  (rubik-substitute (rubik-expand-substitution rubik-desc-rot-1-3)
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-3
                                   rubik-desc-rot-1-3
                                   rubik-desc-rot-1-3))))

(defconst rubik-L
  (rubik-substitute (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-3
                                   rubik-desc-rot-1-3
                                   rubik-desc-rot-1-3))
                    rubik-turn-top-substitution
                    (rubik-expand-substitution rubik-desc-rot-1-3)))

(defconst rubik-L2
  (rubik-substitute (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-3
                                   rubik-desc-rot-1-3
                                   rubik-desc-rot-1-3))
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    (rubik-expand-substitution rubik-desc-rot-1-3)))

(defconst rubik-Li
  (rubik-substitute (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-3
                                   rubik-desc-rot-1-3
                                   rubik-desc-rot-1-3))
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    (rubik-expand-substitution rubik-desc-rot-1-3)))

(defconst rubik-B
  (rubik-substitute (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2))
                    rubik-turn-top-substitution
                    (rubik-expand-substitution rubik-desc-rot-1-2)))

(defconst rubik-B2
  (rubik-substitute (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2))
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    (rubik-expand-substitution rubik-desc-rot-1-2)))

(defconst rubik-Bi
  (rubik-substitute (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2))
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    (rubik-expand-substitution rubik-desc-rot-1-2)))

(defconst rubik-D
  (rubik-substitute (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2))
                    rubik-turn-top-substitution
                    (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2))))

(defconst rubik-D2
  (rubik-substitute (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2))
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2))))

(defconst rubik-Di
  (rubik-substitute (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2))
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    rubik-turn-top-substitution
                    (rubik-expand-substitution
                     (rubik-rotate rubik-desc-rot-1-2
                                   rubik-desc-rot-1-2))))

(defconst rubik-x
  (rubik-expand-substitution rubik-desc-rot-1-2))

(defconst rubik-x2
  (rubik-expand-substitution
   (rubik-rotate rubik-desc-rot-1-2
                 rubik-desc-rot-1-2)))

(defconst rubik-xi
  (rubik-expand-substitution
   (rubik-rotate rubik-desc-rot-1-2
                 rubik-desc-rot-1-2
                 rubik-desc-rot-1-2)))

(defconst rubik-y
  (rubik-expand-substitution rubik-desc-rot-2-3))

(defconst rubik-y2
  (rubik-expand-substitution
   (rubik-rotate rubik-desc-rot-2-3
                 rubik-desc-rot-2-3)))

(defconst rubik-yi
  (rubik-expand-substitution
   (rubik-rotate rubik-desc-rot-2-3
                 rubik-desc-rot-2-3
                 rubik-desc-rot-2-3)))

(defconst rubik-zi
  (rubik-expand-substitution rubik-desc-rot-1-3))

(defconst rubik-z2
  (rubik-expand-substitution
   (rubik-rotate rubik-desc-rot-1-3
                 rubik-desc-rot-1-3)))

(defconst rubik-z
  (rubik-expand-substitution
   (rubik-rotate rubik-desc-rot-1-3
                 rubik-desc-rot-1-3
                 rubik-desc-rot-1-3)))

;; mode definitions
(defvar-local cube-state nil
  "State of the cube that is being processed")

(defvar-local cube-undo (list 'undo)
  "List of previously executed commands")

(defvar-local cube-redo (list 'redo)
  "List of previously undone commands")

(defun rubik-center-string (str l)
  (if (> (length str) l)
      (substring str l)
    (let* ((sl (length str))
           (pad (/ (- l sl) 2))
           (rem (- l pad)))
      (concat (format (format "%%%ds" rem) str) (make-string pad ? )))))

(defun rubik-display-cube ()
  "Draw current cube state, assuming empty buffer"
  ;; first, update canvasses according to  the cube state
  (rubik-paint-cube cube-state)
  (let ((w (length (first rubik-cube-front-default)))
        (h (length rubik-cube-front-default)))
    (insert (format "%s%s\n"
                    (rubik-center-string "*FRONT*" w)
                    (rubik-center-string "*BACK*" w)))
    (insert-rectangle rubik-cube-front)
    (forward-line (- (decf h)))
    (move-end-of-line nil)
    (insert-rectangle rubik-cube-back)))

(defun rubik-display-undo ()
  "Print undo information"
  (loop with line-str = "\nUndo: "
        for cmd in (reverse (cdr cube-undo))
        for i = 1 then (1+ i)
        do (progn
             (setq line-str (concat line-str (format "%d. %s " i (get cmd 'name))))
             (when (> (length line-str) fill-column)
               (insert line-str)
               (setq line-str (concat "\n" (make-string 6 ? )))))
        finally (insert line-str)))

(defun rubik-display-redo ()
  "Print undo information"
  (loop with line-str = "\nRedo: "
        for cmd in (cdr cube-redo)
        for i = 1 then (1+ i)
        do (progn
             (setq line-str (concat line-str (format "%d. %s " i (get cmd 'name))))
             (when (> (length line-str) fill-column)
               (insert line-str)
               (setq line-str (concat "\n" (make-string 6 ? )))))
        finally (insert line-str)))

(defun rubik-draw-all ()
  (setq buffer-read-only nil)
  (erase-buffer)
  (rubik-display-cube)
  (rubik-display-undo)
  (rubik-display-redo)
  (setq buffer-read-only t))

(defmacro rubik-define-command (transformation desc)
  (let ((command-name (intern (concat (symbol-name transformation) "-command"))))
    (put command-name 'name desc)
    `(defun ,command-name (&optional arg)
       (interactive)
       (rubik-apply-transformation cube-state ,transformation)
       (unless arg
         (setcdr cube-redo nil)
         (setcdr cube-undo (cons (quote ,command-name) (cdr cube-undo)))
         (rubik-draw-all)))))

(defmacro rubik-define-commands (&rest transformations)
  (let ((head (car transformations))
        (command-name (cadr transformations))
        (tail (cddr transformations)))
    `(progn
       ,(when head
          `(rubik-define-command ,head ,command-name))
       ,(when tail
          `(rubik-define-commands ,@tail)))))

(rubik-define-commands rubik-U "Upper" rubik-U2 "Upper twice" rubik-Ui "Upper inverted"
                       rubik-F "Front" rubik-F2 "Front twice" rubik-Fi "Front inverted"
                       rubik-R "Right" rubik-R2 "Right twice" rubik-Ri "Right inverted"
                       rubik-L "Left" rubik-L2 "Left twice" rubik-Li "Left inverted"
                       rubik-B "Back" rubik-B2 "Back twice" rubik-Bi "Back inverted"
                       rubik-D "Down" rubik-D2 "Down twice" rubik-Di "Down inverted"
                       rubik-x "X rotation" rubik-x2 "X overturn" rubik-xi "X rotation inverted"
                       rubik-y "Y rotation" rubik-y2 "Y overturn" rubik-yi "Y rotation inverted"
                       rubik-z "Z rotation" rubik-z2 "Z overturn" rubik-zi "Z rotation inverted")

(defconst rubik-reverse-commands
  '((rubik-U-command . rubik-Ui-command)
    (rubik-U2-command . rubik-U2-command)
    (rubik-F-command . rubik-Fi-command)
    (rubik-F2-command . rubik-F2-command)
    (rubik-R-command . rubik-Ri-command)
    (rubik-R2-command . rubik-R2-command)
    (rubik-L-command . rubik-Li-command)
    (rubik-L2-command . rubik-L2-command)
    (rubik-B-command . rubik-Bi-command)
    (rubik-B2-command . rubik-B2-command)
    (rubik-D-command . rubik-Di-command)
    (rubik-D2-command . rubik-D2-command)
    (rubik-x-command . rubik-xi-command)
    (rubik-x2-command . rubik-x2-command)
    (rubik-y-command . rubik-yi-command)
    (rubik-y2-command . rubik-y2-command)
    (rubik-z-command . rubik-zi-command)
    (rubik-z2-command . rubik-z2-command)))

(defun rubik-reset ()
  "Set cube to initial state"
  (interactive)
  (setq cube-state (rubik-make-initial-cube))
  (setq cube-undo (list 'undo))
  (setq cube-redo (list 'redo))
  (rubik-draw-all))

(defun rubik-undo (&optional arg)
  "Undo up to ARG commands from undo list"
  (interactive "p")
  (let ((num (or arg 1)))
    (loop repeat num
          for lastcmd = (cadr cube-undo)
          when lastcmd do
          (let ((revcmd (or (cdr (assoc lastcmd rubik-reverse-commands))
                            (car (rassoc lastcmd rubik-reverse-commands)))))
            (setcdr cube-redo (cons lastcmd (cdr cube-redo)))
            (setcdr cube-undo (cddr cube-undo))
            (apply revcmd '(t))))
    (rubik-draw-all)))

(defun rubik-redo (&optional arg)
  "Redo up to ARG commands"
  (interactive "p")
  (let ((num (or arg 1)))
    (loop repeat num
          for lastcmd = (cadr cube-redo)
          when lastcmd do
          (progn
            (setcdr cube-undo (cons lastcmd (cdr cube-undo)))
            (setcdr cube-redo (cddr cube-redo))
            (apply lastcmd '(t))))
    (rubik-draw-all)))

(defvar rubik-mode-map
  (let ((map (make-sparse-keymap))
        (map-i (make-sparse-keymap))
        (map-2 (make-sparse-keymap)))
    (define-key map "u" 'rubik-U-command)
    (define-key map-2 "u" 'rubik-U2-command)
    (define-key map-i "u" 'rubik-Ui-command)
    (define-key map "f" 'rubik-F-command)
    (define-key map-2 "f" 'rubik-F2-command)
    (define-key map-i "f" 'rubik-Fi-command)
    (define-key map "r" 'rubik-R-command)
    (define-key map-2 "r" 'rubik-R2-command)
    (define-key map-i "r" 'rubik-Ri-command)
    (define-key map "l" 'rubik-L-command)
    (define-key map-2 "l" 'rubik-L2-command)
    (define-key map-i "l" 'rubik-Li-command)
    (define-key map "b" 'rubik-B-command)
    (define-key map-2 "b" 'rubik-B2-command)
    (define-key map-i "b" 'rubik-Bi-command)
    (define-key map "d" 'rubik-D-command)
    (define-key map-2 "d" 'rubik-D2-command)
    (define-key map-i "d" 'rubik-Di-command)
    (define-key map "x" 'rubik-x-command)
    (define-key map-2 "x" 'rubik-x2-command)
    (define-key map-i "x" 'rubik-xi-command)
    (define-key map "y" 'rubik-y-command)
    (define-key map-2 "y" 'rubik-y2-command)
    (define-key map-i "y" 'rubik-yi-command)
    (define-key map "z" 'rubik-z-command)
    (define-key map-2 "z" 'rubik-z2-command)
    (define-key map-i "z" 'rubik-zi-command)
    (define-key map "g" 'rubik-reset)
    (define-key map "2" map-2)
    (define-key map "i" map-i)
    (define-key map (kbd "M-u") 'rubik-undo)
    (define-key map (kbd "M-r") 'rubik-redo)
    map))

(define-derived-mode rubik-mode special-mode
  "rubik's cube")

(add-hook 'rubik-mode-hook
          (lambda ()
            (rubik-reset)))

(defun rubik ()
  "Prepare for rubik mode"
  (interactive)
  (switch-to-buffer "*Rubik*")
  (rubik-mode))


