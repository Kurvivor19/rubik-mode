;;   -*- lexical-binding: t; -*-
;;; rubic_cube.el
;; emulation and solution of rubic's cube in emacs

(defconst rubic-cube-front-default '("             / \\             "
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
(defconst rubic-cube-back-default '("            _/|\\_            "
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

(defvar rubic-cuve-back nil
  "Canbas for front of rubic's cube")

(defvar rubic-cube-front nil
  "Canvas for back of rubic's cube")

(defconst side-coord '[(0 14) (7 3) (11 17) (5 5) (1 15) (9 14)]
  "Coordinates of starting points of the cube sides")

(defface rubic-red
  '((t . (:background "red")))
  "Red colored cube side")
(defface rubic-orange
  '((t . (:background "orange")))
  "Orange colored cube side")
(defface rubic-white
  '((t . (:background "antique white")))
  "white colored cube side")
(defface rubic-blue
  '((t . (:background "blue")))
  "Blue colored cube side")
(defface rubic-yellow
  '((t . (:background "yellow")))
  "Yellow colored cube side")
(defface rubic-green
  '((t . (:background "green")))
  "green colored cube side")

(defconst rubic-faces [rubic-red
                       rubic-white
                       rubic-green
                       rubic-yellow
                       rubic-blue
                       rubic-orange])

(defun rubic-color-painter (face)
  "make function that will apply color to string"
  (lambda (str beg end prn)
    (add-face-text-property beg end face t str)))

(defun rubic-paint-diamond (cube-screen coord painter)
  "Paint diamond shape with provided paint function"
  (let ((l (first coord))
        (c (second coord)))
    (apply painter (nth l cube-screen) c (1+ c) nil nil)
    (apply painter (nth (incf l) cube-screen) (- c 2) (+ c 3) t nil)
    (apply painter (nth (incf l) cube-screen) (- c 2) (+ c 3) nil nil)
    (apply painter (nth (incf l) cube-screen) c (1+ c) nil nil)))

(defun rubic-paint-slope-down (cube-screen coord painter)
  "Paint downward slope with provided paint function"
    (let ((l (first coord))
          (c (second coord)))
      (apply painter (nth l cube-screen) c (+ c 2) nil nil)
      (apply painter (nth (incf l) cube-screen) c (+ c 3) t nil)
      (apply painter (nth (incf l) cube-screen) (1+ c) (+ c 3) nil nil)))

(defun rubic-paint-slope-up (cube-screen coord painter)
  "Paint upwards slope with provided paint function"
    (let ((l (first coord))
          (c (second coord)))
      (apply painter (nth l cube-screen) (1- c) (1+ c) nil nil)
      (apply painter (nth (incf l) cube-screen) (- c 2) (1+ c) t nil)
      (apply painter (nth (incf l) cube-screen) (- c 2) c nil nil)))

(defun rubic-make-initial-cube ()
  (loop with res = (make-vector (* 9 6) 0)
        for i from 0 to 5
        do (loop for j from 0 to 8
                 do (aset res (+ j (* i 9)) i))
        finally return res))

(defun rubic-displace-diamond (coord local-number)
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

(defun rubic-displace-downslope (coord local-number)
  "Calculate displacements of te cell of front and left sides"
  (let ((l (first coord))
        (c (second coord))
        (row (/ local-number 3))
        (col (mod local-number 3)))
    (setq l (+ l (* 3 row)))
    (setq l (+ l (* 2 col)))
    (setq c (+ c (* 4 col)))
    (list l c)))

(defun rubic-displace-upslope (coord local-number)
  "Calculate displacements of te cell of front and left sides"
  (let ((l (first coord))
        (c (second coord))
        (row (/ local-number 3))
        (col (mod local-number 3)))
    (setq l (+ l (* 3 row)))
    (setq l (- l (* 2 col)))
    (setq c (+ c (* 4 col)))
    (list l c)))

(defun rubic-get-local-number (cell-number)
  (list (/ cell-number 9) (mod cell-number 9)))

(defun rubic-paint-cell (front back number color)
  (destructuring-bind (side local-number) (rubic-get-local-number number)
    (let ((canvas (if (< side 3) front back))
          (painter (rubic-color-painter (aref rubic-faces color)))
          (filler (case side
                    ((0 5) 'rubic-paint-diamond)
                    ((1 4) 'rubic-paint-slope-down)
                    ((2 3) 'rubic-paint-slope-up)))
          (translator (case side
                        ((0 5) 'rubic-displace-diamond)
                        ((1 4) 'rubic-displace-downslope)
                        ((2 3) 'rubic-displace-upslope))))
      (apply filler canvas (apply translator (aref side-coord side) local-number nil) painter nil))))


(defun rubic-paint-cube (cube-state)
  (setq rubic-cube-front (mapcar #'copy-sequence rubic-cube-front-default))
  (setq rubic-cube-back (mapcar #'copy-sequence rubic-cube-back-default))
  (loop for i from 1 to (* 6 9) do
        (rubic-paint-cell rubic-cube-front rubic-cube-back (1- i) (aref cube-state (1- i)))))

;; definitions for rotational notation
(require 'calc)
(defconst rubic-i '(cplx 0 1))
(defconst rubic-neg-i '(cplx 0 -1))

(defconst rubic-desc-rot-identity
  '((0 1) (1 1) (2 1) (3 1) (4 1) (5 1)))

(defconst rubic-desc-rot-1-2
  `((1 1) (5 ,rubic-i) (2 ,rubic-neg-i) (0 -1) (4 ,rubic-i) (3 ,rubic-i)))

(defconst rubic-desc-rot-1-3
  `((2 ,rubic-i) (1 ,rubic-i) (5 -1) (3 ,rubic-neg-i) (0 ,rubic-i) (4 1)))

(defconst rubic-desc-rot-2-3
  `((0 ,rubic-neg-i) (2 1) (3 1) (4 1) (1 1) (5 ,rubic-i)))

(defun rubic-compose-rot (rot-1 rot-2)
  "Make rotation from two defined ones"
  (loop for (n1 r1) in rot-1
        for (n2 r2) = (nth n1 rot-2)
        collect (list n2 (math-mul r1 r2))))

(defun rubic-rotate (&rest turns)
  "Compose as many rotations as needed"
  (reduce 'rubic-compose-rot turns))

(defconst rubicside-renumbering-clockwise
  '(6 3 0 7 4 1 8 5 2))

(defun rubic-compose-subst (sub1 sub2)
  (loop for i1 in sub1
        for i2 = (nth i1 sub2)
        collect i2))

(defun rubic-substitute (&rest subs)
  (reduce 'rubic-compose-subst subs))

(defconst rubic-side-rotations
  (list (cons rubic-i rubicside-renumbering-clockwise)
        (cons -1 (rubic-substitute rubicside-renumbering-clockwise
                                   rubicside-renumbering-clockwise))
        (cons rubic-neg-i (rubic-substitute rubicside-renumbering-clockwise
                                            rubicside-renumbering-clockwise
                                            rubicside-renumbering-clockwise))
        ;; we could call with 4 rotations or define a constant for identity. But therte is no need, really
        (cons 1 (list 0 1 2 3 4 5 6 7 8))))

(defun rubic-expand-substitution (sub)
  "Get substitution for entire cube state"
  (loop with res = (make-vector (* 9 6) 0)
        for i from 0 to 5
        for (side-number rot) = (nth i sub)
        for side-reorder = (cdr (assoc rot rubic-side-rotations))
        for base-offset = (* i 9)
        for target-offset = (* side-number 9)
        do (loop for j from 0 to 8
                 do
                 (setf (aref res (+ j target-offset))
                       (+ base-offset (nth j side-reorder))))
        finally return (append res nil)))

(defconst rubic-turn-top-substitution
  (let ((temp (coerce (loop for i from 0 to (1- (* 9 6)) collect i) 'vector))
        (side-offsets '((9 36) (18 9) (27 18) (36 27)))
        (rotation (rubic-substitute rubicside-renumbering-clockwise
                                    rubicside-renumbering-clockwise
                                    rubicside-renumbering-clockwise)))
    (loop for i from 0 to 8 do (aset temp i (nth i rotation)))
    (loop for (loc num) in side-offsets do
          (loop for i from 0 to 2 do
                (aset temp (+ loc i) (+ num i))))
    (append temp nil)))

(defun rubic-apply-transformation (state transform)
  (let ((second (copy-sequence state)))
    (loop for i from 0 to (1- (length second)) do
          (aset state (nth i transform) (aref second i)))))

;; definitions of common transformations
(defconst rubic-U
  rubic-turn-top-substitution)

(defconst rubic-U2
  (rubic-substitute rubic-turn-top-substitution
                    rubic-turn-top-substitution))

(defconst rubic-Ui
  (rubic-substitute rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution))

(defconst rubic-F
  (rubic-substitute (rubic-expand-substitution rubic-desc-rot-1-2)
                    rubic-turn-top-substitution
                    (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2))))

(defconst rubic-F2
  (rubic-substitute (rubic-expand-substitution rubic-desc-rot-1-2)
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2))))

(defconst rubic-Fi
  (rubic-substitute (rubic-expand-substitution rubic-desc-rot-1-2)
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2))))

(defconst rubic-R
  (rubic-substitute (rubic-expand-substitution rubic-desc-rot-1-3)
                    rubic-turn-top-substitution
                    (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-3
                                   rubic-desc-rot-1-3
                                   rubic-desc-rot-1-3))))

(defconst rubic-R2
  (rubic-substitute (rubic-expand-substitution rubic-desc-rot-1-3)
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-3
                                   rubic-desc-rot-1-3
                                   rubic-desc-rot-1-3))))

(defconst rubic-Ri
  (rubic-substitute (rubic-expand-substitution rubic-desc-rot-1-3)
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-3
                                   rubic-desc-rot-1-3
                                   rubic-desc-rot-1-3))))

(defconst rubic-L
  (rubic-substitute (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-3
                                   rubic-desc-rot-1-3
                                   rubic-desc-rot-1-3))
                    rubic-turn-top-substitution
                    (rubic-expand-substitution rubic-desc-rot-1-3)))

(defconst rubic-L2
  (rubic-substitute (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-3
                                   rubic-desc-rot-1-3
                                   rubic-desc-rot-1-3))
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    (rubic-expand-substitution rubic-desc-rot-1-3)))

(defconst rubic-Li
  (rubic-substitute (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-3
                                   rubic-desc-rot-1-3
                                   rubic-desc-rot-1-3))
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    (rubic-expand-substitution rubic-desc-rot-1-3)))

(defconst rubic-B
  (rubic-substitute (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2))
                    rubic-turn-top-substitution
                    (rubic-expand-substitution rubic-desc-rot-1-2)))

(defconst rubic-B2
  (rubic-substitute (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2))
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    (rubic-expand-substitution rubic-desc-rot-1-2)))

(defconst rubic-Bi
  (rubic-substitute (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2))
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    (rubic-expand-substitution rubic-desc-rot-1-2)))

(defconst rubic-D
  (rubic-substitute (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2))
                    rubic-turn-top-substitution
                    (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2))))

(defconst rubic-D2
  (rubic-substitute (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2))
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2))))

(defconst rubic-Di
  (rubic-substitute (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2))
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    rubic-turn-top-substitution
                    (rubic-expand-substitution
                     (rubic-rotate rubic-desc-rot-1-2
                                   rubic-desc-rot-1-2))))

(defconst rubic-x
  (rubic-expand-substitution rubic-desc-rot-1-2))

(defconst rubic-x2
  (rubic-expand-substitution
   (rubic-rotate rubic-desc-rot-1-2
                 rubic-desc-rot-1-2)))

(defconst rubic-xi
  (rubic-expand-substitution
   (rubic-rotate rubic-desc-rot-1-2
                 rubic-desc-rot-1-2
                 rubic-desc-rot-1-2)))

(defconst rubic-y
  (rubic-expand-substitution rubic-desc-rot-2-3))

(defconst rubic-y2
  (rubic-expand-substitution
   (rubic-rotate rubic-desc-rot-2-3
                 rubic-desc-rot-2-3)))

(defconst rubic-yi
  (rubic-expand-substitution
   (rubic-rotate rubic-desc-rot-2-3
                 rubic-desc-rot-2-3
                 rubic-desc-rot-2-3)))

(defconst rubic-zi
  (rubic-expand-substitution rubic-desc-rot-1-3))

(defconst rubic-z2
  (rubic-expand-substitution
   (rubic-rotate rubic-desc-rot-1-3
                 rubic-desc-rot-1-3)))

(defconst rubic-z
  (rubic-expand-substitution
   (rubic-rotate rubic-desc-rot-1-3
                 rubic-desc-rot-1-3
                 rubic-desc-rot-1-3)))

