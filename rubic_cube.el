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
  (let ((res (make-vector (* 9 6) 0)))
    (loop for i from 0 to 5
          do (loop for j from 0 to 8
                   do (aset res (+ j (* i 9)) i)))
    res))

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
