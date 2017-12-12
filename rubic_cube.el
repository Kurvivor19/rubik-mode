;;   -*- lexical-binding: t; -*-
;;; rubic_cube.el
;; emulation and solution of rubic's cube in emacs

(defconst rubic-cube-front-default '("             / \\             "
                                     "           /     \\           "
                                     "         / \\     / \\         "
                                     "       /     \\ /     \\       "
                                     "     / \\     / \\     / \\     "
                                     "   /     \\ /     \\ /     \\   "
                                     "  |\\     / \\     / \\     /|  "
                                     "  |  \\ /     \\ /     \\ /  |  "
                                     "  |   |\\     / \\     /|   |  "
                                     "  |\\  |  \\ /     \\ /  |  /|  "
                                     "  |  \\|   |\\     /|   |/  |  "
                                     "  |   |\\  |  \\ /  |  /|   |  "
                                     "  |\\  |  \\|   |   |/  |  /|  "
                                     "  |  \\|   |\\  |  /|   |/  |  "
                                     "  |   |\\  |  \\|/  |  /|   |  "
                                     "   \\  |  \\|   |   |/  |  /   "
                                     "     \\|   |\\  |  /|   |/     "
                                     "       \\  |  \\|/  |  /       "
                                     "         \\|   |   |/         "
                                     "           \\  |  /           "
                                     "             \\|/             "))
(defconst rubic-cube-back-default '("             /|\\             "
                                    "           /  |  \\           "
                                    "         /|   |   |\\         "
                                    "       /  |  /|\\  |  \\       "
                                    "     /|   |/  |  \\|   |\\     "
                                    "   /  |  /|   |   |\\  |  \\   "
                                    "  |   |/  |  /|\\  |  \\|   |  "
                                    "  |  /|   |/  |  \\|   |\\  |  "
                                    "  |/  |  /|   |   |\\  |  \\|  "
                                    "  |   |/  |  / \\  |  \\|   |  "
                                    "  |  /|   |/     \\|   |\\  |  "
                                    "  |/  |  / \\     / \\  |  \\|  "
                                    "  |   |/     \\ /     \\|   |  "
                                    "  |  / \\     / \\     / \\  |  "
                                    "  |/     \\ /     \\ /     \\|  "
                                    "   \\     / \\     / \\     /   "
                                    "     \\ /     \\ /     \\ /     "
                                    "       \\     / \\     /       "
                                    "         \\ /     \\ /         "
                                    "           \\     /           "
                                    "             \\ /             "))

(defconst side-coord '((0 14) (7 3) (7 25) (1 13) (1 15) (9 14))
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
