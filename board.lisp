;;;; Puyopuyo board state representation

(defvar colors '(red blue purple yellow green trash))

(defclass puyo ()
  (color))

(defclass state ()
  ((array :accessor arr)
   (resolved :accessor res :initform t)))

(defclass board ()
  ((width :accessor w :initform 6
          :initarg :w :type integer)
   (height :accessor h :initform 12
           :initarg :h :type integer)
   (state :accessor state
          :initarg :state)
   (resolved :accessor res :initform t)))

(defvar state (make-array '() :element-type puyo))

(defgeneric state-count)   ;; amount of states in a board
(defgeneric add-state)     ;; push a state to the board state stack
(defgeneric get-dimensions);; board or state dimensions
(defgeneric get-puyo)      ;; gets a puyo at an x,y
(defgeneric is-resolved)   ;; checks for outstanding chains

(defun new-board ((w 6) (h 12))
  (make-instance board :w w :h h))
