;;;; Puyopuyo board state representation

(defvar colors '(red blue purple yellow green trash))

(defclass board ()
  ((width :accessor w :initform 6
          :initarg :w :type integer)
   (height :accessor h :initform 12
           :initarg :h :type integer)
   (state :accessor state
          :initarg :state)))

(defclass puyo ()
  (color))
