;;;; Puyopuyo board state representation

(defvar colors '(red blue purple yellow green trash))

(defclass puyo ()
  ((color :accessor col :initarg :col)))

(defclass state ()
  ((array :accessor arr)
   (resolved :accessor res :initform t)))

(defclass board ()
  ((width :accessor w :initform 6
          :initarg :w :type integer)
   (height :accessor h :initform 12
           :initarg :h :type integer)
   (states :accessor states
          :initarg :states)))

(defvar state (make-array '() :element-type 'puyo :initial-element nil))

;;; Puyo Ops
(defgeneric is-trash (puyo))

(defmethod is-trash ((p puyo))
  (eq (col p) 'trash))

;;; State Ops
(defgeneric get-dimensions (object)) ;; board or state dimensions

(defmethod get-dimensions ((s state))
  (array-dimensions (arr s)))

(defgeneric get-puyo)      ;; gets a puyo at an x,y for a board/state?
(defgeneric is-resolved)   ;; checks for outstanding chains in a state

;;; Board Ops
(defmethod get-dimensions ((b board))
  (list (w b) (h b)))

(defgeneric state-count)   ;; amount of states in a board
(defgeneric add-state)     ;; push a state to the board state stack
(defgeneric project-states);; give next puyo drop and project resultant states

(defun new-board ((w 6) (h 12))
  (make-instance board :w w :h h))
