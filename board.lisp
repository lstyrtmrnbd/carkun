;;;; Puyopuyo game states representation

(defvar colors '(red blue purple yellow green trash))

(defclass puyo ()
  ((color :accessor color :initarg :color)))

(defclass state ()
  ((width :accessor w :initarg :w :type integer)
   (height :accessor h :initarg :h :type integer)
   (array :accessor arr :initarg :arr)
   (resolved :accessor res :initform t)))

(defclass board ()
  ((width :accessor w :initform 6
          :initarg :w :type integer)
   (height :accessor h :initform 12
           :initarg :h :type integer)
   (states :accessor states :initform '()
           :initarg :states)))

(defun make-puyo (color)
  (make-instance 'puyo :color color))

(defun make-state (h w)
  (make-instance 'state :h h :w w :arr (make-array (list h w) :initial-element nil)))

(defun make-board ()
  (make-instance 'board))

(defvar state (make-array '(12 6) :element-type 'puyo :initial-element nil))

;;; Puyo Ops
(defgeneric is-trash (puyo))

(defmethod is-trash ((p puyo))
  (eq (col p) 'trash))

;;; State Ops
(defgeneric get-dimensions (object)) ;; board or state dimensions

(defmethod get-dimensions ((s state))
  (array-dimensions (arr s)))

(defgeneric get-puyo (s x y)) ;; gets a puyo at an x,y for a board/state?

(defmethod get-puyo ((s state) (x integer) (y integer))
  (aref (arr s) x y))

(defun set-puyo (s p x y)
  (setf (aref (arr s) x y) p))

(defgeneric is-resolved (state)) ;; checks for outstanding chains in a state

;;; Board Ops
(defmethod get-dimensions ((b board))
  (list (h b) (w b)))

(defun state-count (board)
  (length (states board))) ;; amount of states in a board

(defun push-state (state board)
  (when (not (equal (get-dimensions state)
                    (list (h board) (w board))))
    (error "Board and state dimension mismatch."))
  (push state (states board)))

(defun print-state (b index)
  (let  ((state (nth index (states b))))
    (dotimes (y (1- (h state)))
      (dotimes (x (1- (w state)))
        (let ((p (aref (arr state) y x)))
          (cond ((null p) (format t " . "))
                ((eq (color p) 'purple) (format t " p "))
                ((eq (color p) 'red) (format t " r "))
                ((eq (color p) 'green) (format t " g "))
                ((eq (color p) 'blue) (format t " b "))
                ((eq (color p) 'yellow) (format t " y "))
                ((eq (color p) 'trash) (format t " x ")))))
      (format t "~%"))))
    
(defgeneric add-state ()) ;; push a state to the board state stack

(defgeneric project-states ()) ;; give next puyo drop and project resultant states

(defun resolvedp ()
  "Check if board is resolved: no ")

(defun settledp (state)
  "Check that no pieces have empty space below them."
  (destructuring-bind (h w) (array-dimensions (arr state))
    (loop for y from 1 below h do
      (loop for x from 0 below w do
           (when (and (not (null (aref (arr state) y x)))
                      (null (aref (arr state) (1- y) x)))
             (return-from settledp nil)))))
  t)
