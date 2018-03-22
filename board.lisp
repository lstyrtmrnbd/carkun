;;;; Puyopuyo game states representation

;;; Model

;; puyo: single color blob
;; puyos: up to 2D array of puyo; (0 0) bottom left, (rows columns)
;; puyopuyo: puyo blob, simple : the puyo array could be one generated by the game

(defparameter *colors* '(red blue purple yellow green trash))

(defclass puyo ()
  ((color :accessor color :initarg :color)))

(defclass puyopuyo ()
  ((puyos :accessor puyos :initarg :puyos)
   (simple :accessor simple)))

;; state
;; puyos : 2D array of puyos or nil if empty; (0 0) bottom left, (rows columns)
;; next : a puyopuyo blob to be dropped next
;; resolved : are there outstanding combinations on the board?

(defclass state ()
  ((puyos :accessor puyos :initarg :puyos)
   (next-puyopuyo :accessor next)
   (resolved :accessor res :initform t)))

(defclass board ()
  ((width :accessor w :initform 6
          :initarg :w :type integer)
   (height :accessor h :initform 12
           :initarg :h :type integer)
   (states :accessor states :initform '()
           :initarg :states)))

;;; Constructors

(defun make-puyo (color)
  (make-instance 'puyo :color color))

;; maybe add switch to fill up along i or out along j
(defun make-simple-puyos (puyo-list)
  "Makes an array large enough to contain the listed puyos and fills it."
  (let* ((near (nearest-square (length puyo-list)))
         (puyos (make-array (list near near) :initial-element nil)))
    (loop for i from 0 below near do
         (loop for j from 0 below near do
              (setf (aref puyos i j) (nth (+ (* i near) j) puyo-list))))
    puyos))

(defun nearest-square (limit)
  "Returns the side length of a square large enough to encompass limit."
  (let ((i 1))
    (loop while (< (* i i) limit) do
         (incf i))
    i))

(defun make-double-puyos (puyo)
  (make-array 2 :initial-element puyo))

(defun make-puyopuyo (puyos)
  (make-instance 'puyopuyo :puyos puyos))

(defun make-state (h w)
  (make-instance 'state :puyos (make-array (list h w) :initial-element nil)))

(defun make-board ()
  (make-instance 'board))

(defun make-board-with-state (states)
  (make-instance 'board :states states))

;;; Puyo Ops

(defun is-trash (puyo)
  (eq (color puyo) 'trash))

(defun get-colors (puyos)
  (let ((colors '()))
    (destructuring-bind (h w) (array-dimensions puyos)
      (loop for y from 0 below h do
           (loop for x from 0 below w do
                (when (aref puyos y x)
                  (push (color (aref puyos y x)) colors)))))
    colors))

;; probably no longer necessary
(defun hash-puyos (puyos hash-table)
  "Fills a hash table with the existing puyos using their array indices as key."
  (destructuring-bind (h w) (array-dimensions puyos)
    (loop for y from 0 below h do
         (loop for x from 0 below w do
              (setf (gethash (list x y) hash-table) (aref puyos y x)))))
  (maphash #'(lambda (key value)
               (when (null value)
                 (remhash key hash-table)))
           hash-table))

(defun check-combo (puyos)
  "Is there a combo in puyos array"
  (destructuring-bind (h w) (array-dimensions puyos)
    (loop for y from 0 below h do
         (loop for x from 0 below w do
              (when (aref puyos y x)
                (when (< 3 (length (flood-fill y x puyos)))
                  (return-from check-combo t)))))
    nil))

(defstruct (queue)
  (elements))

(defun push-queue (elt queue)
  (push elt (queue-elements queue)))

(defun pop-queue (queue)
  (let ((popped (car (last (queue-elements queue)))))
    (setf (queue-elements queue) (butlast (queue-elements queue)))
    popped))

(defun is-empty-queue (queue)
  (= 0 (length (queue-elements queue))))

(defun flood-fill (y x puyos)
  "Flood fill variant returns list of coords of same color puyos connected to the puyo at (y x)."
  (let ((combo nil)
        (rep-color (color (aref puyos y x)))
        (Q (make-queue :elements nil))
        (currefs nil))
    (push (list y x) combo)
    (push-queue (list y x) Q)
    (loop while (not (is-empty-queue Q)) do
         (setf currefs (pop-queue Q))
         (destructuring-bind (u v) currefs
           (let ((north (list (1+ u) v))
                 (south (list (1- u) v))
                 (east  (list u (1+ v)))
                 (west  (list u (1- v))))
             (when (is-valid-flood-node north rep-color puyos combo)
               (push-queue north Q)
               (push north combo))
             (when (is-valid-flood-node south rep-color puyos combo)
               (push-queue south Q)
               (push south combo))
             (when (is-valid-flood-node east rep-color puyos combo)
               (push-queue east Q)
               (push east combo))
             (when (is-valid-flood-node west rep-color puyos combo)
               (push-queue west Q)
               (push west combo)))))
    combo))

(defun is-valid-flood-node (refs rep-color puyos combo)
  (and (in-bounds refs puyos)
       (arefs refs puyos)
       (eq rep-color (color (arefs refs puyos)))
       (not (refs-in-list refs combo))))

(defun refs-in-list (refs list)
  (some #'(lambda (val)
            (equal val refs))
        list))

(defun arefs (refs array)
  (destructuring-bind (y x) refs
    (aref array y x)))

(defun in-bounds (refs array)
  (destructuring-bind (h w) (array-dimensions array)
    (destructuring-bind (y x) refs
      (and (<= 0 y)
           (>= (1- h) y)
           (<= 0 x)
           (>= (1- w) x)))))

;; shifts puyos in their array once to simulate rotation
(defun rotate (puyos &optional (clockwise nil)) nil)

;;; State Ops

(defgeneric get-dimensions (object)) ;; board or state dimensions

(defmethod get-dimensions ((s state))
  (array-dimensions (puyos s)))

(defun get-puyo (state x y)
  (aref (puyos state) x y))

(defun set-puyo (state puyo x y)
  (setf (aref (puyos state) x y) puyo))

(defun set-puyos (state puyos x y) nil)

(defun resolvedp ()
  "Check if state is resolved: no outstanding chains.")

;;; Board Ops

(defmethod get-dimensions ((b board))
  (list (h b) (w b)))

(defun state-count (board)
  (length (states board))) ;; amount of states in a board

(defun push-state (state board)
  (when (not (equal (get-dimensions state)
                    (get-dimensions board)))
    (error "Board and state dimension mismatch."))
  (push state (states board)))

(defun print-state (b index)
  (let  ((state (nth index (states b))))
    (destructuring-bind (h w) (array-dimensions (puyos state))
      (loop for y from (1- h) downto 0 do
           (loop for x from 0 below w do
                (let ((p (aref (puyos state) y x)))
                  (cond ((null p) (format t " . "))
                        ((eq (color p) 'purple) (format t " p "))
                        ((eq (color p) 'red) (format t " r "))
                        ((eq (color p) 'green) (format t " g "))
                        ((eq (color p) 'blue) (format t " b "))
                        ((eq (color p) 'yellow) (format t " y "))
                        ((eq (color p) 'trash) (format t " x ")))))
           (format t "~%")))))

(defun stablep (puyos)
  (and (settledp puyos)
       (not (combo-check puyos))))

(defun settledp (puyos)
  "Check that no pieces have empty space below them."
  (not (unsettled puyos)))

(defun unsettled (puyos)
  "Returns the coords of the first unsettled point."
  (destructuring-bind (h w) (array-dimensions puyos)
    (loop for y from 1 below h do
         (loop for x from 0 below w do
              (when (and (aref puyos y x)
                         (null (aref puyos (1- y) x)))
                (return-from unsettled (list (1- y) x)))))
    nil))

(defun settle-col (refs puyos)
  "Shifts puyos above refs point down."
  (destructuring-bind (ry rx) refs
    (loop for y from 0 below (- (first (array-dimensions puyos)) (1+ ry)) do
         (setf (aref puyos (+ ry y) rx)
               (aref puyos (+ ry (1+ y)) rx)))
    (setf (aref puyos (1- (first (array-dimensions puyos))) rx) ; special case for top of column
          nil)))

(defun settle (puyos)
  "Settles all puyos in puyos."
  (let ((uns (unsettled puyos)))
    (loop while uns do
         (settle-col uns puyos)
         (setf uns (unsettled puyos)))))

(defun project-states () nil) ;; give next puyo drop and project resultant states

;;; Testing

(defvar *tmpboard* (make-board))
(defvar *state* (make-array '(12 6) :element-type 'puyo :initial-element nil))
(defvar *2gpuyo* (make-simple-puyos (list (make-puyo 'green) (make-puyo 'green))))
(defvar *unsettled* (make-array '(2 2) :initial-element nil))

(defun init-test ()
  (setf (aref *unsettled* 1 0) (make-puyo 'blue))
  (push-stat *state* *tmpboard*))
