;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (UTILS); Base: 10; -*-

;;; This file is QUEUES.LISP
;;; It contains functions to generate and manipulate queues
;;;    and stacks (without consing) and priority queues (with consing).
;;; It also contains code for `remembering' lists of nodes, by pushing
;;;    them on an array, rather than on a list (without consing).
;;;
;;; ** (c) Copyright 1986 Reid G. Simmons.  All rights reserved.
;;; **     Ported from Zetalisp to Common Lisp 1987 Colin L. Campbell,
;;;        Morton Thiokol, Inc.
;;;
;;; The abstractions defined are QUEUE, STACK, PRIORITY-QUEUE and MEMORY.
;;; Each abstraction `X' supports the functions GENERATE-X, CLEAR-X, EMPTY-X?
;;;
;;; QUEUE supports ENQUEUE and DEQUEUE, and the macro WITH-A-NEW-QUEUE which
;;;    recycles the queues.
;;;    It also supports HEAD-ENQUEUE for getting items in a LIFO manner.
;;;
;;; STACK supports PUSH-STACK and POP-STACK, and the macro WITH-A-NEW-STACK
;;;    which recycles the stacks.
;;;
;;; PRIORITY-QUEUE supports PRIORITY-ENQUEUE and PRIORITY-DEQUEUE.
;;;
;;; MEMORY supports REMEMBER and DO-MEMORY (a macro which applys a function to
;;;    each `remembered' item in a last in, first out manner), and the macro
;;;    WITH-A-NEW-MEMORY which recycles the memory structures.

;;; The function (ALL-Xs) returns a list of all the queues, stacks or memories.
;;; The function (CLEAR-ALL-Xs) clears all the queues, stacks or memories which
;;;   were generated, with the following proviso -- if a new queue, etc. is 
;;;   generated with the same name, only the last one will be remembered.

(defpackage :UTILS (:USE :COMMON-LISP))
(in-package :UTILS)
(use-package :UTILS :USER)

(export `(GENERATE-STACK CLEAR-STACK EMPTY-STACK? PUSH-STACK POP-STACK
	  GET-ALL-STACKS CLEAR-ALL-STACKS WITH-A-NEW-STACK STACK IS-STACKED?

	  GENERATE-QUEUE CLEAR-QUEUE EMPTY-QUEUE? ENQUEUE DEQUEUE HEAD-ENQUEUE 
	  GET-ALL-QUEUES CLEAR-ALL-QUEUES WITH-A-NEW-QUEUE QUEUE IS-QUEUED?

	  GENERATE-PRIORITY-QUEUE CLEAR-PRIORITY-QUEUE EMPTY-PRIORITY-QUEUE? 
	  PRIORITY-ENQUEUE PRIORITY-DEQUEUE GET-ALL-PRIORITY-QUEUES 
	  CLEAR-ALL-PRIORITY-QUEUES PRIORITY-QUEUE
	  
	  GENERATE-MEMORY CLEAR-MEMORY EMPTY-MEMORY? REMEMBER DO-MEMORY
	  GET-ALL-MEMORIES CLEAR-ALL-MEMORIES WITH-A-NEW-MEMORY MEMORY 
	  IS-REMEMBERED? MARK-AND-REMEMBER UNMARK-ALL DO-ALL-MARKED-OBJECTS))


;;; Adds (first removing any other object with the same name) a queue, stack, 
;;;   memory etc. to the global list ALL-QUEUES, ALL-STACKS, ALL-MEMORIES, etc.
(defmacro add-carefully-to-object-list (type object all-objects)
  (declare (symbol type) (T object) (list all-objects))
  (let ((name-fn (intern (concatenate 'string (string type) "-NAME") 'UTILS)))
    `(setq ,all-objects
	   (cons ,object
		 (remove (funcall ',name-fn ,object) ,all-objects
			 :KEY (function ,name-fn))))))

(defsubst items-size (items) (array-dimension items 0))

;;;                ________________
;;;                .              .
;;;                .    STACKS    .
;;;                .______________.


(defparameter *ALL-STACKS* NIL "A list of all STACKS created by GENERATE-STACK")

;;; Returns a list of all STACKS created by GENERATE-STACK
(defsubst GET-ALL-STACKS () 
  (declare (function get-all-stacks () list))
  *all-stacks*)

(defstruct (stack (:print-function stack-printer))
  (name NIL :type symbol)
  (items '#() :type (array * (*))) ;; is an array
  (extension-length 0 :type integer))

(define-defstruct-printer stack "[~A stack containing ~A]"
  (stack-name stack) (list-stack-items stack))

;;; Creates a STACK structure
(defun-typed GENERATE-STACK (name &key (length 100) anonymous)
  (declare (function generate-stack (symbol integer boolean-type) stack))
  (let ((stack (make-stack :NAME name
			   :ITEMS (make-array (list length)
					      :FILL-POINTER 0 :ADJUSTABLE T)
			   :EXTENSION-LENGTH length)))
    (unless anonymous (add-carefully-to-object-list STACK stack *all-stacks*))
    stack))

;;; Returns an integer pointing to the next available slot in the stack-items array
(defsubst-settable stack-top (stack) 
  (fill-pointer (stack-items stack)))

;;; Removes all entries from the stack
;;; If "erase" is non-nil, sets all elements of the stack to NIL (to facilitate GC)
(defun-typed CLEAR-STACK (stack &optional erase)
  (declare (function clear-stack (stack boolean-type) done-type))
  (setf (stack-top stack) 0)
  (when erase (let ((items (stack-items stack)))
		(dotimes (i (items-size items)) (setf (aref items i) NIL))))
  :DONE)

;;; Removes the entries of the stacks found in ALL-STACKS
(defun-typed CLEAR-ALL-STACKS ()
  (declare (function clear-all-stacks () NULL))
  (dolist (stack *all-stacks*) (clear-stack stack T)))

;;; Returns T if the stack has no entries
(defun-typed EMPTY-STACK? (stack)
  (declare (function empty-stack? (stack) T))
  (zerop (stack-top stack)))

;;; Pushes an item onto a stack, increasing the stack size if necessary
(defun-typed PUSH-STACK (item stack)
  (declare (function push-stack (T stack) T))
  (vector-push-extend item (stack-items stack)
		      (stack-extension-length stack))
  item)

;;; Returns the last item pushed on the stack, second value is T for successful pop
(defun-typed POP-STACK (stack)
  (declare (function pop-stack (stack) T boolean-type))
  (cond ((empty-stack? stack) (values NIL NIL))
	(T (values (vector-pop (stack-items stack)) T))))

;;; Returns a list of stack elements (last pushed at the head of the list)
(defun-typed list-stack-items (stack)
  (declare (function list-stack-items (stack) list))
  (let ((stack-items (stack-items stack))
	items)
    (dotimes (i (stack-top stack)) (push (aref stack-items i) items))
    items))

(defparameter *stack-of-stacks* (generate-stack 'STACK-OF-STACKS :LENGTH 10
						:ANONYMOUS T)
  "Contains a set of stack structures that can be reused")

;;; Returns a stack to be used locally
;;;  taken either from the pool of reusable stacks, or newly generated
(defun-typed allocate-temporary-stack ()
  (declare (function allocate-temporary-stack () stack))
  (or (pop-stack *stack-of-stacks*)
      (generate-stack (gentemp "TEMP-STACK-") :LENGTH 100)))

;;; Makes stack available for reuse
(defun-typed deallocate-temporary-stack (stack)
  (declare (function deallocate-temporary-stack (stack) list))
  (clear-stack stack)
  (push-stack stack *stack-of-stacks*)
  NIL)

;;;
;;; Retrieve an empty stack, evaluate the body, reclaim the stack.
;;;   STACK-NAME is lexically scoped.
;;;
(defmacro WITH-A-NEW-STACK (stack-name &body body)
  (declare (symbol stack-name) (list body))
  `(let ((,stack-name (allocate-temporary-stack)))
     (unwind-protect ,(single-clause body 'PROGN)
       (deallocate-temporary-stack ,stack-name))))

;;;
;;; Returns T if the item is already on the stack
;;;
(defun IS-STACKED? (item stack)
  (declare (function is-stacked? (T stack) boolean-type))
  (let ((items (stack-items stack)))
    (dotimes (index (stack-top stack))
      (when (eql item (aref items index)) (return T)))))

;;;                ________________
;;;                .              .
;;;                .    QUEUES    .
;;;                .______________.

(defparameter *ALL-QUEUES* NIL "A list of all QUEUES created by GENERATE-QUEUE")

;;; Returns list of all QUEUES created by GENERATE-QUEUE
(defsubst GET-ALL-QUEUES () 
  (declare (function get-all-queues () list))
  *all-queues*)

(defstruct (queue (:print-function queue-printer))
  (name NIL :type symbol)
  (first-item 0 :type integer)
  (last-item 0 :type integer)
  (items '#() :type (array * (*))) ;; is an array
  (length 0 :type integer)
  (extension-length 0 :type integer))

(define-defstruct-printer queue "[~A queue containing ~A]"
  (queue-name queue) (list-queue-items queue))

;;; Creates a QUEUE structure
(defun-typed GENERATE-QUEUE (name &key (length 100) anonymous)
  (declare (function generate-queue (symbol integer boolean-type) queue))
  (let ((queue (make-queue :NAME name :ITEMS (make-array (list length))
			   :LENGTH (1- length) :EXTENSION-LENGTH length)))
    (unless anonymous (add-carefully-to-object-list QUEUE queue *all-queues*))
    queue))

;;; Remove all entries from the queue
;;; If "erase" is non-nil, sets all elements of the queue to NIL (to facilitate GC)
(defun-typed CLEAR-QUEUE (queue &optional erase)
  (declare (function clear-queue (queue boolean-type) done-type))
  (setf (queue-first-item queue) 0)
  (setf (queue-last-item queue) 0)
  (when erase (let ((items (queue-items queue)))
		(dotimes (i (items-size items)) (setf (aref items i) NIL))))
  :DONE)

;;; Removes the entries of the queues found in ALL-QUEUES
(defun-typed CLEAR-ALL-QUEUES ()
  (declare (function clear-all-queues () NULL))
  (dolist (queue *all-queues*) (clear-queue queue T)))

;;; Returns T if the queue has no entries
(defun-typed EMPTY-QUEUE? (queue)
  (declare (function empty-queue? (queue) boolean-type))
  (= (queue-first-item queue) (queue-last-item queue)))

;;; Increments NUMBER or wraps around to zero if NUMBER equals MAX
(defsubst circular-number (number max)
  (declare (function circular-number (integer integer) integer))
  (if (= number max) 0 (1+ number)))

;;; Enqueues item on queue, increasing the size of the queue if necessary
(defun-typed ENQUEUE (item queue)
  (declare (function enqueue (T queue) T))
  (let* ((old-last (queue-last-item queue))
	 (new-last (circular-number old-last (queue-length queue))))
    (when (= new-last (queue-first-item queue))
      (queue-extend queue (queue-extension-length queue))
      (setq new-last (1+ (queue-last-item queue))))
    (setf (aref (queue-items queue) new-last) item)
    (setf (queue-last-item queue) new-last)
    item))

;;; Takes the next element from the queue, second value is T on successful dequeue
(defun-typed DEQUEUE (queue)
  ;;(declare (values item successful-p))
  (declare (function dequeue (queue) T boolean-type))
  (cond ((empty-queue? queue)
	 (values NIL NIL))
	(T (let ((qfirst-item (circular-number (queue-first-item queue)
					       (queue-length queue))))
	     (setf (queue-first-item queue) qfirst-item)
	     (values (aref (queue-items queue) qfirst-item) T)))))

;;; Pushes item at the head of the queue for a LIFO effect;
;;;   increases queue size if needed
(defun-typed HEAD-ENQUEUE (item queue)
  (declare (function head-enqueue (T queue) T))
  (let* ((queue-length (queue-length queue))
	 (new-first (cond ((zerop (queue-first-item queue)) queue-length)
			  (t (1- (queue-first-item queue))))))
    (when (= new-first (queue-last-item queue))
      (queue-extend queue (queue-extension-length queue))
      (setq queue-length (queue-length queue) new-first (queue-length queue)))
    (setf (aref (queue-items queue) (circular-number new-first queue-length)) item)
    (setf (queue-first-item queue) new-first)
    item))

(defparameter *stack-of-queues* (generate-stack 'STACK-OF-QUEUES :LENGTH 10
						:ANONYMOUS T)
  "Contains a set of queue structures that can be reused")

;;; Returns a queue to be used locally, 
;;;  taken either from the pool of reusable queues, or newly generated
(defun-typed allocate-temporary-queue ()
  (declare (function allocate-temporary-queue () queue))
  (or (pop-stack *stack-of-queues*)
      (generate-queue (gentemp "TEMP-QUEUE-") :LENGTH 100)))

;;; Makes queue available for reuse
(defun-typed deallocate-temporary-queue (queue)
  (declare (function deallocate-temporary-queue (queue) list))
  (clear-queue queue)
  (push-stack queue *stack-of-queues*)
  NIL)

;;;
;;; Retrieve an empty queue, evaluate the body, reclaim the queue.
;;;   QUEUE-NAME is lexically scoped.
;;;
(defmacro WITH-A-NEW-QUEUE (queue-name &body body)
  (declare (symbol queue-name) (list body))
  `(let ((,queue-name (allocate-temporary-queue)))
     (unwind-protect ,(single-clause body 'PROGN)
       (deallocate-temporary-queue ,queue-name))))

;;;
;;; Returns T if the item is already in the queue
;;;   (Somewhat hairy code reflects desire for speed).
;;;
(defun IS-QUEUED? (item queue)
  (declare (function is-queued? (T queue) boolean-type))
  (unless (empty-queue? queue)
    (let ((queue-length (queue-length queue))
	  (items (queue-items queue))
	  (first (queue-first-item queue))
	  (last (queue-last-item queue)))
      (cond ((< first last)
	     (do ((index (1+ first) (1+ index)))
		 ((> index last) NIL)
	       (when (eql item (aref items index)) (return T))))
	    (T (or (do ((index 0 (1+ index)))
		       ((> index last) NIL)
	             (when (eql item (aref items index)) (return T)))
		   (do ((index (1+ first) (1+ index)))
		       ((> index queue-length) NIL)
	             (when (eql item (aref items index)) (return T)))))))))

;;; Returns a list of all the items in the queue 
;;;   (first enqueued at the head of the list).
(defun-typed list-queue-items (queue)
  (declare (function list-queue-items (queue) list))
  (let ((queue-items (queue-items queue))
	(queue-length (queue-length queue))
	(last-item (queue-last-item queue))
	result new-cons)
    (do ((i (queue-first-item queue) (circular-number i queue-length))
	 (last-cons-in-result NIL new-cons))
      ((= i last-item) result)
      (setq new-cons (list (aref queue-items (circular-number i queue-length))))
      (if last-cons-in-result
	  (rplacd last-cons-in-result new-cons)
	(setq result new-cons)))))

;;; Increases the length of QUEUE by LENGTH
(defun-typed queue-extend (queue length)
  (declare (function queue-extend (queue integer) NULL))
  (let* ((old-array (queue-items queue))
	 (old-dimension (items-size old-array))
	 (max-number (1- old-dimension))
	 (new-array (make-array (list (+ old-dimension length)))))
    (setf (queue-length queue) (1- (items-size new-array)))
    (do ((i 1 (1+ i))
	 (j (circular-number (queue-first-item queue) max-number)
	    (circular-number j max-number)))
      ((= i old-dimension)
       (setf (queue-items queue) new-array)
       (setf (queue-first-item queue) 0)
       (setf (queue-last-item queue) max-number))
      (setf (aref new-array i) (aref old-array j)))))

;;;                ___________________
;;;                .                 .
;;;                . PRIORITY-QUEUES .
;;;                ._________________.

;;; INEFFICIENT IMPLEMENTATION OF PRIORITY-QUEUES
;;; Has the property that if two values are EQUAL, then the newest entry
;;;   gets inserted ahead of the older entries (it's really like a PRIORITY-STACK 
;;;   in that way!!!)

(defparameter *ALL-PRIORITY-QUEUES* NIL
  "A list of all priority-queues created by GENERATE-PRIORITY-QUEUE")

;;; A list of all priority-queues created by GENERATE-PRIORITY-QUEUE
(defsubst GET-ALL-PRIORITY-QUEUES () 
  (declare (function get-all-priority-queues () list))
  *all-priority-queues*)

(defstruct (priority-queue (:print-function priority-queue-printer))
  (name NIL :type symbol)
  (sorted-list NIL :type list)
  (value-lessp-fn NIL :type function))

(define-defstruct-printer priority-queue "[~A priority-queue containing ~A]"
  (priority-queue-name priority-queue)
  (list-priority-queue-items priority-queue))

;;; Creates a PRIORITY-QUEUE structure
;;;   VALUE-LESSP-FN will be used to determine the order of dequeueing
(defun-typed GENERATE-PRIORITY-QUEUE (name value-lessp-fn)
  (declare (function generate-priority-queue (symbol function) priority-queue))
  (let ((priority-queue (make-priority-queue :NAME name 
					     :VALUE-LESSP-FN value-lessp-fn)))
    (add-carefully-to-object-list priority-queue priority-queue
				  *all-priority-queues*)
    priority-queue))

;;; Removes all items from the queue
(defun-typed CLEAR-PRIORITY-QUEUE (priority-queue)
  (declare (function clear-priority-queue (priority-queue) done-type))
  (setf (priority-queue-sorted-list priority-queue) NIL)
  :DONE)

;;; Removes all items from all priority queues
(defun-typed CLEAR-ALL-PRIORITY-QUEUES ()
  (declare (function clear-all-priority-queues () NULL))
  (dolist (priority-queue *all-priority-queues*)
    (clear-priority-queue priority-queue)))

;;; Returns T if the priority queue is empty
(defun-typed EMPTY-PRIORITY-QUEUE? (priority-queue)
  (declare (function empty-priority-queue (priority-queue) boolean-type))
  (null (priority-queue-sorted-list priority-queue)))

;;; Enqueues an item in a priority-queue;
;;;    increases the size of the queue if necessary
(defun-typed PRIORITY-ENQUEUE (item priority-queue)
  (declare (function priority-enqueue (T priority-queue) done-type))
  (let ((value-lessp-fn (priority-queue-value-lessp-fn priority-queue)))
    (do ((previous-list NIL current-list)
	 (current-list (priority-queue-sorted-list priority-queue)
		       (cdr current-list)))
      ((or (null current-list)
	   (not (funcall value-lessp-fn (car current-list) item)))
       (cond (previous-list (rplacd previous-list (cons item current-list)))
	     (T (setf (priority-queue-sorted-list priority-queue)
		      (cons item current-list)))))))
  :DONE)

;;; Gets the next highest priority item from the queue.
;;; Second value is T if successful.
(defun-typed PRIORITY-DEQUEUE (priority-queue)
  ;;(declare (values item successful-p))
  (declare (function priority-dequeue (priority-queue) T boolean-type))
  (let ((current-list (priority-queue-sorted-list priority-queue)))
    (cond (current-list
	   (setf (priority-queue-sorted-list priority-queue) (cdr current-list))
	   (values (car current-list) T))
	  (T (values NIL NIL)))))

;;; List the items in the priority queue ranked in priority order
(defun-typed list-priority-queue-items (priority-queue)
  (declare (function list-priority-queue-items (priority-queue) list))
  (priority-queue-sorted-list priority-queue))


;;;                ________________
;;;                .              .
;;;                .    MEMORY    .
;;;                .______________.

;;; `Memory' is a way of keeping track of a list of things without consing.

(defparameter *ALL-MEMORIES* NIL
  "A list of all MEMORY objects created by GENERATE-MEMORY")

;;; Returns list of all MEMORY objects created by GENERATE-MEMORY
(defsubst GET-ALL-MEMORIES () 
  (declare (function get-all-memories () list))
  *all-memories*)

(defstruct (memory (:print-function memory-printer))
  (name NIL :type symbol)
  (items '#() :type (array * (*)))
  (extension-length 0 :type integer))

(define-defstruct-printer memory "[~A memory containing ~d items]"
  (memory-name memory) (fill-pointer (memory-items memory)))

;;; Creates a MEMORY structure
(defun-typed GENERATE-MEMORY (name &key (length 100) anonymous)
  (declare (function generate-memory (symbol integer boolean-type) memory))
  (let ((memory (make-memory :NAME name 
			     :ITEMS (make-array (list length)
						:FILL-POINTER 0 :ADJUSTABLE T)
			     :EXTENSION-LENGTH length)))
    (unless anonymous 
      (add-carefully-to-object-list MEMORY memory *all-memories*))
    memory))

;;; Removes all items from a MEMORY.
;;; If "erase" is non-nil, sets all elements of the memory to NIL (to facilitate GC)
(defun-typed CLEAR-MEMORY (memory &optional erase)
  (declare (function clear-memory (memory boolean-type) done-type))
  (setf (fill-pointer (memory-items memory)) 0)
  (when erase (let ((items (memory-items memory)))
		(dotimes (i (items-size items)) (setf (aref items i) NIL))))
  :DONE)

;;; Removes all elements from all MEMORY objects.  MEMORY objects still exist.
(defun-typed CLEAR-ALL-MEMORIES ()
  (declare (function clear-all-memory (memory) NULL))
  (dolist (memory *all-memories*) (clear-memory memory T)))

;;; Returns T if no objects are contained in MEMORY
(defun-typed EMPTY-MEMORY? (memory)
  (declare (function empty-memory? (memory) boolean-type))
  (zerop (fill-pointer (memory-items memory))))

;;; Pushes ENTITY onto MEMORY increasing the memory size if necessary
(defun-typed REMEMBER (entity memory)
  ;;(declare (values entity))
  (declare (function remember (T memory) T))
  (vector-push-extend entity (memory-items memory)
		      (memory-extension-length memory))
  entity)

(defparameter *stack-of-memories* (generate-stack 'STACK-OF-MEMORIES :LENGTH 10
						  :ANONYMOUS T)
  "Contains a set of memory structures that can be reused")

;;; Returns a memory to be used locally
;;;  taken either from the pool of reusable memories, or newly generated
(defun-typed allocate-temporary-memory ()
  (declare (function allocate-temporary-memory () memory))
  (or (pop-stack *stack-of-memories*)
      (generate-memory (gentemp "TEMP-MEMORY-") :LENGTH 100)))

;;; Makes memory available for reuse
(defun-typed deallocate-temporary-memory (memory)
  (declare (function deallocate-temporary-memory (memory) list))
  (clear-memory memory)
  (push-stack memory *stack-of-memories*)
  NIL)

;;;
;;; Retrieve an empty memory, evaluate the body, reclaim the memory.
;;;   MEMORY-NAME is lexically scoped.
;;;
(defmacro WITH-A-NEW-MEMORY (memory-name &body body)
  (declare (symbol memory-name) (list body))
  `(let ((,memory-name (allocate-temporary-memory)))
     (unwind-protect ,(single-clause body 'PROGN)
       (deallocate-temporary-memory ,memory-name))))

;;; Execute the body, iterating over the items in memory in a last in, 
;;;   first out manner.
(defmacro DO-MEMORY ((entity memory) &body body)
  (declare (T entity) (type memory memory) (list body))
  (let ((items (gensym)) (index (gensym)))
    `(let ((,items (memory-items ,memory)))
       (do ((,index (1- (fill-pointer ,items)) (1- ,index))
	    ,entity)
	 ((minusp ,index))
	 (setq ,entity (aref ,items ,index))
	 ,@body))))

;;;
;;; Returns T if the item is already on the stack
;;;
(defun IS-REMEMBERED? (item memory)
  (declare (function is-remembered? (T memory) boolean-type))
  (let ((items (memory-items memory)))
    (dotimes (index (fill-pointer items))
      (when (eql item (aref items index)) (return T)))))

;;;
;;; HANDLING MARKED OBJECTS (USING `MEMORY')
;;;

(defparameter *marked-objects* (generate-memory '*marked-objects* :LENGTH 200))

(defun-typed mark-and-remember (obj the-mark &optional (memory *marked-objects*))
  (declare (function mark-and-remember (internals T memory) internals))
  (setf (mark obj) the-mark)
  (remember obj memory))

(defun-typed unmark-all (&optional (memory *marked-objects*))
  (declare (function unmark-all (memory) done-type))
  (do-memory (object memory) (unmark-object object))
  (clear-memory memory))

(defun-typed do-all-marked-objects (fcn &optional (memory *marked-objects*))
  (declare (function do-all-mark-objects (function memory) NULL))
  (do-memory (object memory) (funcall fcn object)))