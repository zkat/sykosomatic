(defpackage :smug
  (:use :cl)
  (:export
   #:bind
   #:result
   #:parse
   #:item
   #:fail
   #:plus

   #:=let*
   #:*case-sensitive-p*
   #:=char
   #:=string
   #:=and
   #:=or
   #:=not
   #:=satisfies
   #:=prog1
   #:=prog2

   #:before
   #:natural-number
   #:zero-or-more
   #:one-or-more
   #:cardinal
   #:ordinal
   #:one-of
   #:no-more-input

   #:text
   #:whitespace
   #:skip-whitespace
   #:none-of
   #:maybe
   #:either
   #:word
   #:alpha-char

   ))

(in-package :smug)

(defpackage :smug.examples
  (:use :cl :smug))

(defgeneric input-empty-p (input)
  (:method ((input cl:string))
    (zerop (length input))))

(defgeneric input-first (input)
  (:method ((input cl:string))
    (declare (optimize (speed 3) (safety 0)))
    (aref input 0)))

(defgeneric input-rest (input)
  (:method ((input cl:string))
    (declare (optimize (speed 3)))
    (multiple-value-bind (string index)
        (array-displacement input)
      (let ((original-string (or string input)))
        (make-array (1- (length input))
                    :displaced-to original-string
                    :displaced-index-offset (1+ index)
                    :element-type 'character)))))

(defstruct (string-input (:constructor %make-string-input))
  (position 0) (string "" :type string))

(defun make-string-input (string)
  (%make-string-input :string string))

(defmethod input-empty-p ((input string-input))
  (= (string-input-position input)
     (1- (length (string-input-string input)))))

(defmethod input-first ((input string-input))
  (aref (string-input-string input)
        (string-input-position input)))

(defmethod input-rest  ((input string-input))
  (%make-string-input :position (1+ (string-input-position input))
                      :string (string-input-string input)))

(declaim (inline bind item result fail =or))

(defun result (value)
  (declare (optimize (speed 3)))
  (lambda (input)
    (list (cons value input))))

(defun fail (&key (error nil))
  (if error
      (lambda (input)
        (declare (ignore input))
        (error error))
      (constantly nil)))

(defun item ()
  (lambda (input)
    (unless (input-empty-p input)
      (list (cons (input-first input)
                  (input-rest input))))))

(defun bind (parser function)
  (lambda (input)
    (loop :for (value . input) :in (funcall parser input)
       :append (funcall (funcall function value) input))))

(defun =satisfies (predicate)
  (bind (item)
        (lambda (x)
          (if (funcall predicate x)
              (result x)
              (fail)))))

(defun plus (&rest parsers)
  (lambda (input)
    (loop :for parser in parsers
       :append (funcall parser input))))

;;;; PARSER-LET* is the natural syntax for lispers
(defmacro =let* (bindings &body body)
  (if bindings
      (let ((symbol (first (first bindings))))
        `(bind ,@(cdr (first bindings))
               (lambda (,symbol)
                 ,@(when (string-equal (symbol-name symbol) "_")
                         `((declare (ignorable ,symbol))))
                 (=let* ,(cdr bindings)
                   ,@body))))
      `(progn ,@body)))

(defun no-more-input ()
  (lambda (input)
    (when (input-empty-p input)
      (list (cons nil input)))))


(defun =or (&rest parsers)
  (declare (optimize (speed 3)))
  (labels ((non-consing-or (parsers)
             (lambda (input)
               (or (funcall (the function (first parsers)) input)
                   (when (rest parsers)
                     (funcall (the function (non-consing-or (rest parsers))) input))))))
    (non-consing-or parsers)))


(defun =not (parser)
  (lambda (input)
    (let ((result (funcall parser input)))
      (if result
          nil
          (list (cons t input))))))

(defun =and (p1 &rest ps)
  (=let* ((result p1))
    (if ps
        (apply #'=and ps)
        (result result))))

(defvar *case-sensitive-p* nil)
(defun =char (x)
  (=satisfies (lambda (y)
                (if *case-sensitive-p*
                    (eql x y)
                    (equalp x y)))))

(defun =prog1 (parser &rest parsers)
  (=let* ((result parser)
          (_ (apply #'=and parsers)))
    (result result)))

(defun =prog2 (parser1 result-parser &rest more-parsers)
  (=and parser1 (apply #'=prog1 result-parser more-parsers)))

(defun before (parser end-parser)
  (=let* ((i parser)
          (result (lambda (input)
                    (if (funcall end-parser input)
                        (list (cons i input))
                        nil))))
    (result result)))

(defun =string (string)
  (if (input-empty-p string)
      (result "")
      (=let*
          ((_ (=char (input-first string)))
           (_ (=string (input-rest string))))
        (result string))))

(defun digit ()
  (=satisfies #'digit-char-p))

(defun natural-number ()
  (labels ((evaluate (chars)
             (reduce #'op (mapcar #'digit-char-p chars)))
           (op (m n)
             (+ (* 10 m) n)))
    (=let* ((xs (one-or-more (digit))))
      (result (evaluate xs)))))

(defun sophisticated-int ()
  (flet ((op ()
           (plus (=let* ((_ (=char #\-)))
                   (result #'-))
                 (result #'identity))))
    (=let* ((op (op))
            (n (natural-number)))
      (result (funcall op n)))))

(defun int ()
  (sophisticated-int))

(defun bracket (open-parser body-parser close-parser)
  (=let* ((_ open-parser)
          (x body-parser)
          (_ close-parser))
    (result x)))

(defun alphanumeric ()
  (=satisfies #'alphanumericp))

;; cardinal = 1, 2, three, four...
(defun cardinal ()
  (=let* ((card (=or (=let* ((num (natural-number))
                             (_ (one-or-more (whitespace))))
                       (result num))
                     (text (alphanumeric)))))
    (if (numberp card)
        (result card)
        (let ((position (position card (loop for i from 1 below 99
                                          collect (format nil "~r" i)) :test #'string-equal)))
          (if position
              (result (1+ position))
              (fail))))))

;; ordinal = 1st, 2nd, third, fourth....
(defun ordinal ()
  ;; TODO - 1st, 2nd, ...
  (=let* ((ord (text (alphanumeric))))
    (let ((position (position ord (loop for i from 1 upto 20
                                     collect (format nil "~:r" i)) :test #'string-equal)))
      (if position
          (result (1+ position))
          (fail)))))

(defun none-of (char-bag)
  (=let* ((char (item)))
    (if (not (find char char-bag))
        (result char)
        (fail))))

(defun one-of (char-bag)
  (=let* ((char (item)))
    (if (find char char-bag)
        (result char)
        (fail))))

(defun text (&optional (parser (item)))
  (=let* ((text (one-or-more parser)))
    (result (coerce text 'cl:string))))

(defun eof (&optional (result :eof))
  (bind (no-more-input)
        (lambda (_) (declare (ignore _ ))
                (result result))))

(defun zero-or-more (parser &optional (combinator #'=or))
  (declare (optimize (speed 3)))
  (funcall (the function combinator)
           (=let* ((x (the function parser))
                   (y (=or (zero-or-more parser combinator)
                           (result nil))))
             (result (cons x y)))
           (result nil)))

(defun one-or-more (parser)
  (=let* ((x parser)
          (y (zero-or-more parser)))
    (result (cons x y))))

(defun at-least (n parser)
  (=let* ((x parser)
          (_ (if (eql 1 n)
                 (result t)
                 (at-least (1- n) parser))))
    (result (make-list n :initial-element x))))

(defun line ()
  (=or
   (=let* ((xs (text (none-of '(#\Newline))))
           (end (=or (no-more-input)
                     (=char #\Newline))))

     (result
      (list* :line xs (list end))))
   (bind (=char #\Newline)
         (constantly
          (result '(:line "" :terminator #\Newline))))))

(defun whitespace ()
  (one-of '(#\Tab #\Newline #\Space)))

(defun skip-whitespace (parser)
  (=let* ((_ (zero-or-more (whitespace)))
          (v parser)
          (_ (zero-or-more (whitespace))))
    (result v)))

(defun either (parser &rest condition-types)
  (lambda (input)
    (block parse
     (handler-bind ((condition (lambda (c)
                                 (when (find-if (lambda (type)
                                                  (typep c type))
                                                condition-types)
                                   (return-from parse
                                    (list (cons c input)))))))
       (funcall parser input)))))

(defun maybe (parser &rest condition-types)
  (lambda (input)
    (block parse
      (handler-bind ((condition (lambda (c)
                                  (when (find-if (lambda (type)
                                                   (typep c type))
                                                 condition-types)
                                    (return-from parse
                                      (list (cons nil input)))))))
        (or (funcall parser input)
            (list (cons nil input)))))))

(defun word (&optional (test #'identity))
  (=let* ((word (skip-whitespace (text (alpha-char)))))
    (if (funcall test word)
        (result word)
        (fail))))

(defun alpha-char ()
  (=satisfies #'alpha-char-p))

(defun org-block (&optional (level 0))
  (=or (section level)
       (simple-list)
       (text-block (line))))

(defun text-block (parser)
  (=and (=not (section-heading))
        parser))

(defun section-heading (&optional (level 0))
  (=let* ((indicator (at-least (1+ level)
                               (=char #\*)))
          (space (one-or-more (=char #\Space)))
          (name (line)))
    (result (list :level (length indicator)
                  :indicator (cons indicator space)
                  :name name))))

(defun section (&optional (level 0))
  (=let* ((heading (section-heading level))
          (contents (zero-or-more
                     (org-block (1+ level)))))
    (result (list :section :heading heading :contents contents))))

(defun section-line (&optional (level 0))
  (=and (=not (section-heading level))
        (line)))

(defun list-item-content-line (indentation-level)
  (=let* ((indentation (at-least indentation-level (whitespace)))
          (line (line)))
    (result (cons indentation line))))

(defun list-item ()
  (=let* ((pre-space (zero-or-more (whitespace)))
          (indicator (one-of "*+-"))
          (post-space (one-or-more (whitespace)))
          (first-line (line))
          (rest-lines (zero-or-more
                       (list-item-content-line
                        (+ 1 (length pre-space)
                           (length post-space))))))
    (result (list :list-item
                  :indicator (list pre-space
                                   indicator
                                   post-space)
                  :content (cons (cons nil first-line)
                                 rest-lines)))))

(defun simple-list ()
  (=let* ((list (text-block (one-or-more (list-item)))))
    (result (cons :unordered-list list))))
