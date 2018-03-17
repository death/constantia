;;;; +----------------------------------------------------------------+
;;;; | Constantia                                                     |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia-test
  (:use #:cl #:constantia)
  (:import-from #:parachute
                #:test
                #:plain
                #:output
                #:report-on
                #:result
                #:status
                #:test-result
                #:name
                #:expression
                #:parent-result
                #:parent
                #:define-test
                #:is
                #:true
                #:false
                #:of-type
                #:is-values)
  (:export
   #:run-tests))

(in-package #:constantia-test)

(defclass minimal (plain)
  ())

(defmethod report-on :around ((result result) (report minimal))
  (when (and (typep result 'parent-result)
             (not (eq :unknown (status result))))
    (call-next-method)))

(defun fq-test-name (test)
  (labels ((rec (test)
             (if (parent test)
                 (cons (name test)
                       (rec (parent test)))
                 (list (name test)))))
    (nreverse (rec test))))

(defmethod report-on ((result test-result) (report minimal))
  (format (output report) "~{~A~^ :: ~}"
          (fq-test-name (expression result))))

(defun run-tests ()
  (test :constantia-test :report 'minimal))

(define-test constantia)

(define-test (constantia misc))

(define-test (misc square)
  (is = 4 (square 2))
  (is = 4 (square -2))
  (is = 1/4 (square 1/2)))

(define-test (misc abs-)
  (is = 3 (abs- 8 5))
  (is = 3 (abs- 5 8))
  (is = 3 (abs- -5 -8)))

(define-test (misc singlep)
  (true (singlep '(x)))
  (false (singlep '()))
  (false (singlep '(x x)))
  (false (singlep 42))
  (false (singlep '(x . x))))

(define-test (misc in-range-p)
  (true (in-range-p 4 0 5))
  (true (in-range-p 0 0 5))
  (false (in-range-p 5 0 5))
  (false (in-range-p -1 0 5))
  (false (in-range-p 5.1 0 5)))

(define-test (misc as-keyword)
  (is eql :foo (as-keyword 'foo)))

(define-test (misc gethash/i)
  (let ((ht (make-hash-table)))
    (is eql nil (gethash :foo ht))
    (is eql 42 (gethash/i :foo ht 42))
    (is eql 42 (gethash :foo ht))
    (is eql 42 (gethash/i :foo ht 123))
    (is eql 42 (gethash :foo ht))))

(define-test (misc best-element)
  (is eql 1 (best-element '(1 3 5 4 2)))
  (is eql 1 (best-element #(1 3 5 4 2)))
  (is eql 5 (best-element #(1 3 5 4 2) :better-p #'>))
  (is eql 2 (best-element '(1 3 5 4 2) :start 1))
  (is eql 3 (best-element '(1 3 5 4 2) :start 1 :end 4))
  (is eql 5 (best-element '(1 3 5 4 2) :key #'-))
  (is-values (best-element '(1 3 5 4 2) :key #'-)
             (eql 5) (eql -5)))

(define-test (misc as-list)
  (is eq '#1=(a b) (as-list '#1#))
  (is equal '(a b) (as-list #(a b))))

(define-test (misc as-vector)
  (is eq '#1=#(a b) (as-vector '#1#))
  (is equalp #(a b) (as-vector '(a b))))

(define-test (misc make-octet-vector)
  (of-type (simple-array u8 (4)) (make-octet-vector 4))
  (is equalp #(42 42) (make-octet-vector 2 :initial-element 42)))

(define-test (misc concat-octet-vectors)
  (let ((a (make-octet-vector 1 :initial-element 1))
        (b (make-octet-vector 1 :initial-element 2)))
    (of-type (simple-array u8 (2)) (concat-octet-vectors a b))
    (is equalp #(1 2) (concat-octet-vectors a b))))

(define-test (misc plist-get)
  (is eql nil (plist-get 'foo '()))
  (is eql 'zot (plist-get 'quux '(foo bar quux zot)))
  (is eql 'zot (plist-get "quux" '("foo" bar "quux" zot)))
  (is eql nil (plist-get "QUUX" '("foo" bar "quux" zot)))
  (is eql nil (plist-get 'quux '(foo bar))))

(define-test (misc agetf)
  (is eql nil (agetf '() 'foo))
  (is eql 42 (agetf '() 'foo 42))
  (is eql 'bar (agetf '((foo . bar)) 'foo))
  (let ((alist '()))
    (setf (agetf alist 'foo) 'bar)
    (is equal '((foo . bar)) alist)
    (setf (agetf alist 'foo) 'zot)
    (is equal '((foo . zot)) alist)))

(define-test (misc float-sum)
  (is eql 0.0 (float-sum '()))
  (is eql 1.0 (float-sum '(0.5 0.5)))
  (is eql 1.0d0 (float-sum #(0.5d0 0.5d0)))
  (is eql -0.5d0 (float-sum #(oops 0.5d0 0.5 1/2 -1 oops)
                            :start 1 :end 5 :key #'-)))

(define-test (misc moment)
  (let ((m1 (moment '(1 2 3 4 5) :sample t))
        (m2 (moment '(1 2 3 4 5) :sample nil)))
    (is = 3.0 (moment-mean m1))
    (is = 2.5 (moment-svar m1))
    (is = 3.0 (moment-mean m2))
    (is = 2.0 (moment-svar m2))))

(define-test (misc queue)
  (let ((q (make-queue)))
    (true (queue-empty-p q))
    (true (zerop (queue-size q)))
    (is eql nil (queue-top q))
    (queue-push 'a q)
    (queue-push 'b q)
    (false (queue-empty-p q))
    (is = 2 (queue-size q))
    (is eql 'a (queue-top q))
    (queue-pop q)
    (is = 1 (queue-size q))
    (is eql 'b (queue-top q))
    (queue-clear q)
    (true (queue-empty-p q))
    (true (zerop (queue-size q)))))

(define-test (misc call-every-n)
  (let* ((c1 0)
         (f1 (call-every-n (lambda () (incf c1)) 1))
         (c2 0)
         (f2 (call-every-n (lambda () (incf c2)) 2)))
    (is = 0 c1)
    (is = 0 c2)
    (funcall f1)
    (funcall f2)
    (is = 1 c1)
    (is = 0 c2)
    (funcall f1)
    (funcall f2)
    (is = 2 c1)
    (is = 1 c2)))

(define-test (constantia wrap-string)
  (let ((long "Hello world"))
    (macrolet ((test (expectation width mode)
                 `(is equal ,expectation (wrap-string long ,width ,mode))))
      (test '("Hello") 5 :cut)
      (test '("Hello world") 100 :cut)
      (test '("He...") 5 :ellipsis)
      (test '("Hello world") 100 :ellipsis)
      ;; Semantics for :word and :character wrapping are fuzzy... may
      ;; want to fix them some day.
      (test '("Hell-" "o wo-" "rld") 5 :character)
      (test '("Hello world") 100 :character)
      (test '("Hello" "world") 6 :character)
      (test '("Hello" "world") 5 :word))))

(define-test (constantia print-table)
  (flet ((table (column-specs rows &rest args)
           (with-output-to-string (stream)
             (apply #'print-table column-specs rows :stream stream args))))
    (macrolet ((test (expectation form)
                 `(is equal ,(substitute #\Newline #\^ expectation) ,form)))
      ;; Just a few funny cases...
      (test "+---+---+^| x | y |^+---+---+^| a | b |^+---+---+^"
            (table '("x" "y") '(("a" "b"))))
      (test "+---+---+^| x | y |^+---+---+^| a | b |^+---+---+^"
            (table '("x" "y") '(("a" "b")) :condensed nil))
      (test "+-----+---+^|  x  | y |^+-----+---+^| abc | b |^+-----+---+^"
            (table '(("x" :width (:fixed 3)) ("y" :width (:max 3)))
                   '(("abcd" "b"))))
      (test "+-----+-----+^|  x  |  y  |^+-----+-----+^| ... | bc- |^|     | de  |^+-----+-----+^"
            (table '(("x" :width (:fixed 3) :wrap :ellipsis)
                     ("y" :width (:max 3) :wrap :character))
                   '(("abcd" "bcde"))))
      (test "+----+-----+^| xx |  y  |^+----+-----+^|  a |  b  |^+----+-----+^"
            (table '(("xx" :width (:fixed :heading) :align :right)
                     ("y" :width (:fixed 3) :align :center))
                   '(("a" "b"))))
      (test "+---+---+^| x | y |^+---+---+^| A | B |^| C | D |^+---+---+^"
            (table '("x" "y") '((a b) (c d))))
      (test "+---+---+^| x | y |^+---+---+^| A | B |^+---+---+^| C | D |^+---+---+^"
            (table '("x" "y") '((a b) (c d)) :condensed nil)))))
