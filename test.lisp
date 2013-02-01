;;;; Last modified : 2013-02-01 20:57:50 tkych

;; cl-mod-prime/test.lisp


;;====================================================================
;; Test for CL-MOD-PRIME
;;====================================================================

(in-package :cl-user)
(defpackage #:test.cl-mod-prime
  (:use :cl :cl-mod-prime))
(in-package #:test.cl-mod-prime)


(defun prime-p-and-primes-below-test (n)
  (loop :for p :in (primes-below n)
        :always (prime-p p)))

(defun phi-factorize-test (n)
  (= (phi n)
     (loop :for (p . e) :in (factorize n :group? t)
           :collect (* (expt p (1- e)) (1- p)) :into acc
           :finally (return (reduce #'* acc)))))


;; (phi-factorize-test 100000000) => NIL ; :rho-method fail


(defun randoms-below (limit n)
  (loop :repeat n :collect (random limit)))
 
(defun compare-functions-arg1 (f1 f2 repeat limit
                               &optional output-diff?)
  (let* ((randoms (randoms-below limit repeat))
         (result1 (time (mapcar f1 randoms)))
         (result2 (time (mapcar f2 randoms))))
    (or (equal result1 result2)
        (and output-diff?
             (loop
                :for z :in randoms
                :for x :in result1
                :for y :in result2
                :unless (equal x y) :collect (list z x y))))))

(defun compare-div-rho (n limit)
  (compare-functions-arg1
   (lambda (n)                            ;rho
     (let ((*factorize-swiching-limit* 0))
       (factorize n)))
   (lambda (n)                            ;div
     (let ((*factorize-swiching-limit* most-positive-fixnum))
       (factorize n)))
   n limit nil))


;; Since rho maybe fail, though less effcient, default value of
;; *factorize-swiching-limit* is 10000000.

;; CL-USER> (compare-div-rho 100000 10000000)
;; Evaluation took:
;;   0.749 seconds of real time
;;   0.744046 seconds of total run time (0.740046 user, 0.004000 system)
;;   99.33% CPU
;;   1,642,890,764 processor cycles
;;   7,536,640 bytes consed
  
;; Evaluation took:
;;   1.201 seconds of real time
;;   1.200075 seconds of total run time (1.200075 user, 0.000000 system)
;;   99.92% CPU
;;   2,634,765,636 processor cycles
;;   7,667,984 bytes consed



(defun compare-td-mr (n limit)
  (compare-functions-arg1
   (lambda (n)                            ;trial-division
     (let ((*prime-p-swiching-limit* most-positive-fixnum))
       (prime-p n)))
   (lambda (n)                            ;miller-rabin
     (let ((*prime-p-swiching-limit* 0))
       (prime-p n)))
   n limit))

;; (compare-td-mr 100000 400000000)
;; (compare-td-mr 100000 500000000) ; <- *prime-p-swiching-limit*
;; (compare-td-mr 100000 600000000)
;; (compare-td-mr 100000 700000000)


(defun compare-functions-arg2 (f1 f2 repeat limit)
  (let* ((randoms1 (randoms-below limit repeat))
         (randoms2 (randoms-below limit repeat))
         (result1 (time (mapcar f1 randoms1 randoms2)))
         (result2 (time (mapcar f2 randoms1 randoms2))))
    (or (equal result1 result2)
        (loop
           :for z1 :in randoms1 :for z2 :in randoms2
           :for x :in result1
           :for y :in result2
           :unless (equal x y) :collect (list (list z1 z2) x y)))))


;;====================================================================
