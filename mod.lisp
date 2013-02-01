;;;; Last modified : 2013-02-01 20:57:27 tkych

;; cl-mod-prime/mod.lisp


;;====================================================================
;; Modular Arithmetic Library for Common Lisp
;;====================================================================
;; mod-mult
;; mod-expt
;; mod-inv
;; mod-sqrt      <- not yet
;; mod-nth-root  <- not yet
;; dlog          <- not yet

;;--------------------------------------------------------------------
;; abbrev TAOCP2 as 
;;  D. Knuth. The Art of Computer Programming, vol.2:
;;            Seminumerical Algorithms. 3rd ed., Addison-Wesley, 1997.

;; abbrev CINTA as
;;  V. Shoup. A Computational Introduction to Number Theory and Algebra.
;;            2nd ed., Cambridge University Press, 2008.
;;            available at http://shoup.net/ntb/ntb-v2.pdf
;;--------------------------------------------------------------------


(in-package #:cl-mod-prime)

;;--------------------------------------------------------------------
;; MOD-MULT &rest integers-divisor => integer
;;--------------------------------------------------------------------
(defun mod-mult (&rest integers-divisor)
  "Return multiple of INTEGERS modulo DIVISOR.
"
  (if (<= (length integers-divisor) 1)
      (error "There must be at least 2 arguments.")
      (let ((divisor (first (last integers-divisor))))
        (loop :for i :in (rest (butlast integers-divisor))
              :with result := (first integers-divisor)
              :do (setf result
                        (mod (* result (mod i divisor)) divisor))
              :finally (return result)))))


;;--------------------------------------------------------------------
;; MOD-EXPT base power divisor => base^power-mod-divisor
;;--------------------------------------------------------------------
;; c.f. CINTA, pp.65-6, The repeated-squaring algorithm.
;; Pre-optimized version:
;; (defun %mod-expt2 (base power divisor)
;;   (let ((a (mod base divisor)) (b 1))
;;     (loop :for i :downfrom (1- (integer-length power)) :to 0 :do
;;        (if (logbitp i power)
;;            (setf b (mod (* (mod (expt b 2) divisor) a) divisor))
;;            (setf b (mod (expt b 2) divisor))))
;;     b))

(defun mod-expt (base power divisor)
  "Return BASE raised to the POWER modulo DIVISOR.
BASE, POWER must be a non-negative intgers.
DIVISOR must be a positive integer.

Examples:
  (mod-expt 986 33 42) => 20
  (mod-expt 986 33 -1) => ERROR!!
  (mod-expt 986 -23 1) => ERROR!!
"
  (unless (and (integerp base) (integerp power) (integerp divisor)
               (not (minusp base)) (not (minusp power))
               (not (minusp divisor)))
    (error "~&BASE, POWER, DIVISOR must be non-negative integers.~
            ~%BASE: ~S, POWER: ~S, DIVISOR: ~S."
           base power divisor))
  (if (and (<= power most-positive-fixnum)
           (<= divisor most-positive-fixnum)
           (<= base most-positive-fixnum))
      (%mod-expt-fixnums base power divisor)
      (%mod-expt-integers base power divisor)))

(defun %mod-expt-fixnums (base power divisor)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (fixnum fixnum fixnum) fixnum)
                  %mod-expt-fixnums)
           (fixnum base power divisor))
  (let ((a (the fixnum (mod base divisor))) (b 1))
    (declare (fixnum a b))
    (loop :for i fixnum :downfrom (1- (integer-length power)) :to 0 :do
       (if (logbitp i power)
           (setf b (the fixnum
                     (mod (the integer
                            (* (the fixnum (mod (the integer (expt b 2))
                                                divisor))
                               a))
                          divisor)))
           (setf b (the fixnum
                     (mod (the integer (expt b 2)) divisor)))))
    b))

(defun %mod-expt-integers (base power divisor)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (integer integer integer) integer)
                  %mod-expt-integers)
           (integer base power divisor))
  (let ((a (the integer (mod base divisor))) (b 1))
    (declare (integer a b))
    (loop :for i integer :downfrom (1- (integer-length power)) :to 0 :do
       (if (logbitp i power)
           (setf b (the integer
                     (mod (the integer
                            (* (the integer
                                 (mod (the integer (expt b 2))
                                      divisor))
                               a))
                          divisor)))
           (setf b (the integer
                     (mod (the integer (expt b 2)) divisor)))))
    b))


;;--------------------------------------------------------------------
;; MOD-INV n divisor => x (s.t. x*n = 1 mod divisor)
;;--------------------------------------------------------------------

(defun mod-inv (n divisor)
  "Return inverse N mod DIVISOR if it exists.
N and DIVISOR must be intgers.

Examples:
  (mod-inv 7 13) => 2   ;c.f. (mod (* 2 7) 13) => 1
  (mod-inv 2 4)  => NIL
"
  (unless (integerp n) (error "~A is not a intger." n))
  (unless (integerp divisor) (error "~A is not a intger." divisor))
  ;; If a*n + b*divisor = gcd(n,divisor) = 1,
  ;; then mod(a,divisor) * mod(n,divisor) = 1 mod divisor.
  (multiple-value-bind (gcd a b) (xgcd n divisor)
    (declare (ignore b))
    (when (= 1 gcd)
      (mod a divisor))))


;;====================================================================
