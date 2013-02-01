;;;; Last modified : 2013-02-01 20:57:21 tkych

;; cl-mod-prime/misc.lisp


;;====================================================================
;; Miscellaneous
;;====================================================================
;; Z*
;; ord    <- not yet
;; xgcd
;; phi

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
;; Z*
;;--------------------------------------------------------------------

(defun Z* (n)
  "Return set of invertible element below n.

Example:
  (Z* 42) => (1 5 11 13 17 19 23 25 29 31 37 41)
"
  ;; (if (prime-p n)
  ;;     (loop :for i :from 1 :below n :collect i)
      (loop :for i :from 1 :below n :when (= 1 (gcd i n)) :collect i))
;)

  
;;--------------------------------------------------------------------
;; XGCD x y => c, a, b,  s.t a*x + b*y = c = gcd(x,y)
;;--------------------------------------------------------------------

(defun xgcd (x y)
  "Return (values U0 U1 U2), s.t. U1*X + U2*Y = U0 = gcd(X,Y).
X,Y must be integers.

Example:
  (xgcd 42 24) => 6, -1, 2   ;42*(-1) + 24*2 = 6

"
  (unless (integerp x) (error "~S is not integer." x))
  (unless (integerp y) (error "~S is not integer." y))
  (if (and (<= x most-positive-fixnum) (<= y most-positive-fixnum))
      (%xgcd-fixnums x y)
      (%xgcd-integers x y)))

;;--------------------------------------
;; c.f. CINTA, p.79, 4.2 The extended Euclid's algorithm.
;;      TAOCP2, p.342, Algorithm X (Extended Euclid's algorithm).
;; Pre-optimized version:
;; (defun %xgcd (x y)
;;   (let ((u0 x) (u1 1) (u2 0) (v0 y) (v1 0) (v2 1))
;;     (loop :until (zerop v0)
;;           :do (multiple-value-bind (q r) (floor u0 v0)
;;                 (psetf u0 v0
;;                        u1 v1
;;                        u2 v2
;;                        v0 r
;;                        v1 (- u1 (* q v1))
;;                        v2 (- u2 (* q v2))))
;;           :finally (return (values u0 u1 u2)))))

(defun %xgcd-fixnums (x y)
"%XGCD-FIXNUM is optimaized XGCD for fixnum X,Y."
  (declare (fixnum x y) (optimize (speed 3) (debug 0) (safety 0)))
  (let ((u0 x) (u1 1) (u2 0) (v0 y) (v1 0) (v2 1))
    (declare (fixnum u0 u1 u2 v0 v1 v2))
    (loop :until (zerop v0)
          :do (multiple-value-bind (q r)
                  (the (values fixnum fixnum) (floor u0 v0))
                (declare (fixnum q r))
                (psetf u0 v0
                       u1 v1
                       u2 v2
                       v0 r
                       v1 (the fixnum (- u1 (the fixnum (* q v1))))
                       v2 (the fixnum (- u2 (the fixnum (* q v2))))))
          :finally (return (values u0 u1 u2)))))

(defun %xgcd-integers (x y)
  "%XGCD-INTEGER is optimaized XGCD for integer X,Y."
  (declare (integer x y) (optimize (speed 3) (debug 0) (safety 0)))
  (let ((u0 x) (u1 1) (u2 0) (v0 y) (v1 0) (v2 1))
    (declare (integer u0 u1 u2 v0 v1 v2))
    (loop :until (zerop v0)
          :do (multiple-value-bind (q r)
                  (the (values integer integer) (floor u0 v0))
                (declare (integer q r))
                (psetf u0 v0
                       u1 v1
                       u2 v2
                       v0 r
                       v1 (the integer (- u1 (the integer (* q v1))))
                       v2 (the integer (- u2 (the integer (* q v2))))))
          :finally (return (values u0 u1 u2)))))


;;--------------------------------------------------------------------
;; PHI n => |{ i | 1<=i<n & gcd(n,i)=1 }|
;;--------------------------------------------------------------------
;; Euler's totient function phi(N).

(defun phi (n)
  "Euler's totient function, phi(n) := |{ i | 1<=i<n & gcd(n,i)=1 }|.
Return a number of relatively prime to N from 1 below N.
N must be a integer.

Note:
  n = p1^e1 * ... * pr^er ,
  phi(n) = p1^(e1-1) * (p1-1) * ... * pr^(er-1) * (pr-1) .
"
  (unless (integerp n) (error "~S is not integer." n))
  (if (<= n 1)
      0
      (if (<= n most-positive-fixnum)
          (%euler-phi-fixnum n)
          (%euler-phi-integer n))))

;;--------------------------------------
;; c.f. Haruhiko Okumura, Arugorizumu Jiten, p.342, Gijyutsuhyo-ronsya, 1991
;; Pre-optimized version:
;; (defun %phi (n)
;;   (let ((result n))
;;     (when (evenp n)
;;       (setf result (/ result 2))
;;       (loop :do (setf n (/ n 2)) :while (evenp n)))
;;     (loop :for factor :from 3 :by 2
;;           :while (<= factor (floor n factor))
;;           :do (when (zerop (mod n factor))
;;                 (setf result (* (floor result factor) (1- factor)))
;;                 (loop :do (setf n (floor n factor))
;;                       :while (zerop (mod n factor)))))
;;     (when (< 1 n)
;;       (setf result (* (floor result n) (1- n))))
;;     result))

(defun %euler-phi-fixnum (n)
"%EULER-PHI-FIXNUM is optimaized EULER-PHI for fixnum N."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (fixnum) fixnum) %euler-phi-fixnum)
           (fixnum n))
  (let ((result n))
    (declare (fixnum result))
    (when (evenp n)
      (setf result (the fixnum (ash result -1)))
      (loop :do (setf n (the fixnum (ash n -1))) :while (evenp n)))
    (loop :for factor fixnum :from 3 :by 2
          :while (<= factor (the (values fixnum fixnum)
                              (floor n factor)))
          :do (when (zerop (the fixnum (mod n factor)))
                (setf result (the fixnum
                               (* (the (values fixnum fixnum)
                                    (floor result factor))
                                  (the fixnum (1- factor)))))
                (loop :do (setf n (the (values fixnum fixnum)
                                    (floor n factor)))
                      :while (zerop (the fixnum (mod n factor))))))
    (when (< 1 n)
      (setf result (the fixnum
                     (* (the (values fixnum fixnum) (floor result n))
                        (the fixnum (1- n))))))
    result))

(defun %euler-phi-integer (n)
"%EULER-PHI-INTEGER is optimaized EULER-PHI for integer N."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (integer) integer) %euler-phi-integer)
           (integer n))
  (let ((result n))
    (declare (integer result))
    (when (evenp n)
      (setf result (the integer (ash result -1)))
      (loop :do (setf n (the integer (ash n -1))) :while (evenp n)))
    (loop :for factor integer :from 3 :by 2
          :while (<= factor (the (values integer integer)
                              (floor n factor)))
          :do (when (zerop (the integer (mod n factor)))
                (setf result (the integer
                               (* (the (values integer integer)
                                    (floor result factor))
                                  (the integer (1- factor)))))
                (loop :do (setf n (the (values integer integer)
                                    (floor n factor)))
                      :while (zerop (the integer (mod n factor))))))
    (when (< 1 n)
      (setf result (the integer
                     (* (the (values integer integer) (floor result n))
                        (the integer (1- n))))))
    result))


;;====================================================================
