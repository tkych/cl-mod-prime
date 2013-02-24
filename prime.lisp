;;;; Last modified : 2013-02-20 08:33:22 tkych

;; cl-mod-prime/prime.lisp


;;====================================================================
;; Prime Library for Common Lisp
;;====================================================================
;; prime-p
;; *prime-p-swiching-limit*
;; *prime-p-fail-probability=1/4^*
;; next-prime
;; random-prime
;; primes-below
;; factorize
;; *factorize-swiching-limit*
;; random-factors

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
;; PRIME-P integer => boolean, fail-probability
;;--------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; *prime-p-swiching-limit*, *prime-p-fail-probability=1/4^*
  ;; prime-p,
  ;; %d-prime-p, %d-prime-p-fixnum, %d-prime-p-integer,
  ;; %mr-prime-p, %mr-prime-p-fixnum, %mr-prime-p-integer

(defparameter *prime-p-swiching-limit* 500000000
  "The function PRIME-P has internal dispatch that controls the algorithm of primality test.
A integer below *PRIME-P-SWICHING-LIMIT*, is checked primality by the Trial Division algorithm.
A integer above *PRIME-P-SWICHING-LIMIT*, is checked primality by the Miller-Rabin Probabilistic algorithm.
The default value of *PRIME-P-SWICHING-LIMIT* is 500000000.
According to my experience with my environment (see. /cl-mod-prime/test.lisp, compare-td-mr),
a integer below about 500000000, the Trial Division algorithm is more effecient than the Miller-Rabin algorithm.
If your environment is not, then config this parameter.
e.g. If you always prefer the Miller-Rabin algorithm, then (setf *PRIME-P-SWICHING-LIMIT* 0) .
")

(defparameter *prime-p-fail-probability=1/4^* 50
  "The function PRIME-P has internal iterations for the Miller-Rabin probabilistic algorithm.
If primality checking integer N is over *prime-p-swiching-limit*,
then at most FAIL-PROBABILITY, result of (PRIME-P N) may be wrong (that is, N is composite and result is T).
The parameter *PRIME-P-FAIL-PROBABILITY=1/4^* specifies a number of internal iterations.
FAIL-PROBABILITY multipled by 1/4 per 1 iteration.
Default value of *PRIME-P-FAIL-PROBABILITY=1/4^* is 50, so fail probability of prime-p is at most 1/2^100.
If you prefer more regid primality test, increce this parameter within positive fixnums.
")

(defun prime-p (n)
  "Return (values RESULT FAIL-PROBABILITY).
If N is a prime, then RESULT is always T.
Otherwise, if N is composite, then RESULT may be T or NIL.
At most FAIL-PROBABILITY, RESULT may be wrong (that is, N is composite and RESULT is T).
N must be a integer.

Note:
 * If N is below *PRIME-P-SWICHING-LIMIT* (default is 500000000),
   then FAIL-PROBABILITY is always 0.
   For more details, see doc in *PRIME-P-SWICHING-LIMIT*.

 * The parameter *PRIME-P-FAIL-PROBABILITY=1/4^* specifies a number of internal iterations.
   FAIL-PROBABILITY multipled by 1/4 per 1 iteration.
   For more details, see doc in *PRIME-P-FAIL-PROBABILITY=1/4^*.

Examples:
  (prime-p 42) => NIL, 0
  (prime-p 43) => T, 0   ;43 < *PRIME-P-SWICHING-LIMIT*
  (prime-p 843231983040012492664856905761567078617345413887258638877481099493016757762394510526492481)
       => T, 1/1267650600228229401496703205376
"
  (declare (inline %td-prime-p %mr-prime-p))
  (unless (integerp n) (error "~S is not integer." n))
  (when (member n '(2 3 5 7))
    (RETURN-FROM prime-p (values t 0)))
  (when (or (<= n 1) (evenp n) (zerop (mod n 3)))
    (RETURN-FROM prime-p (values nil 0)))
  (if (<= n *prime-p-swiching-limit*)
      (values (%td-prime-p n) 0)
      (progn
        (unless (and (integerp *prime-p-fail-probability=1/4^*)
                     (<= 1
                         *prime-p-fail-probability=1/4^*
                         most-positive-fixnum))
          (error "~S is not positive fixnum."
                 *prime-p-fail-probability=1/4^*))
        (%mr-prime-p n))))


;;--------------------------------------
;; Trial Division Algorithm
;;--------------------------------------
;; c.f. Project Euler, Problem 7, hk's overview.
;; Pre-optimized version:
;; (defun %prime-p (n)
;;   (cond ((<= n 1)  nil)              ;negative integers, 0, 1 is not prime
;;         ((< n 4)   t)                ;2, 3 are prime
;;         ((evenp n) nil)              ;multiples of 2 are not prime
;;         ((< n 9)   t)                ;4, 6, 8 are exclueded
;;         ((zerop (mod n 3)) nil)      ;multiples of 3 are not prime
;;         (t (loop :for factor :from 5 :by 6
;;                  :with root-n := (isqrt n) ;root-n^2 <= n < (root-n + 1)^2
;;                  :while (<= factor root-n)
;;                  :never (or (zerop (mod n factor))
;;                             (zerop (mod n (+ factor 2))))))))

(defun %td-prime-p (n)
  (if (<= n most-positive-fixnum)
      (%td-prime-p-fixnum n)
      (%td-prime-p-integer n)))

(defun %td-prime-p-fixnum (n)
  "%TD-PRIME-P-FIXNUM is optimaized %TD-PRIME-P for fixnum N."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (fixnum) boolean) %td-prime-p-fixnum)
           (fixnum n))
  (loop :for factor fixnum :from 5 :by 6
        :with root-n fixnum := (isqrt n) ;root-n^2 <= n < (root-n + 1)^2
        :while (<= factor root-n)
        :never (or (zerop (mod n factor))
                   (zerop (mod n (+ factor 2))))))

(defun %td-prime-p-integer (n)
  "%TD-PRIME-P-INTEGER is optimaized %TD-PRIME-P for integer N.
This function will not be called, except for *PRIME-P-SWICHING-LIMIT* is over most-positive-fixnum."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (integer) boolean) %td-prime-p-integer)
           (integer n))
  (loop :for factor integer :from 5 :by 6
        :with root-n integer := (isqrt n) ;root-n^2 <= n < (root-n + 1)^2
        :while (<= factor root-n)
        :never (or (zerop (mod n factor))
                   (zerop (mod n (+ factor 2))))))


;;--------------------------------------
;; Miller-Rabin Probabilistic algorithm
;;--------------------------------------
;; c.f. TAOCP2, p.395, Algorithm P (Probilistic primality test).
;; Pre-optimized version:
;; (defun %%prime-p (n &optional (fail-probability=1/4^ 50))
;;   (flet ((maybe-prime-p (q k)
;;            (let* ((x (1+ (random (1- n))))
;;                   (init-y (mod-expt x q n)))
;;              (if (= 1 init-y)
;;                  t
;;                  (loop :for j :from 0 :below k
;;                        :for y := init-y :then (mod-expt y 2 n)
;;                        :do (cond ((= y (1- n)) (RETURN t))
;;                                  ((= y 1)      (RETURN nil))
;;                                  (t            nil)))))))
;;     (unless (evenp n)
;;       ;; find q,k s.t. 1 + q*2^k = n.
;;       (multiple-value-bind (q k)
;;           (loop :with w := (1- n)
;;                 :for v :from 0
;;                 :while (evenp w) :do (setf w (/ w 2))
;;                 :finally (return (values w v)))
;;         (let ((result (loop :repeat fail-probability=1/4^
;;                             :always (maybe-prime-p q k))))
;;           (values result
;;                   (if result
;;                       (expt 1/4 fail-probability=1/4^)
;;                       0)))))))


(defun %mr-prime-p (n)
  "Dispath for Miller-Rabin primality test."
  (if (<= n most-positive-fixnum)
      (%mr-prime-p-fixnum n)
      (%mr-prime-p-integer n)))

(defun %mr-fail-prob ()
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function () rational)
                  %mr-fail-prob)
           (integer n))
  (/ 1 (the integer
         (ash 1
              (the integer
                (ash *prime-p-fail-probability=1/4^* 1))))))

(defun %mr-prime-p-fixnum (n)
  "%MR-PRIME-P-FIXNUM is optimaized %MR-PRIME-P for fixnum N."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (inline %mr-fail-prob)
           (ftype (function (fixnum) (values boolean rational))
                  %mr-prime-p-fixnum)
           (fixnum n))
  (let ((n-1 (the fixnum (1- n))))
    (declare (fixnum n-1))
    (labels ((maybe-prime-p (q k)       ;? flet can't be optimized ?
               (declare (optimize (speed 3) (debug 0) (safety 0))
                        (ftype (function (fixnum fixnum)
                                         (values boolean rational))
                               maybe-prime-p)
                        (fixnum q k))
               (let* ((x (the fixnum (1+ (the fixnum (random n-1)))))
                      (init-y (the fixnum (mod-expt x q n))))
                 (declare (fixnum x init-y))
                 (if (= 1 init-y)
                     t
                     (loop :for j fixnum :from 0 :below k
                           :for y fixnum := init-y
                           :then (the fixnum (mod-expt y 2 n))
                           :do (cond ((= y n-1) (RETURN t))
                                     ((= y 1)   (RETURN nil))
                                     (t         nil)))))))
      ;; find q,k s.t. 1 + q*2^k = n.
      (multiple-value-bind (q k)
          (loop :with w fixnum := n-1
                :for v fixnum :from 0
                :while (evenp w) :do (setf w (ash w -1))
                :finally (return (values w v)))
        (declare (fixnum q k))
        (let ((result (loop :repeat *prime-p-fail-probability=1/4^*
                            :always (maybe-prime-p q k))))
          (declare (boolean result))
          (values result
                  (if result (%mr-fail-prob) 0)))))))

(defun %mr-prime-p-integer (n)
  "%MR-PRIME-P-INTEGER is optimaized %MR-PRIME-P for integer N."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (inline %mr-fail-prob)
           (ftype (function (integer) (values boolean rational))
                  %mr-prime-p-integer)
           (integer n))
  (let ((n-1 (the integer (1- n))))
    (declare (integer n n-1))
    (labels ((maybe-prime-p (q k)
               (declare (optimize (speed 3) (debug 0) (safety 0))
                        (ftype (function (integer fixnum)
                                         (values boolean rational))
                               maybe-prime-p)
                        (integer q k))
               (let* ((x (the integer
                           (1+ (the integer (random n-1)))))
                      (init-y (the integer (mod-expt x q n))))
                 (declare (integer x init-y))
                 (if (= 1 init-y)
                     t
                     (loop :for j :of-type integer :from 0 :below k
                           :for y :of-type integer := init-y
                           :then (the integer (mod-expt y 2 n))
                           :do (cond ((= y n-1) (RETURN t))
                                     ((= y 1)   (RETURN nil))
                                     (t         nil)))))))
      ;; find q,k s.t. 1 + q*2^k = n.
      (multiple-value-bind (q k)
          (loop :with w :of-type integer := n-1
                :for v :of-type integer :from 0
                :while (evenp w)
                :do (setf w (the integer (ash w -1)))
                :finally (return (values w v)))
        (declare (integer q k))
        (let ((result (loop :repeat *prime-p-fail-probability=1/4^*
                            :always (maybe-prime-p q k))))
          (declare (boolean result))
          (values result
                  (if result
                      (if result (%mr-fail-prob) 0))))))))

) ; end of eval-when



;;--------------------------------------------------------------------
;; NEXT-PRIME integer num-iterations => maybe-prime, fail-probability
;;--------------------------------------------------------------------

(defun next-prime (n &optional (fail-probability=1/4^ 50))
  "Return (valuse PRIME FAIL-PROBABILITY).
PRIME is the most small prime which is above N,
s.t. N<PRIME & forall P in Primes[ N<P => PRIME<=P ].
PRIME may be non-prime at most FAIL-PROBABILITY.
N must be integer.
FAIL-PROBABILITY=1/4^ must be a positive fixnum.

Examples:
  (next-prime 12) => 13, 0
  (next-prime 13) => 17, 0
  (next-prime -19) => 2, 0
  (next-prime most-positive-fixnum)
    => 4611686018427388039, 1/1267650600228229401496703205376
"
  (unless (integerp n) (error "~S is not integer." n))
  (when (<= n 1) (RETURN-FROM next-prime (values 2 0)))
  (let ((*prime-p-fail-probability=1/4^* fail-probability=1/4^))
    (if (< n #.(loop :for i :downfrom most-positive-fixnum
                     :when (prime-p i) :return i))
        (%next-prime-fixnum n)
        (%next-prime-integer n))))

;;--------------------------------------
;; Pre-optimized version:
;; (defun %next-prime (n &optional (fail-probability=1/4^ 50))
;;   (loop :for i :from (if (evenp n) (+ n 1) (+ n 2)) :by 2
;;         :do (multiple-value-bind (prime? fail-prob)
;;                 (let ((*prime-p-fail-probability=1/4^*
;;                        fail-probability=1/4^))
;;                   (prime-p i))
;;               (when prime?
;;                 (return (values i fail-prob))))))


(defun %next-prime-fixnum (n)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (fixnum) (values fixnum rational))
                  %next-prime-fixnum)
           (fixnum n))
  (loop :for i fixnum :from (the fixnum (+ n (if (evenp n) 1 2)))
                      :by 2
        :do (multiple-value-bind (prime? fail-prob)
                (prime-p i)
              (declare (boolean prime?) (rational fail-prob))
              (when prime?
                (RETURN
                  (the (values fixnum rational)
                    (values i fail-prob)))))))

(defun %next-prime-integer (n)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (integer) (values integer rational))
                  %next-prime-integer)
           (integer n))
  (loop :for i integer :from (the integer (+ n (if (evenp n) 1 2)))
                       :by 2
        :do (multiple-value-bind (prime? fail-prob)
                (prime-p i)
              (declare (boolean prime?) (rational fail-prob))
              (when prime?
                (RETURN
                  (the (values integer rational)
                    (values i fail-prob)))))))


;;--------------------------------------------------------------------
;; RANDOM-PRIME
;;   num-bits num-iterations => maybe-prime, fail-probability
;;--------------------------------------------------------------------

;; c.f. CINTA, p.294, 9.4.2 Generating a random l-bit prime
;;                    10.3.3 Generating a random l-bit prime
;; Pre-optimized version:
;; (defun %random-prime (num-bits &optional (fail-probability=1/4^ 50))
;;   (labels ((rec (num-bits)
;;              (let ((random-integer
;;                     (loop :for i fixnum :from 0 :below (1- num-bits)
;;                           :sum (* (random 2) (expt 2 i)) :into acc
;;                           :finally
;;                           (return (+ acc (expt 2 (1- num-bits)))))))
;;                (multiple-value-bind (prime? fail-prob)
;;                    (prime-p random-integer fail-probability=1/4^)
;;                  (if prime?
;;                      (values random-integer fail-prob)
;;                      (rec num-bits))))))
;;     (let ((*prime-p-fail-probability=1/4^* fail-probability=1/4^))
;;       (rec num-bits))))


(defun random-prime (num-bits &optional (fail-probability=1/4^ 50))
  "Return (values NUM-BITS-PRIME FAIL-PROBABILITY).
NUM-BITS-PRIME may be non-prime at most FAIL-PROBABILITY.
NUM-BITS must be a positive fixnum above 1.
"
  (unless (integerp num-bits)
    (error "~S is not integer." num-bits))
  (unless (<= 2 num-bits most-positive-fixnum)
    (error "~S is not positive fixnum above 1." num-bits))
  (labels ((rec (num-bits)
             (declare (optimize (speed 3) (debug 0) (safety 0))
                      (ftype (function (fixnum) integer) rec)
                      (fixnum num-bits))
             (let ((random-integer
                    (loop :for i fixnum :from 0 :below (1- num-bits)
                          :sum (the integer
                                 (ash (the bit (random 2)) i))
                          :into acc integer
                          :finally
                          (return (the integer
                                    (+ acc
                                       (the integer
                                         (ash 1 (1- num-bits)))))))))
               (declare (integer random-integer))
               (multiple-value-bind (prime? fail-prob)
                   (prime-p random-integer)
                 (declare (boolean prime?) (rational fail-prob))
                 (if prime?
                     (values random-integer fail-prob)
                     (rec num-bits))))))
    (let ((*prime-p-fail-probability=1/4^* fail-probability=1/4^))
      (rec num-bits))))


;;--------------------------------------------------------------------
;; PRIMES-BELOW integer => list-of-primes-below-the-integer
;;--------------------------------------------------------------------
;; ! FIXME: when n is over (* 2 most-positive-fixnum) !

;; c.f. Project Euler, Problem 10, daniel.is.fischer's overview.
;; Pre-optimized version:
;; (defun %primes-below (n)
;;   (let* ((sieve-bound (floor n 2))
;;          (sieve (make-array sieve-bound          ;odd-sieve
;;                             :element-type 'bit :initial-element 0))
;;          (cross-limit (floor (isqrt n) 2)))
;;     (loop :for i :from 1 :to cross-limit :do
;;        (when (zerop (sbit sieve i))
;;          (loop :for j :from (* 2 i (1+ i)) :below sieve-bound
;;                       :by (1+ (* 2 i)) :do
;;             (setf (sbit sieve j) 1))))
;;     (loop :for i :from 1 :below sieve-bound
;;           :when (zerop (sbit sieve i))
;;           :collect (1+ (* 2 i)) :into acc
;;           :finally (return (cons 2 acc)))))


(defun primes-below (n)
  "Return a sorted list of all primes below N.
N must be a integer.

Examples:
  (primes-below 13) => (2 3 5 7 11)
  (primes-below -1) => NIL
"
  (unless (integerp n) (error "~S is not integer." n))
  (when (<= n 2) (RETURN-FROM primes-below '()))
  (if (<= n most-positive-fixnum)
      (%primes-below-fixnum n)
      (%primes-below-integer n)))

(defun %primes-below-fixnum (n)
  "%PRIMES-BELOW-FIXNUM is optimaized PRIMES-BELOW for fixnum N."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (fixnum) list) %primes-below-fixnum)
           (fixnum n))
  (let* ((sieve-bound (the fixnum (ash n -1)))
         (sieve (make-array sieve-bound
                            :element-type 'bit :initial-element 0))
         (cross-limit (the fixnum (ash (isqrt n) -1))))
    (declare (fixnum sieve-bound cross-limit)
             (type (simple-bit-vector *) sieve))
    (loop :for i fixnum :from 1 :to cross-limit :do
       (when (zerop (the bit (sbit sieve i)))
         (loop :for j fixnum :from (the fixnum
                                     (ash (the fixnum
                                            (* i (the fixnum (1+ i))))
                                          1))
                             :below sieve-bound
                             :by (the fixnum
                                   (1+ (the fixnum (ash i 1)))) :do
            (setf (sbit sieve j) 1))))
    (loop :for i fixnum :from 1 :below sieve-bound
          :when (zerop (the bit (sbit sieve i)))
          :collect (the fixnum (1+ (the fixnum (ash i 1)))) :into acc
          :finally (return (cons 2 acc)))))

(defun %primes-below-integer (n)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (integer) list) %primes-below-integer)
           (integer n))
  (let* ((sieve-bound (the integer (ash n -1)))
         (sieve (make-array sieve-bound
                            :element-type 'bit :initial-element 0))
         (cross-limit (the integer (ash (isqrt n) -1))))
    (declare (integer sieve-bound cross-limit)
             (type (simple-bit-vector *) sieve))
    (loop :for i :of-type integer :from 1 :to cross-limit :do
       (when (zerop (sbit sieve i))
         (loop :for j :of-type integer
                      :from (the integer
                              (ash (the integer
                                     (* i (the integer (1+ i))))
                                   1))
                      :below sieve-bound :by (the integer
                                               (1+ (the integer
                                                     (ash i 1)))) :do
            (setf (sbit sieve j) 1))))
    (loop :for i :of-type integer :from 1 :below sieve-bound
          :when (zerop (the bit (sbit sieve i)))
          :collect (the integer (1+ (the integer (ash i 1)))) :into acc
          :finally (return (cons 2 acc)))))


;;--------------------------------------------------------------------
;; FACTORIZE non-negative-integer &key group? algorithm
;;             => list-of-prime-factors
;;--------------------------------------------------------------------

(defparameter *factorize-swiching-limit* 50000000
  "The function FACTORIZE has internal dispatch that controls algorithm of prime factorization.
A integer below *FACTORIZE-SWICHING-LIMIT*, is factorized by Trial-Division algorithm.
A integer above *FACTORIZE-SWICHING-LIMIT*, is factorized by Pollard-Rho algorithm.
The default value of *FACTORIZE-SWICHING-LIMIT* is 50000000.
There is possibliliy that Pollard-Rho algorithm will be fail.
If you always prefer Pollard-Rho algorithm, then (setf *FACTORIZE-SWICHING-LIMIT* 0) .
")

;; (group-factors '(2 2 2 3)) => ((2 . 3) (3 . 1))
(defun %group-factors (factors)
  (cdr (nreverse
        (reduce (lambda (prev next)
                  (destructuring-bind ((prime . count) . rest) prev
                    (if (= prime next)
                        (cons (cons prime (1+ count)) rest)
                        (cons (cons next 1) prev))))
                factors :initial-value (list (cons 0 0))))))

(defun factorize (n &key (group? nil) (algorithm :auto))
  "Return (values PRIME-FACTORS FAIL-PROBABILITY ALGORITHM).
PRIME-FACTORS is sorted list of prime factors of N.
FAIL-PROBABILITY is probability that one of PRIME-FACTORS is worng.
ALGORITHM is algorithm that is used in factorize computation.

N must be a non-negative integer.
If keyword :GROUP? is t, then output PRIME-FACTORS is grouped.
Other keyword, :ALGORITHM specifies internal algorithm.
:trial-division, :division, :div are Trial-Divison algorithm.
:pollard-rho, :rho are Pollard-Rho algorithm.
:auto is Trial-Divison or Pollard-Rho algorithm whether n is below *factorize-swiching-limit* or not.

Note:
  :pollard-rho algorithm is sutable for a large N, but there is a chance result is FAIL.
  This FAIL has nothing to do with FAIL-PROBAILITY.
  FAIL-PROBAILITY is came from primt-p with miller-rabin algorithm,
  whereas FAIL is intrinsic to pollard-rho algorithm.

Examples:
  (factorize 42)  => (2 3 7), 0, :TRIAL-DIVISION
  (factorize 1)   => NIL, 0, :TRIAL-DIVISION
  (factorize 0)   => NIL, 0, :TRIAL-DIVISION
  (factorize -53) => ERROR!!

  (factorize 1024) => (2 2 2 2 2 2 2 2 2 2), 0, :TRIAL-DIVISION
  (factorize 1024 :group? t) => ((2 . 10)), 0, :TRIAL-DIVISION

  (factorize 35262714657262341)
     => (3 47 250090174874201), 1/1208925819614629174706176, :POLLARD-RHO
  (factorize 25 :algorithm :rho)
     => NIL, 1/1208925819614629174706176, :POLLARD-RHO  ;!! FAIL !!
"
  (unless (integerp n) (error "~S is not integer." n))
  (when (minusp n) (error "~S is not positive integer nor zero." n))
  (if (<= n 1)
      (values '() 0 :trial-division)
      (multiple-value-bind (factors fail-prob algo)
          (case algorithm
            ((:auto)
             (if (<= n *factorize-swiching-limit*)
                 (values (%td-factorize n) 0 :trial-division)
                 (multiple-value-bind (facs prob)
                     (%rho-factorize n)
                   (values facs prob :pollard-rho))))
            ((:rho :pollard-rho)
             (multiple-value-bind (facs prob)
                     (%rho-factorize n)
                   (values facs prob :pollard-rho)))
            ((:div :trial-division :divsion)
             (values (%td-factorize n) 0 :trial-division))
            (t
             (error "~S is unknown algorithm." algorithm)))
        (values (if group? (%group-factors factors) factors)
                fail-prob
                algo))))

;;--------------------------------------
;; Trial-Divison Algorithm
;;--------------------------------------
;; c.f. Project Euler, Problem 3, hk's overview.
;; Pre-optimized version:
;; (defun %factorize (n)
;;   (let ((factors '()))
;;     (LOOP (multiple-value-bind (q r) (floor n 2)
;;             (if (zerop r)
;;                 (setf n q factors (cons 2 factors))
;;                 (RETURN))))
;;     (loop :with max-factor := (isqrt n)
;;           :for factor :from 3 :by 2
;;           :while (and (< 1 n) (<= factor max-factor))
;;           :do (LOOP (multiple-value-bind (q r) (floor n factor)
;;                       (if (zerop r)
;;                           (setf n q factors (cons factor factors))
;;                           (RETURN)))))
;;     (if (= 1 n)
;;         (nreverse factors)
;;         (nreverse (push n factors)))))

(defun %td-factorize (n)
  (if (<= n most-positive-fixnum)
      (%td-factorize-fixnum n)
      (%td-factorize-integer n)))

(defun %td-factorize-fixnum (n)
  "%TD-FACTORIZE-FIXNUM is optimaized FACTORIZE for fixnum N."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (fixnum) list) %td-factorize-fixnum)
           (fixnum n))
  (let ((factors '()))
    (declare (list factors))
    (LOOP (multiple-value-bind (q r) (floor n 2)
            (declare (fixnum q r))
            (if (zerop r)
                (setf n q factors (cons 2 factors))
                (RETURN))))
    (loop :with max-factor fixnum := (isqrt n)
          :for factor fixnum :from 3 :by 2
          :while (and (< 1 n) (<= factor max-factor))
          :do (LOOP (multiple-value-bind (q r) (floor n factor)
                      (declare (fixnum q r))
                      (if (zerop r)
                          (setf n q factors (cons factor factors))
                          (RETURN)))))
    (if (= 1 n)
        (nreverse factors)
        (nreverse (push n factors)))))

(defun %td-factorize-integer (n)
  "%TD-FACTORIZE-INTEGER is optimaized FACTORIZE for integer N."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (integer) list) %td-factorize-integer)
           (integer n))
  (let ((factors '()))
    (declare (list factors))
    (LOOP (multiple-value-bind (q r) (floor n 2)
            (declare (integer q r))
            (if (zerop r)
                (setf n q factors (cons 2 factors))
                (RETURN))))
    (loop :with max-factor integer := (isqrt n)
          :for factor integer :from 3 :by 2
          :while (and (< 1 n) (<= factor max-factor))
          :do (LOOP (multiple-value-bind (q r) (floor n factor)
                      (declare (integer q r))
                      (if (zerop r)
                          (setf n q factors (cons factor factors))
                          (RETURN)))))
    (if (= 1 n)
        (nreverse factors)
        (nreverse (push n factors)))))

;;--------------------------------------
;; Pollard-Rho Algorithm
;;--------------------------------------
;; c.f, TAOCP2, p.385, Algorithm B (Factoring by the rho method).
;; Pre-optimized version:
;; (defun %rho-factorize (n)
;;   (let ((factors nil))
;;     (LOOP (multiple-value-bind (q r) (floor n 2)
;;             (if (zerop r)
;;                 (setf n q factors (cons 2 factors))
;;                 (RETURN))))
;;     (if (< n 2)
;;         (values factors 0)
;;         (let ((x0 5) (x1 2) (k 1) (j 1))                           ;B1
;;           (loop :until (prime-p n) :do                             ;B2
;;              (loop :for g := (gcd (- x1 x0) n) :do                 ;B3
;;                 (cond ((= g 1) (decf k)                            ;B4
;;                                (when (zerop k)
;;                                  (setf x1 x0 j (* 2 j) k j))
;;                                (setf x0 (mod (1+ (expt x0 2)) n))) ;->B3
;;                       ((= g n) (RETURN-FROM %rho-factorize
;;                                  (values nil 1)))                  ;->FAIL
;;                       (t       (setf factors (cons g factors)
;;                                      n (floor n g)
;;                                      x0 (mod x0 n)
;;                                      x1 (mod x1 n))
;;                                (return))))                         ;->B2
;;              :finally (push n factors))
;;           (nreverse factors))))))

(defun %rho-factorize (n)
  (if (<= n most-positive-fixnum)
      (%rho-factorize-fixnum n)
      (%rho-factorize-integer n)))

(defun %rho-factorize-fixnum (n)
  "%RHO-FACTORIZE-FIXNUM is optimaized %RHO-FACTORIZE for FIXNUM N."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (fixnum fixnum) list) %rho-factorize-fixnum)
           (fixnum n))
  (let ((factors nil))
    (declare (list factors))
    (LOOP (multiple-value-bind (q r) (the (values fixnum fixnum)
                                       (floor n 2))
            (declare (fixnum q r))
            (if (zerop r)
                (setf n q factors (the list (cons 2 factors)))
                (RETURN))))
    (if (< n 2)
        (values factors 0)
        (let ((x0 5) (x1 2) (k 1) (j 1))
          (declare (fixnum x0 x1 k j))
          (loop :until (the (values boolean rational)
                         (prime-p n)) :do
             (loop
                :for g :of-type fixnum
                := (the fixnum
                     (xgcd (the fixnum (- x1 x0)) n)) :do
                (cond ((= g 1) (the fixnum (decf k))
                       (when (zerop k)
                         (setf x1 x0 j (the fixnum
                                         (ash j 1)) k j))
                       (setf x0 (the fixnum
                                  (mod (the fixnum
                                         (1+ (the fixnum
                                               (expt x0 2))))
                                       n))))
                      ((= g n) (RETURN-FROM %rho-factorize-fixnum
                                 (values nil 1)))
                      (t       (setf factors (the list (cons g factors))
                                     n (the (values fixnum fixnum)
                                         (floor n g))
                                     x0 (the fixnum (mod x0 n))
                                     x1 (the fixnum (mod x1 n)))
                               (return))))
             :finally (push n factors))
          (values (sort (nreverse factors) #'<)
                  (loop :repeat (count-if
                                 (lambda (p)
                                   (<= *prime-p-swiching-limit* p))
                                 factors)
                        :sum (%mr-fail-prob)))))))

(defun %rho-factorize-integer (n)
  "%RHO-FACTORIZE-INTEGER is optimaized %RHO-FACTORIZE for integer N."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (integer fixnum) list) %rho-factorize-integer)
           (integer n))
  (let ((factors nil))
    (declare (list factors))
    (LOOP (multiple-value-bind (q r) (the (values integer integer)
                                       (floor n 2))
            (declare (integer q r))
            (if (zerop r)
                (setf n q factors (the list (cons 2 factors)))
                (RETURN))))
    (if (< n 2)
        (values factors 0)
        (let ((x0 5) (x1 2) (k 1) (j 1))
          (declare (integer x0 x1 k j))
          (loop :until (the (values boolean rational)
                         (prime-p n)) :do
             (loop
                :for g :of-type integer
                := (the integer
                     (xgcd (the integer (- x1 x0)) n))
                :do (cond ((= g 1) (the integer (decf k))
                           (when (zerop k)
                             (setf x1 x0 j (the integer
                                             (ash j 1)) k j))
                           (setf x0 (the integer
                                      (mod (the integer
                                             (1+ (the integer
                                                   (expt x0 2))))
                                           n))))
                          ((= g n) (RETURN-FROM %rho-factorize-integer
                                     (values nil 1)))
                          (t       (setf factors (the list (cons g factors))
                                         n (the (values integer integer)
                                             (floor n g))
                                         x0 (the integer (mod x0 n))
                                         x1 (the integer (mod x1 n)))
                                   (return))))
             :finally (push n factors))
          (values (sort (nreverse factors) #'<)
                  (loop :repeat (count-if
                                 (lambda (p)
                                   (<= *prime-p-swiching-limit* p))
                                 factors)
                         :sum (%mr-fail-prob)))))))


;;--------------------------------------------------------------------
;; RANDOM-FACTORS integer &key group?
;;                   => random-factors-number fail-probability
;;--------------------------------------------------------------------
;; c.f. CINTA, p.298, algorithm RFN (Random Factors Number)

(defun random-factors (n &key (group? nil))
  "Return factorized random number below N.
If group? is T, then factors will be grouped.
N must be a integer above 1.

Examples:
  (random-factors 1)  => ERROR!!
  (random-factors 2)  => NIL
  (random-factors 42) => (2 5)
  (random-factors 420000 :group? t) => ((2 . 3) (3 . 1) (3049 . 1))
"
  (unless (and (integerp n) (<= 2 n))
    (error "~S must be a integer above 1." n))
  ;; c.f. CINTA, p.295, algorithm RS (Random non-increasing Sequence)
  (flet ((random-non-increace-seq (n)
           (loop :for r := n :then (1+ (random r))
                 :collect r :into seq
                 :until (= r 1)
                 :finally (return (rest seq)))))
    (loop :for seq := (random-non-increace-seq n)
          :for prime-seq := (remove-if-not #'prime-p seq)
          :do (let ((y (reduce #'* prime-seq)))
                (when (<= (1+ (random n)) y n)
                    (RETURN (values (if group?
                                        (%group-factors (reverse prime-seq))
                                        (reverse prime-seq))
                                    (loop :repeat (count-if
                                                   (lambda (p)
                                                     (<= *prime-p-swiching-limit* p))
                                                   prime-seq)
                                          :sum (%mr-fail-prob)))))))))

;;====================================================================
