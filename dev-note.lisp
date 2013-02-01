;;;; Last modified : 2013-01-31 20:05:48 tkych

(ensure-project-path #p"/home/tkych/projects/cl-mod-prime/")
(bring :cl-mod-prime)

(md-to-html #p"/home/tkych/projects/cl-mod-prime/README.md"
            #p"/home/tkych/projects/cl-mod-prime/README.html")


;;====================================================================
;; Development Note for CL-MOD-PRIME
;;====================================================================

;; abbrev TAOCP2 as 
;;  D. Knuth. The Art of Computer Programming, vol.2:
;;            Seminumerical Algorithms. 3rd ed., Addison-Wesley, 1997.

;; abbrev CINTA as
;;  V. Shoup. A Computational Introduction to Number Theory and Algebra.
;;            2nd ed., Cambridge University Press, 2008.
;;            available at http://shoup.net/ntb/ntb-v2.pdf



;;--------------------------------------------------------------------
;; CINTA, p.295, algorithm RS (Random non-increasing Sequence)
(defun random-non-increace-seq (m)
  (unless (<= 2 m)
    (error "~S must be a integer from 2." m))
  (loop :for n := m :then (1+ (random n))
        :collect n :into seq
        :until (= n 1)
        :finally (return (rest seq))))
        

;; 9.6 Generating a random factored number
;; CINTA, p.298, algorithm RFN (Random Factored Number)
(defun random-facterd (m)
  (loop :for seq := (random-non-increace-seq m)
        :for prime-seq := (loop :for i :in seq
                                :when (prime-p i) :collect i)
        :do (let ((y (reduce #'* prime-seq)))
              (if (<= y m)
                  (let ((x (1+ (random m))))
                    (if (<= x y)
                        (RETURN prime-seq)))))))

;;--------------------------------------------------------------------

(defun %%factorize1 (n)
  (loop :for a := (random n)
        :for d := (gcd a n)
        :



;;--------------------------------------
;; see, TAOCP2, p.387,
;;      Algorithm C (Factoring by addition and subtraction).
;; (defun %factorize3 (n)
;;   "Return (values P Q) s.t. P*Q=N & P<=root(N)<=Q.
;; N must be odd integer."
;;   (when (oddp n)
;;     (loop
;;        :with a := (1+ (* 2 (isqrt n)))
;;        :with b := 1
;;        :with r := (- (expt (isqrt n) 2) n)
;;        :until (zerop r)
;;        :do (setf r (+ r a) a (+ a 2))
;;            (loop :while (plusp r) :do (setf r (- r b) b (+ b 2)))
;;        :finally (return (values (/ (- a b) 2)
;;                                 (/ (- (+ a b) 2) 2))))))



;;--------------------------------------------------------------------
;; CINTA, p.326, 11 Finding generators and discrete logarithms in Zp*

;; (group-factors '(2 2 2 3)) => ((2 . 3) (3 . 1))
(defun group-factors (factors)
  (cdr (nreverse
        (reduce (lambda (prev next)
                  (destructuring-bind ((p . e) . rest) prev
                    (if (= p next)
                        (cons (cons p (1+ e)) rest)
                        (cons (cons next 1) prev))))
                factors :initial-value (list (cons 0 0))))))

;; !! TODO !! c.f. CINTA, p.329, EXERCISE 11.2.

;; c.f. CINTA, p.328.
(defun find-generator (p)
  (let ((factors (group-factors (factorize (1- p))))
        (generator 1))
    (loop :for (q . e) :in factors :do
       (let ((a (loop :for a := (random p)
                      :for b := (mod-expt a (/ (1- p) q) p)
                      :when (/= 1 b) :return a)))
       (setf generator
             (mod (* generator (mod-expt a (/ (1- p) (expt q e)) p))
                  p))))
    generator))



;;--------------------------------------------------------------------
;; For all prime p, there exists a generator g in Zp* s.t.
;; for every n in Zp*, there exists unique x in Zp s.t. n = g^x mod p.
;; The integer x is called the DISCRETE LOGARITHMS of n to the base g,
;; and denoted (DLOG n g p).

;; (defun naive-dlog (n base divisor)
;;   (let ((rep-n (mod n divisor)))
;;     (loop :for i :from 0 :to divisor
;;           :when (= rep-n (mod-expt base i divisor)) :return i)))


;; brute force
(defun dlog1 (a g divisor)
  (loop :for i :from 1
        :for b := 1 :then (* g b)
        :while (/= b a)
        :finally (return i)))

;; baby step/giant step method
;; ht -> bst or radix tree (also called a search trie)
(defun dlog2 (a g p)
  (let ((ht (make-hash-table :initial-element nil))
        (m  (ceiling (ord a p) 2)))
    ;; baby step
    (loop :for i :from 0 :below m
          :for b := 1 :then (mod (* b g) p)
          :do (setf (gethash b ht) i))
    ;; giant step
    (loop :with inv-g := (mod-inv g p)
          :for b := a :then (mod (* b inv-g) p)
          :for j :from 0
          :for i := (gethash b ht)
          :until i
          :finally (return (+ i (* j m))))))

;; c.f. CINTA, p.332, Algoritm RDL.



;; 11.2.5 A space-efficient square-root time algorithm





;;--------------------------------------------------------------------
;; 12 Quadratic reciprocity and computing modular square roots
;;--------------------------------------------------------------------


;; c.f. CINTA, 12.3 Computing the jacobi symbol.
;; n: odd positive integer, a: integer,
;;   s.t. n=q1*q2*...*qk, qi is odd prime.
;; jabobi-symbol (a | n) := (a | q1)*...*(a | qk)

;; XXXX
(defun jacobi-symbol (a n)
  "Return +1 or -1, whether A has quare root in modulo N or not.
"
  (loop :with s := 1 :do
     (assert (and (oddp n) (plusp n)))  ;loop invariant
     (setf a (mod a n))
     (when (zerop a)
       (if (= 1 n) (RETURN s) (RETURN 0)))
     (multiple-value-bind (aa h)
         (loop :with w := (1- n)
               :for v :from 0
               :while (evenp w) :do (setf w (ash w -1))
               :finally (return (values w v)))
       (when (and (oddp h) (/= 1 (abs (mod n 8))))
         (setf s (- s)))
       (when (and (/= 1 (mod aa 4)) (/= 1 (mod n 4)))
         (setf s (- s)))
       (setf a n n aa))))

;; (assert (equal (loop :for i :from 1 :to 10
;;                      :collect (jacobi-symbol i 11))
;;                '(1 -1 1 1 1 -1 -1 -1 1 -1)))  ; Fail !!

;; !! TODO !! Exercise 12.5, "binary" Jacobi symbol algorithm.
;;            see. Exercise 4.6.

;; Exercise 4.6 (Binary gcd algorithm)
;; (defun bgcd (a b)
;;   (let ((e 0))
;;     (loop :while (and (evenp a) (evenp b))
;;           :do (setf a (ash a -1) b (ash b -1) e (1+ e)))
;;     (LOOP
;;        (loop :while (evenp a) :do (setf a (ash a -1)))
;;        (loop :while (evenp b) :do (setf b (ash b -1)))
;;        (when (< b a) (rotatef a b))
;;        (decf b a)
;;        (when (zerop b) (RETURN)))
;;     (ash a e)))


;;--------------------------------------
;; CINTA, 12.5 Computing modular square roots.

;; 1. modulus: Prime

(defun mod-sqrt (n p)
  (unless (and (odd p) (prime-p p))
    (error "~S is not odd prime." p))
  (let ((a (mod n p)))
  (when (= 1 (jacobi-symbol a p))
    (let ((z* (z* p)))
      (if (= 3 (mod p 4))
          (mod-expt a (/ (1+ p) 4) p) ;by Fermat's little theorem
          (



;; 2. modulus: Prime^e


;; 3. modulus: Composite


;;--------------------------------------------------------------------
;; CINTA, 14.4 Gaussian elimination

;; Def: reduced row echelon form

;; Def: pivot sequence

;; Def: Gaussian elimination is an algorithm that transforms a given
;; matrix A in F^m*n into a matrix B in F^m*n,
;; where B is in reduced row echelon form, and is obtained from
;; A by a sequence of elementary row operations.
;; There are three types of elementary row operations:
;;   Type I: swap two rows;
;;   Type II: multiply a row by a non-zero scalar;
;;   Type III: add a scalar multiple of one row to a different row.

(defun swap-rows (i j mat)
  )

(defun mult-row (a i matrix)
  )

(defun add-mult-row (a i j matrix)
  )
  
(defun gaussian-elimination (matrix)
  (let ((r 0))
    (loop :for j :from 1 :to n :do
       (let ((l 0) (i r))
         (loop :while (and (zerop l) (<= i m)) :do
            (incf i)
            (when (not (null-matrix-p matrix))
              (setf l i)))
         (when (/= l 0)
           (incf r)
           (setf matrix (swap-rows r l matrix))
           (setf (row r matrix)
                 (mat-inv r j matrix) * (row r matrix))
           (loop :for 


;; Algorithm (extended Gaussian elimination)


;; (gaussian-elimination ((0 1 1)
;;                        (2 1 2)
;;                        (2 2 0)))
;; => ((1 0 2)
;;     (0 1 1)
;;     (0 0 0))



;;--------------------------------------------------------------------
;; CINTA, 15 Subexponential-time descrete logarithms and factoring

;; Smooth number:
;; If y is a non-negative real number and m is a positive integer,
;; then we say that m is y-SMOOTH if all prime divisors of m are
;; at most y.

(defun smooth-p (y m)
  (and (integerp y) (not (minusp y))
       (integerp m) (plusp m)
       (>= y (apply #'max (factorize m)))))


;; Algoritm SEDL (SubExponential-time Descrete Logarithms).
(defun dlog (p q g a)
  (let ((Zp* (Z* p)))
    (loop
       :for i :from 0
       :do (loop
              :for r := (random q)
              :for s := (random q)
              :for d := (random-nth Zp*)
              :until (= m 
 :until (= i (1+ k))

 (if (zerop (mod s q))
     nil
     (mod (* (- r) (mod-inv s q)) q))



;; Lemma 15.2.
;; The probability that Algorithm SEDL outputs “failure” is 1/q.


;;--------------------------------------------------------------------






;;====================================================================
;; Test
;;====================================================================
(defun randoms-below (limit repeat)
  (loop :repeat repeat :collect (random limit)))

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
;; Utils
;;====================================================================

(defmacro *2 (x) `(ash ,x 1))
(defmacro /2 (x) `(ash ,x -1))
(defmacro expt-1/4 (n) `(/ 1 (ash 1 (ash ,n 1))))

(defmacro %evenp (n) `(zerop (logand ,n 1)))
(defmacro %oddp (n) `(zerop (logandc1 ,n 1)))

(defmacro %evenp1 (n) `(not (logbitp 0 ,n)))
(defmacro %oddp1 (n) `(logbitp 0 ,n))

(defun %evenp (n) (zerop (logand n 1)))
(defun %evenp1 (n) (not (logbitp 0 n)))




;;====================================================================
;; Mod
;;====================================================================
;; mod-mult
;; mod-expt
;; mod-inv
;; mod-sqrt      <- not yet
;; mod-nth-root  <- not yet
;; dlog          <- not yet



;;--------------------------------------------------------------------
;; MOD-MULT &rest integers-divisor => integer
;;--------------------------------------------------------------------
(defun mod-mult (&rest integers-divisor)
  "Return multiple of INTEGERS modulo DIVISOR."
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

;; c.f. CINTA, pp.65-6, The repeated-squaring algorithm. <- adopt
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
e.g. (mod-expt 986 33 42) => 20
     (mod-expt 986 33 -1) => ERROR!!
     (mod-expt 986 -23 1) => ERROR!!"
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


;; Optimization:
;;  BASE    POWER   DIVISOR  implemented?
;;------------------------------------------
;;  fixnum  fixnum  fixnum   yes
;;  integer integer integer  yes
;;  fixnum  fixnum  integer  no!?
;;  fixnum  integer fixnum   no!?
;;  integer fixnum  fixnum   no!?
;;  fixnum  integer integer  no!?
;;  integer fixnum  integer  no!?
;;  integer integer fixnum   no!?

;;--------------------------------------
;; c.f. TAOCP2, p.462,
;;      Algorithm A (Right-to-left binary method for exponentiation).
;; Pre-optimized version:
;; (defun %mod-expt (base power divisor)
;;   (loop :with n := power
;;         :with y := 1
;;         :with z := (mod base divisor)
;;         :until (zerop n)
;;         :do (when (oddp n)
;;               (setf y (mod (* y z) divisor)))
;;             (setf n (ash n -1)
;;                   z (mod (expt z 2) divisor))
;;         :finally (return y)))

;;--------------------------------------
;; (defun %mod-expt-fixnums (base power divisor)
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (ftype (function (integer fixnum fixnum) fixnum)
;;                   %mod-expt-fixnums)
;;            (fixnum base power divisor))
;;   (loop :with n fixnum := power
;;         :with y fixnum := 1
;;         :with z fixnum := (the fixnum (mod (the fixnum base)
;;                                            (the fixnum divisor)))
;;         :until (zerop n)
;;         :do (when (oddp n)
;;               (setf y (the fixnum (mod (the integer (* y z))
;;                                        (the fixnum divisor)))))
;;             (setf n (the fixnum (ash n -1))
;;                   z (the fixnum (mod (the integer (expt z 2))
;;                                      (the fixnum divisor))))
;;         :finally (return y)))

;; (defun %mod-expt-integers (base power divisor)
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (ftype (function (integer integer integer) integer)
;;                   %mod-expt-integers)
;;            (integer base power divisor))
;;   (loop :with n :of-type integer := power
;;         :with y :of-type integer := 1
;;         :with z :of-type integer := (the integer (mod base divisor))
;;         :until (zerop n)
;;         :do (when (oddp n)
;;               (setf y (the integer
;;                         (mod (the integer (* y z)) divisor))))
;;             (setf n (the integer (ash n -1))
;;                   z (the integer
;;                       (mod (the integer (expt z 2)) divisor)))
;;         :finally (return y)))


;;--------------------------------------
;; c.f. CINTA, pp.65-6, The repeated-squaring algorithm.

;; Pre-optimized version:
;; (defun %mod-expt2 (base power divisor)
;;   (let ((a (mod base divisor)) (b 1))
;;     (loop :for i :downfrom (1- (integer-length power)) :to 0 :do
;;        (if (logbitp i power)
;;            (setf b (mod (* (mod (expt b 2) divisor) a) divisor))
;;            (setf b (mod (expt b 2) divisor))))
;;     b))

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


;; (defun test-mod-expt2-1 (n)
;;   (loop :repeat n
;;         :for base := (random most-positive-fixnum)
;;         :for power := (random most-positive-fixnum)
;;         :for divisor := (random most-positive-fixnum)
;;         :always (= (%mod-expt2 base power divisor)
;;                    (%mod-expt base power divisor))))
;; (test-mod-expt2-1 100000) => T

;; (defun gen-fixnum-randoms (num-random)
;;   (loop :repeat num-random :collect (random most-positive-fixnum)))

;; (defun test-mod-expt2-2 (n)
;;   (let ((bases    (gen-fixnum-randoms n))
;;         (powers   (gen-fixnum-randoms n))
;;         (divisors (gen-fixnum-randoms n)))
;;     (equal (time (mapcar #'%mod-expt2 bases powers divisors))
;;            (time (mapcar #'%mod-expt bases powers divisors)))))

;;--------------------------------------------------------------------
;; MOD-INV n divisor => x (s.t. x*n = 1 mod divisor)
;;--------------------------------------------------------------------

(defun mod-inv (n divisor)
  "Return inverse N mod DIVISOR if it exists.
N and DIVISOR must be intgers.
e.g. (mod-inv 7 13) => 2   ;c.f. (mod (* 2 7) 13) => 1
     (mod-inv 2 4)  => NIL"
  (unless (integerp n) (error "~A is not a intger." n))
  (unless (integerp divisor) (error "~A is not a intger." divisor))
  ;; If a*n + b*divisor = gcd(n,divisor) = 1,
  ;; then mod(a,divisor) * mod(n,divisor) = 1 mod divisor.
  (multiple-value-bind (gcd a b) (xgcd n divisor)
    (declare (ignore b))
    (when (= 1 gcd)
      (mod a divisor))))


;; (defun mod-inv (x p)
;;   ;; Fermat's theorem:
;;   ;;   If p is prime, then x^(p-1) = 1 in Zp for all x in (Zp)*.
;;   (if (prime-p p)
;;       (mod-expt x (- p 2) p)
;;       ))


;; !!TODO!! c.f. TAOCP2, p.354, Exercise 17


;;--------------------------------------------------------------------
;; MOD-SQRT n divisor => x (s.t. x^2 = n mod divisor) or nil
;;--------------------------------------------------------------------
;; c.f. CINTA, pp.35-44, 2.8 Quadratic residues, 12.6



;;--------------------------------------------------------------------
;; MOD-NTH-ROOT
;;--------------------------------------------------------------------


;;--------------------------------------------------------------------
;; DLOG n base divisor => x (s.t. base^x = n in modulo divisor)
;;--------------------------------------------------------------------
;; c.f. CINTA, p.326,
;;      11 Finding generators and discrete logarithms in Zp*

;; For all prime p, there exists a generator g in Zp* s.t.
;; for every a in Zp*, there exists unique x in Zp s.t.
;; a=g^x in modulo p.
;; The integer x is called the DISCRETE LOGARITHMS of a to the base g,
;; and denoted (DLOG a g p).


;; (defun naive-dlog (n base divisor)
;;   (let ((rep-n (mod n divisor)))
;;     (loop :for i :from 0 :to divisor
;;           :when (= rep-n (mod-expt base i divisor)) :return i)))


;; brute force
;; (defun dlog1 (a g divisor)
;;   (loop :for i :from 1
;;         :for b := 1 :then (* g b)
;;         :while (/= b a)
;;         :finally (return i)))

;; baby step/giant step method
;; !? ht -> bst or radix tree (also called a search trie) !?
;; (defun dlog2 (a g p)
;;   (let ((ht (make-hash-table :initial-element nil))
;;         (m  (ceiling (order a p) 2)))
;;     ;; baby step
;;     (loop :for i :from 0 :below m
;;           :for b := 1 :then (mod (* b g) p)
;;           :do (setf (gethash b ht) i))
;;     ;; giant step
;;     (loop :with inv-g := (mod-inv g p)
;;           :for b := a :then (mod (* b inv-g) p)
;;           :for j :from 0
;;           :for i := (gethash b ht)
;;           :until i
;;           :finally (return (+ i (* j m))))))






;; c.f. CINTA, p.403, Algoritm SEDL.
;; (defun dlog (p q g a)
;;   (loop :for i :from 0 :until (= i (1+ k)) :do
;;      (loop :for r := (random (1- q))
;;            :for s := (random (1- q))
;;            :until (= m 



;;====================================================================
;; Prime
;;====================================================================
;; prime-p
;; maybe-prime-p
;; primes-below
;; factorize
;; maybe-factorize
;; next-prime
;; random-maybe-prime






;;--------------------------------------------------------------------
;; PRIME-P integer => boolean
;;--------------------------------------------------------------------
;; c.f. Project Euler, Problem 7, hk's overview.
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

(defun prime-p (n)
  "Return T if N is a prime, NIL otherwise.
N must be a integer.
For a large integer, use function MAYBE-PRIME-P instead."
  (unless (integerp n) (error "~S is not integer." n))
  (if (<= n most-positive-fixnum)
      (%prime-p-fixnum n)
      (%prime-p-integer n)))

(defun %prime-p-fixnum (n)
  "%PRIME-P-FIXNUM is optimaized PRIME-P for fixnum N."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (fixnum) boolean) %prime-p-fixnum)
           (fixnum n))
  (cond ((<= n 1)  nil)        ;negative integers, 0, 1 is not prime
        ((< n 4)   t)          ;2, 3 are prime
        ((evenp n) nil)        ;multiples of 2 are not prime
        ((< n 9)   t)          ;4, 6, 8 are exclueded
        ((zerop
          (the fixnum (mod n 3))) nil) ;multiples of 3 are not prime
        (t (loop :for factor fixnum :from 5 :by 6
                 :with root-n fixnum := (isqrt n) ;root-n^2 <= n < (root-n + 1)^2
                 :while (<= factor root-n)
                 :never (or (zerop (mod n factor))
                            (zerop (mod n (+ factor 2))))))))

(defun %prime-p-integer (n)
  "%PRIME-P-INTEGER is optimaized PRIME-P for integer N."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (integer) boolean) %prime-p-integer)
           (integer n))
  (cond ((evenp n) nil)              ;multiples of 2 are not prime
        ((zerop (mod n 3)) nil)      ;multiples of 3 are not prime
        (t (loop :for factor integer :from 5 :by 6
                 :with root-n integer := (isqrt n) ;root-n^2 <= n < (root-n + 1)^2
                 :while (<= factor root-n)
                 :never (or (zerop (mod n factor))
                            (zerop (mod n (the integer
                                            (+ factor 2)))))))))

;; (defun test-prime-p (n)
;;   (let ((random-integers
;;          (loop :repeat n :collect (random most-positive-fixnum))))
;;     (equal (time (loop :for i :in random-integers
;;                        :when (prime-p i) :collect i))
;;            (time (loop :for i :in random-integers
;;                        :when (maybe-prime-p i) :collect i)))))


;;--------------------------------------------------------------------
;; MAYBE-PRIME-P integer num-iterations => boolean fail-probability
;;--------------------------------------------------------------------
;; Miller–Rabin primality test.
;; c.f. TAOCP2, p.395, Algorithm P (Probilistic primality test).

;; About fail probability:
;; see. TAOCP2, p., Exercise 4.5.4.22.
;;      CINTA, p., 

;; A probability Epsilon is nigagible for practice,
;; if Epsilon <= 1/2^80. (at 2012)

;; !!! (maybe-prime-p 4) => NIL !!! (unless (evenp n)



;;--------------------------------------
;; Pre-optimized version:
;; (defun maybe-prime-p (n &optional (fail-probability=1/4^ 40))
;;   (flet ((probabilistic-prime-p (q k)
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
;;                             :always (probabilistic-prime-p q k))))
;;           (values result
;;                   (if result
;;                       (expt 1/4 fail-probability=1/4^)
;;                       0)))))))


(defun maybe-prime-p (n &optional (fail-probability=1/4^ 40))
  "Return (values RESULT FAIL-PROBABILITY).
If N is a prime, then RESULT is always T.
Otherwise, if N is composite, then RESULT may be T or NIL.
RESULT may be wrong (that is, N is composite and RESULT is T) at most FAIL-PROBABILITY.
FAIL-PROBABILITY=1/4^ specifies a number of internal iterations.
FAIL-PROBABILITY multipled by 1/4 per 1 iteration. 
N must be a integer. FAIL-PROBABILITY=1/4^ must be a positive fixnum.
For fixnum, use PRIME-P instead.
e.g. (maybe-prime-p 843231983040012492664856905761567078617345413887258638877481099493016757762394510526492481)
     => T, 1/1208925819614629174706176"
  (unless (integerp n) (error "~S is not integer." n))
  (unless (and (integerp fail-probability=1/4^)
               (<= 1 fail-probability=1/4^ most-positive-fixnum)) ;!!!1<
    (error "~S is not positive fixnum." fail-probability=1/4^))
  (when (= n 2) (RETURN-FROM maybe-prime-p (values t 0)))
  (when (or (<= n 1) (evenp n)) (RETURN-FROM maybe-prime-p (values nil 0)))
  (if (<= n most-positive-fixnum)
      (%maybe-prime-p-fixnum n fail-probability=1/4^)
      (%maybe-prime-p-integer n fail-probability=1/4^)))

;; !! ftype probabilistic-prime-p !!
(defun %maybe-prime-p-fixnum (n fail-probability=1/4^)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (fixnum fixnum) (values boolean rational))
                  %maybe-prime-p-fixnum)
           (fixnum n fail-probability=1/4^))
  (let ((n-1 (the fixnum (1- n))))
    (declare (fixnum n-1))
    (flet ((probabilistic-prime-p (q k)
             (declare (optimize (speed 3) (debug 0) (safety 0))
                      (ftype (function (fixnum fixnum) (values boolean rational)))
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
      (unless (evenp n)
        ;; find q,k s.t. 1 + q*2^k = n.
        (multiple-value-bind (q k)
            (loop :with w fixnum := n-1
                  :for v fixnum :from 0
                  :while (evenp w) :do (setf w (ash w -1))
                  :finally (return (values w v)))
          (declare (fixnum q k))
          (let ((result (loop :repeat fail-probability=1/4^
                              :always (probabilistic-prime-p q k))))
            (declare (boolean result))
            (values result
                    (if result
                        (/ 1 (the integer
                               (ash 1 (ash fail-probability=1/4^ 1))))
                        0))))))))

(defun %maybe-prime-p-integer (n fail-probability=1/4^)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (integer fixnum) (values boolean rational))
                  %maybe-prime-p-integer)
           (integer n) (fixnum fail-probability=1/4^))
  (let ((n-1 (the integer (1- n))))
    (declare (integer n n-1) (fixnum fail-probability=1/4^))
    (flet ((probabilistic-prime-p (q k)
             (declare (optimize (speed 3) (debug 0) (safety 0))
                      (ftype (function (integer fixnum) (values boolean rational)))
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
      (unless (evenp n)
        ;; find q,k s.t. 1 + q*2^k = n.
        (multiple-value-bind (q k)
            (loop :with w :of-type integer := n-1
                  :for v :of-type integer :from 0
                  :while (evenp w)
                  :do (setf w (the integer (ash w -1)))
                  :finally (return (values w v)))
          (declare (integer q k))
          (let ((result (loop :repeat fail-probability=1/4^
                              :always (probabilistic-prime-p q k))))
            (declare (boolean result))
            (values result
                    (if result
                        (/ 1 (the integer
                               (ash 1 (ash fail-probability=1/4^ 1))))
                        0))))))))


;; (defun compare-primality-test (num-times)
;;   (let ((random-integers (loop :repeat num-times
;;                                :collect (random most-positive-fixnum))))
;;     (equal (time (mapcar #'prime-p random-integers))
;;            (time (mapcar #'maybe-prime-p random-integers)))))



;;--------------------------------------------------------------------
;; PRIMES-BELOW integer => list-of-primes-below-the-integer
;;--------------------------------------------------------------------

;; !!! FIXME: below (* 2 most-positive-fixnum) !!!

;;--------------------------------------
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
e.g. (primes-below 13) => (2 3 5 7 11)
     (primes-below -1) => NIL"
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

;; !!! array dim limit !!!
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
;; FACTORIZE non-negative-integer => list-of-prime-factors
;;--------------------------------------------------------------------

;; !!! TODO !!!
;; quadratic-sieve
;; number-field-sieve
;; c.f. CINTA, pp.407-, 15.3 An algorithm for factoring


;;--------------------------------------
;; c.f. Project Euler, Problem 3, hk's overview.
;; Pre-optimized version:
;; (defun %factorize0 (n)
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

(defun factorize (n)
  "Return a sorted list of prime factors of N.
N must be a non-negative integer.
e.g. (factorize 12) => (2 2 3)
     (factorize 1) => NIL
     (factorize 0) => NIL
     (factorize -1) => ERROR!!"
  (unless (integerp n) (error "~S is not integer." n))
  (when (minusp n) (error "~S is not positive integer nor zero." n))
  (if (<= n 1)
      '()
      (if (<= n most-positive-fixnum)
          (%factorize-fixnum n)
          (%factorize-integer n))))

(defun %factorize-fixnum (n)
  "%FACTORIZE-FIXNUM is optimaized FACTORIZE for fixnum N."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (fixnum) list) %factorize-fixnum)
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

(defun %factorize-integer (n)
  "%FACTORIZE-INTEGER is optimaized FACTORIZE for integer N."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (integer) list) %factorize-integer)
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


;;---------------------------------
;; FACTORIZE1 (below) is less efficient than FACTORIZE0 (above).

;; see, TAOCP2, p.380, Algorithm A (Factoring by division).
;; (defun %factorize1 (n)
;;   (let ((primes (primes-below (1+ (isqrt n))))
;;         (factors '()))
;;     (loop :named outer :until (= n 1)
;;           :for p :in primes :do
;;        (LOOP (multiple-value-bind (q r) (floor n p)
;;                (if (zerop r)
;;                    (setf n q factors (cons p factors))
;;                    (if (< p q)
;;                        (RETURN)
;;                        (RETURN-FROM outer))))))
;;     (if (= 1 n)
;;         (nreverse factors)
;;         (nreverse (push n factors)))))

;; (defun test-compare-factorize (n)
;;   (let* ((random-fixnums
;;           (loop :repeat n :collect (random most-positive-fixnum)))
;;          (result (equal (time (mapcar #'factorize random-fixnums))
;;                         (time (mapcar #'factorize1 random-fixnums)))))
;;     (if result
;;         result
;;         (loop
;;            :for z :in random-fixnums
;;            :for x :in (mapcar #'factorize random-fixnums)
;;            :for y :in (mapcar #'factorize1 random-fixnums)
;;            :unless (equal x y) :collect (list z x y)))))


;; (defun %factorize1-fixnum (n)
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (ftype (function (fixnum) list) %factorize1-fixnum)
;;            (fixnum n))
;;   (let ((primes (the list (primes-below (the fixnum (1+ (isqrt n))))))
;;         (factors '()))
;;     (declare (list primes factors))
;;     (loop :named outer :until (= n 1)
;;           :for p fixnum :in primes :do
;;        (LOOP (multiple-value-bind (q r) (the (values fixnum fixnum)
;;                                           (floor n p))
;;                (declare (fixnum q r))
;;                (if (zerop r)
;;                    (setf n q factors (cons p factors))
;;                    (if (< p q)
;;                        (RETURN)
;;                        (RETURN-FROM outer))))))
;;     (if (= 1 n)
;;         (nreverse factors)
;;         (nreverse (push n factors)))))

;; (defun %factorize1-integer (n)
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (ftype (function (integer) list) %factorize1-integer)
;;            (integer n))
;;   (let ((primes (the list (primes-below (the integer (1+ (isqrt n))))))
;;         (factors '()))
;;     (declare (list primes factors))
;;     (loop :named outer :until (= n 1)
;;           :for p integer :in primes :do
;;        (LOOP (multiple-value-bind (q r) (the (values integer integer)
;;                                           (floor n p))
;;                (declare (integer q r))
;;                (if (zerop r)
;;                    (setf n q factors (cons p factors))
;;                    (if (< p q)
;;                        (RETURN)
;;                        (RETURN-FROM outer))))))
;;     (if (= 1 n)
;;         (nreverse factors)
;;         (nreverse (push n factors)))))

;; (defun factorize1 (n)
;;   (if (<= n 1)
;;       (if (zerop n) '() '(1))
;;       (if (<= n most-positive-fixnum)
;;           (%factorize1-fixnum n)
;;           (%factorize1-integer n))))


;;--------------------------------------
;; see, TAOCP2, p.387,
;;      Algorithm C (Factoring by addition and subtraction).
;; (defun %factorize3 (n)
;;   "Return (values P Q) s.t. P*Q=N & P<=root(N)<=Q.
;; N must be odd integer."
;;   (when (oddp n)
;;     (loop
;;        :with a := (1+ (* 2 (isqrt n)))
;;        :with b := 1
;;        :with r := (- (expt (isqrt n) 2) n)
;;        :until (zerop r)
;;        :do (setf r (+ r a) a (+ a 2))
;;            (loop :while (plusp r) :do (setf r (- r b) b (+ b 2)))
;;        :finally (return (values (/ (- a b) 2)
;;                                 (/ (- (+ a b) 2) 2))))))


;;--------------------------------------
;; see, TAOCP2, p.397, Algorithm E (Factoring via continued fractions).
;; (defun factorize5 (n)
;;   (let ((d 



;;--------------------------------------------------------------------
;; MAYBE-FACTORIZE non-negative-integer num-iterations
;;   => list-of-prime-factors fail-probability 
;;--------------------------------------------------------------------
;; !!TODO!!
;; Montgomery Method, c.f. TAOCP2, Exercise 4.3.1-41.

;; !!TODO!!
;; Since function gcd has &rest parameters???, it is less efficient.
;; 2 times faster,  after gcd -> xgcd.
;; think xgcd -> bgcd or lgcd
;; c.f. TAOCP2, p.338, Algorithm B (Binary gcd algorithm).
;; c.f. TAOCP2, p.347,
;;      Algorithm L (Euclid's algorithm for large numbers).


;; (maybe-factorize 25) => NIL, 1
;; (maybe-factorize 2197) => NIL, 1

;;--------------------------------------
;; c.f, TAOCP2, p.385, Algorithm B (Factoring by the rho method).

;; Pre-optimized version:
;; (defun %maybe-factorize (n &optional (fail-probability=1/4^ 40))
;;   (let ((factors nil))
;;     (LOOP (multiple-value-bind (q r) (floor n 2)
;;             (if (zerop r)
;;                 (setf n q factors (cons 2 factors))
;;                 (RETURN))))
;;     (if (< n 2)
;;         (values factors 0)
;;         (let ((x0 5) (x1 2) (k 1) (j 1))                           ;B1
;;           (loop :until (maybe-prime-p n fail-probability=1/4^) :do ;B2
;;              (loop :for g := (gcd (- x1 x0) n) :do                 ;B3
;;                 (cond ((= g 1) (decf k)                            ;B4
;;                                (when (zerop k)
;;                                  (setf x1 x0 j (* 2 j) k j))
;;                                (setf x0 (mod (1+ (expt x0 2)) n))) ;->B3
;;                       ((= g n) (RETURN-FROM %maybe-factorize
;;                                  (values nil 1)))                  ;->FAIL
;;                       (t       (setf factors (cons g factors)
;;                                      n (floor n g)
;;                                      x0 (mod x0 n)
;;                                      x1 (mod x1 n))
;;                                (return))))                         ;->B2
;;              :finally (push n factors))
;;           (values (nreverse factors)
;;                   (/ 1 (expt 4 fail-probability=1/4^)))))))


(defun maybe-factorize (n &optional (fail-probability=1/4^ 40))
  "Return (values PRIME-FACTORS FAIL-PROBABILITY).
PRIME-FACTORS is sorted list of prime factors of N.
FAIL-PROBABILITY is probability that PRIME-FACTORS is worng.
N must be a non-negative integer.
FAIL-PROBABILITY=1/4^ must be a positive fixnum.
There is a chance that it will FAIL.
N.B.
  If N is less than most-positive-fixnum, use function FACTORIZE instead.
  MAYBE-FACTORIZE is sutable for a large N.
e.g. (maybe-factorize 35262714657262341)
        => (3 47 250090174874201), 1/1208925819614629174706176
     (maybe-factorize 25)  => NIL, 1   ;<- FAIL
     (maybe-factorize 1)   => NIL, 0
     (maybe-factorize 0)   => NIL, 0
     (maybe-factorize -53) => ERROR!"
  (unless (integerp n) (error "~S is not integer." n))
  (when (minusp n) (error "~S is not positive integer or zero." n))
  (unless (and (integerp fail-probability=1/4^)
               (<= 1 fail-probability=1/4^ most-positive-fixnum))
    (error "~S is not positive fixnum." fail-probability=1/4^))
  (if (<= n 1)
      (values '() 0)
      (%maybe-factorize-integer n fail-probability=1/4^)))

(defun %maybe-factorize-integer (n fail-probability=1/4^)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (integer fixnum) list) maybe-factorize)
           (integer n) (fixnum fail-probability=1/4^))
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
                         (maybe-prime-p n fail-probability=1/4^)) :do
             (loop :for g :of-type integer
                   := (the integer
                        (xgcd (the integer (- x1 x0)) n)) :do
                (cond ((= g 1) (the integer (decf k))
                               (when (zerop k)
                                 (setf x1 x0 j (the integer
                                                 (ash j 1)) k j))
                               (setf x0 (the integer
                                          (mod (the integer
                                                 (1+ (the integer
                                                       (expt x0 2))))
                                               n))))
                      ((= g n) (RETURN-FROM %maybe-factorize-integer
                                 (values nil 1)))
                      (t       (setf factors (the list (cons g factors))
                                     n (the (values integer integer)
                                         (floor n g))
                                     x0 (the integer (mod x0 n))
                                     x1 (the integer (mod x1 n)))
                               (return))))
             :finally (push n factors))
          (values (sort (nreverse factors) #'<)
                  (/ 1 (the integer
                         (ash 1
                              (the integer
                                (ash fail-probability=1/4^ 1))))))))))



;; (defun test-maybe-factorize (n)
;;   (let* ((random-fixnums
;;           (loop :repeat n :collect (random most-positive-fixnum)))
;;          (result (equal (time (mapcar #'maybe-factorize random-fixnums))
;;                         (time (mapcar #'factorize random-fixnums)))))
;;     (if result
;;         result
;;         (loop
;;            :for z :in random-fixnums
;;            :for x :in (mapcar #'maybe-factorize random-fixnums)
;;            :for y :in (mapcar #'factorize random-fixnums)
;;            :unless (equal x y) :collect (list z x y)))))



;;--------------------------------------------------------------------
;; NEXT-PRIME integer => prime
;;--------------------------------------------------------------------

;;--------------------------------------
;; Pre-optimized version:
;; (defun %next-prime (n &optional (fail-probability=1/4^ 40))
;;   (if (<= n most-positive-fixnum)
;;       (loop :for i :from (if (evenp n) (+ n 1) (+ n 2)) :by 2
;;             :when (prime-p i) :return (values i 0))
;;       (loop :for i :from (if (evenp n) (+ n 1) (+ n 2)) :by 2
;;             :do (multiple-value-bind (result fail-prob)
;;                     (maybe-prime-p i fail-probability=1/4^)
;;                   (when result
;;                     (return (values i fail-prob)))))))

(defun next-prime (n &optional (fail-probability=1/4^ 40))
  "Return (valuse PRIME FAIL-PROBABILITY).
PRIME is prime next N s.t. N<PRIME & forall p in Prime[N<p => PRIME<=p].
PRIME may be non-prime number at most FAIL-PROBABILITY.
N must be integer.
e.g. (next-prime 12) => 13, 0
     (next-prime 13) => 17, 0
     (next-prime -19) => 2, 0
     (next-prime most-positive-fixnum)
        => 536870923, 1/1208925819614629174706176"
  (unless (integerp n) (error "~S is not integer." n))
  (when (<= n 1) (RETURN-FROM next-prime (values 2 0)))
  (if (< n #.(loop :for i :downfrom most-positive-fixnum
                   :when (prime-p i) :return i))
      (%next-prime-fixnum n)
      (%next-prime-integer n fail-probability=1/4^)))

(defun %next-prime-fixnum (n)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (fixnum) (values fixnum fixnum))
                  %next-prime-fixnum)
           (fixnum n))
  (loop :for i fixnum :from (the fixnum (+ n (if (evenp n) 1 2))) :by 2
        :when (prime-p i)
        :return (the (values fixnum fixnum) (values i 0))))

(defun %next-prime-integer (n &optional (fail-probability=1/4^ 40))
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (integer) (values integer rational))
                  %next-prime-integer)
           (integer n))
  (loop :for i integer :from (the integer (+ n (if (evenp n) 1 2)))
                       :by 2
        :do (multiple-value-bind (result fail-prob)
                (maybe-prime-p i fail-probability=1/4^)
              (declare (boolean result) (rational fail-prob))
              (when result
                (RETURN
                  (the (values integer rational)
                    (values i fail-prob)))))))




;;--------------------------------------------------------------------
;; RANDOM-PRIME
;;   max-num-bits num-iterations => maybe-prime fail-probability
;;--------------------------------------------------------------------
;; c.f. CINTA, pp.292-, 9.4 Generating a random prime
;;             pp.319-, 10.3 Generating random primes using the Miller–Rabin test

;; N.B.
;;  (integer-length maybe-prime) <= max-num-bits.
;;  If (integer-length maybe-prime) = max-num-bits,
;;  then output results are not uniform distribution
;;  (since (logbitp (1- max-num-bits) maybe-prime) => 1).

;; c.f. CINTA, pp.287-, 9.3 The generate and test paradigm.
;; analysis for
;;  1.ensure halting, 2.expected running time, 3.output distribution.


(defun random-prime (max-num-bits &optional (fail-probability=1/4^ 40))
  "Return (values PRIME FAIL-PROBABILITY).
PRIME is a prime at most MAX-NUM-BITS bits.
PRIME may be non-prime at most FAIL-PROBABILITY.
MAX-NUM-BITS must be a positive fixnum above 1."
  (unless (integerp max-num-bits)
    (error "~S is not integer." max-num-bits))
  (unless (<= 2 max-num-bits most-positive-fixnum)
    (error "~S is not positive fixnum above 1." max-num-bits))
  (labels ((rec (max-num-bits)
             (declare (optimize (speed 3) (debug 0) (safety 0))
                      (ftype (function (fixnum) integer) rec)
                      (fixnum max-num-bits))
             (let ((random-integer
                    (loop :for i fixnum :from 0 :below max-num-bits
                          :sum (ash (random 2) i) integer)))
               (declare (integer random-integer))
               (if (prime-p random-integer fail-probability=1/4^)
                   (values random-integer
                           (/ 1 (ash 1 (ash fail-probability=1/4^ 1))))
                   (rec max-num-bits)))))
    (rec max-num-bits)))


;;====================================================================
;; Miscellaneous
;;====================================================================
;; Z*
;; ord    <- not yet
;; xgcd
;; phi

;;--------------------------------------------------------------------
;; Z*
;;--------------------------------------------------------------------

(defun Z* (n)
  "Return set of invertible element below n."
  (loop :for i :from 1 :below n :when (= 1 (gcd i n)) :collect i))

;; (defun Z* (n)
;;   (if (prime-p n)
;;       (alexandria:iota n)
;;       (loop :for i :from 1 :below n :when (= 1 (gcd i n)) :collect i)))



;;--------------------------------------------------------------------
;; GCD2 x y => gcd(x,y)
;;--------------------------------------------------------------------

;; (defun gcd2 (x y)
;;   (if (or (< most-positive-fixnum x)
;;           (< most-positive-fixnum y))
;;       (%gcd2-integers x y)
;;       (%gcd2-fixnums x y)))

;; (defun %gcd2-integers (x y)
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (ftype (function (integer integer) integer))
;;            (integer x y))
;;   (loop :for u integer := (abs x) :then v
;;         :and v integer := (abs y) :then (mod u v)
;;         :until (zerop v)
;;         :finally (return u)))

;; (defun %gcd2-fixnums (x y)
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (ftype (function (fixnum fixnum) fixnum))
;;            (fixnum x y))
;;   (loop :for u fixnum := (abs x) :then v
;;         :and v fixnum := (abs y) :then (mod u v)
;;         :until (zerop v)
;;         :finally (return u)))

;; (defun test-gcd2-1 (n)
;;   (loop :repeat n
;;         :for a := (random most-positive-fixnum)
;;         :for b := (random most-positive-fixnum)
;;         :always (= (gcd a b) (gcd2 a b))))

;; (defun test-gcd2-2 (n)
;;   (let ((random1
;;          (loop :repeat n :collect (random most-positive-fixnum)))
;;         (random2
;;          (loop :repeat n :collect (random most-positive-fixnum))))
;;     (equal (time (mapcar #'gcd2 random1 random2))
;;            (time (mapcar #'gcd random1 random2)))))


;;--------------------------------------------------------------------
;; BGCD x y => gcd(x,y)
;;--------------------------------------------------------------------
;; ??? Since CL function GCD has &rest lambda-parameter, it is less
;; efficient when we need the value of gcd(x,y) that has only 2
;; arguments. ???

;;--------------------------------------
;; (defun test-bgcd (n limit)
;;   (let ((randoms1
;;          (loop :repeat n :collect (random limit)))
;;         (randoms2
;;          (loop :repeat n :collect (random limit))))
;;     (equal (time (mapcar #'gcd randoms1 randoms2))
;;            (time (mapcar #'bgcd randoms1 randoms2)))))

;; CL-USER> (test-bgcd 1000000 most-positive-fixnum)
;; Evaluation took:
;;   0.411 seconds of real time
;;   0.412026 seconds of total run time (0.412026 user, 0.000000 system)
;;   100.24% CPU
;;   711,994,933 processor cycles
;;   8,003,584 bytes consed
  
;; Evaluation took:
;;   0.473 seconds of real time
;;   0.436027 seconds of total run time (0.436027 user, 0.000000 system)
;;   92.18% CPU
;;   820,747,270 processor cycles
;;   8,003,544 bytes consed
  
;; T

;; CL-USER> (test-bgcd 100000 (expt most-positive-fixnum 2))
;; Evaluation took:
;;   0.986 seconds of real time
;;   0.972061 seconds of total run time (0.968061 user, 0.004000 system)
;;   [ Run times consist of 0.044 seconds GC time, and 0.929 seconds non-GC time. ]
;;   98.58% CPU
;;   1,710,017,403 processor cycles
;;   29,600,296 bytes consed
  
;; Evaluation took:
;;   1.580 seconds of real time
;;   1.532096 seconds of total run time (1.476092 user, 0.056004 system)
;;   [ Run times consist of 0.132 seconds GC time, and 1.401 seconds non-GC time. ]
;;   96.96% CPU
;;   2,737,580,206 processor cycles
;;   223,384,496 bytes consed
  
;; T

;;--------------------------------------
;; c.f. CINTA, p.77, Exercise 4.6

;; Pre-optimized version:
;; (defun bgcd (a b)
;;   (let ((e 0))
;;     (loop :while (and (evenp a) (evenp b))
;;           :do (setf a (ash a -1) b (ash b -1) e (1+ e)))
;;     (LOOP
;;        (loop :while (evenp a) :do (setf a (ash a -1)))
;;        (loop :while (evenp b) :do (setf b (ash b -1)))
;;        (when (< b a) (rotatef a b))
;;        (decf b a)
;;        (when (zerop b) (RETURN)))
;;     (ash a e)))

;; (defun bgcd (a b)
;;   (if (and (<= a most-positive-fixnum)
;;            (<= b most-positive-fixnum))
;;       (%bgcd-fixnums a b)
;;       (%bgcd-integers a b)))

;; (defun %bgcd-fixnums (a b)
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (ftype (function (fixnum fixnum) fixnum) %bgcd-fixnums)
;;            (fixnum a b))
;;   (let ((e 0))
;;     (declare (fixnum e))
;;     (loop :while (and (evenp a) (evenp b))
;;           :do (setf a (ash a -1) b (ash b -1) e (1+ e)))
;;     (LOOP
;;        (loop :while (evenp a) :do (setf a (ash a -1)))
;;        (loop :while (evenp b) :do (setf b (ash b -1)))
;;        (when (< b a) (rotatef a b))
;;        (decf b a)
;;        (when (zerop b) (RETURN)))
;;     (the fixnum (ash a e))))

;; (defun %bgcd-integers (a b)
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (ftype (function (integer integer) integer) %bgcd-integers)
;;            (integer a b))
;;   (let ((e 0))
;;     (declare (integer e))
;;     (loop :while (and (evenp a) (evenp b))
;;           :do (setf a (ash a -1) b (ash b -1) e (1+ e)))
;;     (LOOP
;;        (loop :while (evenp a) :do (setf a (ash a -1)))
;;        (loop :while (evenp b) :do (setf b (ash b -1)))
;;        (when (< b a) (rotatef a b))
;;        (decf b a)
;;        (when (zerop b) (RETURN)))
;;     (the integer (ash a e))))



;;--------------------------------------
;; c.f. TAOCP2, p.338, Algorithm B (Binary gcd algorithm)

;; Pre-optimized version:
;; (defun bgcd (x y)
;;   (let ((k 0) (n))                         ;B1
;;     (loop :until (or (oddp x) (oddp y)) :do
;;        (setf k (1+ k) x (ash x -1) y (ash y -1)))
;;     (tagbody
;;        (if (oddp x)                        ;B2
;;            (progn
;;              (setf n (- y))                ;-> B4
;;              (go :B4))
;;            (setf n x))
;;      :B3
;;        (setf n (ash n -1))                 ;B3
;;      :B4
;;        (when (evenp n) (go :B3))           ;B4, ->B3
;;        (if (plusp n)                       ;B5
;;            (setf x n)
;;            (setf y (- n)))
;;        (setf n (- x y))                    ;B6
;;        (if (/= 0 n)
;;            (go :B3)                        ;->B3
;;            (return-from bgcd (* x (ash 1 k))))))) ;-> end


;; (defun %bgcd-fixnums (x y)
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (ftype (function (fixnum fixnum) fixnum) bgcd)
;;            (fixnum x y))
;;   (let ((k 0) (n 0))                         ;B1
;;     (declare (fixnum k) (fixnum n))
;;     (loop :until (or (oddp x) (oddp y)) :do
;;        (setf k (1+ k) x (ash x -1) y (ash y -1)))
;;     (tagbody
;;        (if (oddp x)                        ;B2
;;            (progn
;;              (setf n (- y))                ;-> B4
;;              (go :B4))
;;            (setf n x))
;;      :B3
;;        (setf n (ash n -1))                 ;B3
;;      :B4
;;        (when (evenp n) (go :B3))           ;B4, ->B3
;;        (if (plusp n)                       ;B5
;;            (setf x n)
;;            (setf y (- n)))
;;        (setf n (- x y))                    ;B6
;;        (if (/= 0 n)
;;            (go :B3)                        ;->B3
;;            (return-from bgcd (* x (ash 1 k)))))))


;;--------------------------------------------------------------------
;; LGCD x y => n
;;--------------------------------------------------------------------
;; c.f. TAOCP2, p.347,
;;      Algorithm L (Euclid's algorithm for large numbers).

;; (defun lgcd (x y)
  
  
;;--------------------------------------------------------------------
;; XGCD x y => c, a, b,  s.t a*x + b*y = c = gcd(x,y)
;;--------------------------------------------------------------------
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

(defun xgcd (x y)
"Return (values U0 U1 U2), s.t. U1*X + U2*Y = U0 = gcd(X,Y).
X,Y must be integers."
  (unless (integerp x) (error "~S is not integer." x))
  (unless (integerp y) (error "~S is not integer." y))
  (if (and (<= x most-positive-fixnum) (<= y most-positive-fixnum))
      (%xgcd-fixnums x y)
      (%xgcd-integers x y)))

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

;;--------------------------------------
;; (defun test-xgcd (n limit)
;;   (let* ((randoms1 (loop :repeat n :collect (random limit)))
;;          (randoms2 (loop :repeat n :collect (random limit)))
;;          (xgcd-result (time (mapcar #'xgcd randoms1 randoms2)))
;;          (gcd-result  (time (mapcar #'gcd randoms1 randoms2))))
;;     (or (equal xgcd-result gcd-result)
;;         (loop
;;            :for r1 :in randoms1
;;            :for r2 :in randoms2
;;            :for x :in xgcd-result
;;            :for y :in gcd-result
;;            :unless (equal x y) :collect (list r1 r2 x y)))))

;; CL-USER> (test-xgcd 1000000 most-positive-fixnum)
;; Evaluation took:
;;   0.422 seconds of real time
;;   0.420026 seconds of total run time (0.420026 user, 0.000000 system)
;;   99.53% CPU
;;   731,137,295 processor cycles
;;   8,003,584 bytes consed
  
;; Evaluation took:
;;   0.436 seconds of real time
;;   0.408026 seconds of total run time (0.408026 user, 0.000000 system)
;;   93.58% CPU
;;   756,433,134 processor cycles
;;   8,007,712 bytes consed
  
;; T

;; CL-USER> (test-xgcd 100000 (expt most-positive-fixnum 2))
;; Evaluation took:
;;   1.804 seconds of real time
;;   1.776112 seconds of total run time (1.700107 user, 0.076005 system)
;;   [ Run times consist of 0.168 seconds GC time, and 1.609 seconds non-GC time. ]
;;   98.45% CPU
;;   3,126,131,557 processor cycles
;;   232,108,792 bytes consed
  
;; Evaluation took:
;;   0.978 seconds of real time
;;   0.944058 seconds of total run time (0.940058 user, 0.004000 system)
;;   [ Run times consist of 0.016 seconds GC time, and 0.929 seconds non-GC time. ]
;;   96.52% CPU
;;   1,695,814,680 processor cycles
;;   29,611,792 bytes consed
  
;; T


;;--------------------------------------------------------------------
;; PHI n => |{ i | 1<=i<n & gcd(n,i)=1 }|
;;--------------------------------------------------------------------
;; Euler's totient function phi(N).
;; see. 奥村晴彦, アルゴリズム辞典, p.342, 技術評論社, 1991
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

(defun phi (n)
  "Euler's totient function, phi(n) := |{ i | 1<=i<n & gcd(n,i)=1 }|.
Return a number of relatively prime to N from 1 below N.
N must be a integer."
  (unless (integerp n) (error "~S is not integer." n))
  (if (<= n 1)
      0
      (if (<= n most-positive-fixnum)
          (%euler-phi-fixnum n)
          (%euler-phi-integer n))))

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


;; (defun naive-euler-phi (n)
;;   (loop :for i :from 1 :below n :count (= 1 (gcd i n))))

;; (defun test-phi (n)
;;   (= (time (euler-phi n))
;;      (time (naive-euler-phi n))))

;; (defun test-phi2 (n)
;;   (let* ((randoms
;;           (loop :repeat n :collect (random 100)))
;;          (result (equal (time (mapcar #'euler-phi randoms))
;;                         (time (mapcar #'naive-euler-phi randoms)))))
;;     (if result
;;         result
;;         (loop
;;            :for i :in randoms
;;            :for x :in (mapcar #'euler-phi randoms)
;;            :for y :in (mapcar #'naive-euler-phi randoms)
;;            :unless (= x y) :collect (list i x y)))))



;; ;;--------------------------------------------------------------------
;; ;; FACTORIZE non-negative-integer => list-of-prime-factors
;; ;;--------------------------------------------------------------------

;; (defun factorize (n)
;;   "Return a sorted list of prime factors of N.
;; N must be a non-negative integer.
;; e.g. (factorize 12) => (2 2 3)
;;      (factorize 1) => NIL
;;      (factorize 0) => NIL
;;      (factorize -1) => ERROR!!"
;;   (unless (integerp n) (error "~S is not integer." n))
;;   (when (minusp n) (error "~S is not positive integer nor zero." n))
;;   (if (<= n 1)
;;       '()
;;       (if (<= n most-positive-fixnum)
;;           (%factorize-fixnum n)
;;           (%factorize-integer n))))

;; ;; c.f. Project Euler, Problem 3, hk's overview.
;; ;; Pre-optimized version:
;; ;; (defun %factorize0 (n)
;; ;;   (let ((factors '()))
;; ;;     (LOOP (multiple-value-bind (q r) (floor n 2)
;; ;;             (if (zerop r)
;; ;;                 (setf n q factors (cons 2 factors))
;; ;;                 (RETURN))))
;; ;;     (loop :with max-factor := (isqrt n)
;; ;;           :for factor :from 3 :by 2
;; ;;           :while (and (< 1 n) (<= factor max-factor))
;; ;;           :do (LOOP (multiple-value-bind (q r) (floor n factor)
;; ;;                       (if (zerop r)
;; ;;                           (setf n q factors (cons factor factors))
;; ;;                           (RETURN)))))
;; ;;     (if (= 1 n)
;; ;;         (nreverse factors)
;; ;;         (nreverse (push n factors)))))

;; (defun %factorize-fixnum (n)
;;   "%FACTORIZE-FIXNUM is optimaized FACTORIZE for fixnum N."
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (ftype (function (fixnum) list) %factorize-fixnum)
;;            (fixnum n))
;;   (let ((factors '()))
;;     (declare (list factors))
;;     (LOOP (multiple-value-bind (q r) (floor n 2)
;;             (declare (fixnum q r))
;;             (if (zerop r)
;;                 (setf n q factors (cons 2 factors))
;;                 (RETURN))))
;;     (loop :with max-factor fixnum := (isqrt n)
;;           :for factor fixnum :from 3 :by 2
;;           :while (and (< 1 n) (<= factor max-factor))
;;           :do (LOOP (multiple-value-bind (q r) (floor n factor)
;;                       (declare (fixnum q r))
;;                       (if (zerop r)
;;                           (setf n q factors (cons factor factors))
;;                           (RETURN)))))
;;     (if (= 1 n)
;;         (nreverse factors)
;;         (nreverse (push n factors)))))

;; (defun %factorize-integer (n)
;;   "%FACTORIZE-INTEGER is optimaized FACTORIZE for integer N."
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (ftype (function (integer) list) %factorize-integer)
;;            (integer n))
;;   (let ((factors '()))
;;     (declare (list factors))
;;     (LOOP (multiple-value-bind (q r) (floor n 2)
;;             (declare (integer q r))
;;             (if (zerop r)
;;                 (setf n q factors (cons 2 factors))
;;                 (RETURN))))
;;     (loop :with max-factor integer := (isqrt n)
;;           :for factor integer :from 3 :by 2
;;           :while (and (< 1 n) (<= factor max-factor))
;;           :do (LOOP (multiple-value-bind (q r) (floor n factor)
;;                       (declare (integer q r))
;;                       (if (zerop r)
;;                           (setf n q factors (cons factor factors))
;;                           (RETURN)))))
;;     (if (= 1 n)
;;         (nreverse factors)
;;         (nreverse (push n factors)))))


;; ;;--------------------------------------------------------------------
;; ;; MAYBE-FACTORIZE non-negative-integer num-iterations
;; ;;   => list-of-prime-factors fail-probability 
;; ;;--------------------------------------------------------------------
;; ;; c.f, TAOCP2, p.385, Algorithm B (Factoring by the rho method).
;; ;; Pre-optimized version:
;; ;; (defun %maybe-factorize (n &optional (fail-probability=1/4^ 40))
;; ;;   (let ((factors nil))
;; ;;     (LOOP (multiple-value-bind (q r) (floor n 2)
;; ;;             (if (zerop r)
;; ;;                 (setf n q factors (cons 2 factors))
;; ;;                 (RETURN))))
;; ;;     (if (< n 2)
;; ;;         (values factors 0)
;; ;;         (let ((x0 5) (x1 2) (k 1) (j 1))                           ;B1
;; ;;           (loop :until (prime-p n fail-probability=1/4^) :do ;B2
;; ;;              (loop :for g := (gcd (- x1 x0) n) :do                 ;B3
;; ;;                 (cond ((= g 1) (decf k)                            ;B4
;; ;;                                (when (zerop k)
;; ;;                                  (setf x1 x0 j (* 2 j) k j))
;; ;;                                (setf x0 (mod (1+ (expt x0 2)) n))) ;->B3
;; ;;                       ((= g n) (RETURN-FROM %maybe-factorize
;; ;;                                  (values nil 1)))                  ;->FAIL
;; ;;                       (t       (setf factors (cons g factors)
;; ;;                                      n (floor n g)
;; ;;                                      x0 (mod x0 n)
;; ;;                                      x1 (mod x1 n))
;; ;;                                (return))))                         ;->B2
;; ;;              :finally (push n factors))
;; ;;           (values (nreverse factors)
;; ;;                   (/ 1 (expt 4 fail-probability=1/4^)))))))

;; (defun maybe-factorize (n &optional (fail-probability=1/4^ 40))
;;   "Return (values PRIME-FACTORS FAIL-PROBABILITY).
;; PRIME-FACTORS is sorted list of prime factors of N.
;; FAIL-PROBABILITY is probability that PRIME-FACTORS is worng.
;; N must be a non-negative integer.
;; FAIL-PROBABILITY=1/4^ must be a positive fixnum.
;; There is a chance that it will FAIL.
;; N.B.
;;   If N is less than most-positive-fixnum, use function FACTORIZE instead.
;;   MAYBE-FACTORIZE is sutable for a large N.
;; e.g. (maybe-factorize 35262714657262341)
;;         => (3 47 250090174874201), 1/1208925819614629174706176
;;      (maybe-factorize 25)  => NIL, 1   ;<- FAIL
;;      (maybe-factorize 1)   => NIL, 0
;;      (maybe-factorize 0)   => NIL, 0
;;      (maybe-factorize -53) => ERROR!"
;;   (unless (integerp n) (error "~S is not integer." n))
;;   (when (minusp n) (error "~S is not positive integer or zero." n))
;;   (unless (and (integerp fail-probability=1/4^)
;;                (<= 1 fail-probability=1/4^ most-positive-fixnum))
;;     (error "~S is not positive fixnum." fail-probability=1/4^))
;;   (if (<= n 1)
;;       (values '() 0)
;;       (%maybe-factorize-integer n fail-probability=1/4^)))


;; (defun %maybe-factorize-integer (n fail-probability=1/4^)
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (ftype (function (integer fixnum) list) maybe-factorize)
;;            (integer n) (fixnum fail-probability=1/4^))
;;   (let ((factors nil))
;;     (declare (list factors))
;;     (LOOP (multiple-value-bind (q r) (the (values integer integer)
;;                                        (floor n 2))
;;             (declare (integer q r))
;;             (if (zerop r)
;;                 (setf n q factors (the list (cons 2 factors)))
;;                 (RETURN))))
;;     (if (< n 2)
;;         (values factors 0)
;;         (let ((x0 5) (x1 2) (k 1) (j 1))
;;           (declare (integer x0 x1 k j))
;;           (loop :until (the (values boolean rational)
;;                          (prime-p n fail-probability=1/4^)) :do
;;              (loop :for g :of-type integer
;;                    := (the integer
;;                         (xgcd (the integer (- x1 x0)) n)) :do
;;                 (cond ((= g 1) (the integer (decf k))
;;                                (when (zerop k)
;;                                  (setf x1 x0 j (the integer
;;                                                  (ash j 1)) k j))
;;                                (setf x0 (the integer
;;                                           (mod (the integer
;;                                                  (1+ (the integer
;;                                                        (expt x0 2))))
;;                                                n))))
;;                       ((= g n) (RETURN-FROM %maybe-factorize-integer
;;                                  (values nil 1)))
;;                       (t       (setf factors (the list (cons g factors))
;;                                      n (the (values integer integer)
;;                                          (floor n g))
;;                                      x0 (the integer (mod x0 n))
;;                                      x1 (the integer (mod x1 n)))
;;                                (return))))
;;              :finally (push n factors))
;;           (values (sort (nreverse factors) #'<)
;;                   (/ 1 (the integer
;;                          (ash 1
;;                               (the integer
;;                                 (ash fail-probability=1/4^ 1))))))))))


;; (defun %d-prime-p-fixnum (n)
;;   "%D-PRIME-P-FIXNUM is optimaized %D-PRIME-P for fixnum N."
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (ftype (function (fixnum) boolean) %d-prime-p-fixnum)
;;            (fixnum n))
;;   (loop :for factor fixnum :from 5 :by 6
;;         :with root-n fixnum := (isqrt n) ;root-n^2 <= n < (root-n + 1)^2
;;         :while (<= factor root-n)
;;         :never (or (zerop (mod n factor))
;;                    (zerop (mod n (+ factor 2))))))

;; (defun %d-prime-p-integer (n)
;;   "%D-PRIME-P-INTEGER is optimaized %D-PRIME-P for integer N."
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (ftype (function (integer) boolean) %d-prime-p-integer)
;;            (integer n))
;;   (loop :for factor integer :from 5 :by 6
;;         :with root-n integer := (isqrt n) ;root-n^2 <= n < (root-n + 1)^2
;;         :while (<= factor root-n)
;;         :never (or (zerop (mod n factor))
;;                    (zerop (mod n (+ factor 2))))))


```lisp
CL-REPL> (use-package :mod-prime)    ;mod-prime is nickname for cl-mod-prime

=> T

;---------------------------------------
CL-REPL> (defun compare-primality-test (num-times)
           (let ((random-integers (loop :repeat num-times
                                        :collect (random most-positive-fixnum))))
             (equal (time (mapcar #'prime-p random-integers))
                    (time (mapcar #'maybe-prime-p random-integers)))))

=> COMPARE-PRIMALITY-TEST

CL-REPL> (compare-primality-test 100000)

Evaluation took:               ;prime-p is more efficient for fixnums
  0.700 seconds of real time
  0.672042 seconds of total run time (0.672042 user, 0.000000 system)
  96.00% CPU
  1,214,435,926 processor cycles
  798,720 bytes consed

Evaluation took:               ;maybe-prime-p is less efficient for fixnums
  2.984 seconds of real time
  2.872179 seconds of total run time (2.688168 user, 0.184011 system)
  [ Run times consist of 0.784 seconds GC time, and 2.089 seconds non-GC time. ]
  96.25% CPU
  5,173,017,620 processor cycles
  490,652,968 bytes consed

=> T

CL-REPL> (defparameter *large-prime* 13407807929942597099574024998205846127479365820592393377723561443721764030073662768891111614362326998675040546094339320838419523375986027530441562135724301)

CL-REPL> (time (prime-p *large-prime*))

Evaluation took:               ;don't use prime-p for large integers
  36.167 seconds of real time
; Evaluation aborted

CL-REPL> (time (maybe-prime-p *large-prime*))

Evaluation took:               ;maybe-prime-p is efficient for large integers
  0.167 seconds of real time
  0.136008 seconds of total run time (0.136008 user, 0.000000 system)
  81.44% CPU
  290,751,175 processor cycles
  11,551,480 bytes consed

=> T
=> 1/1208925819614629174706176  ;maximum probability that 1st return value is wrong, = 1/4^40

CL-REPL> (time (maybe-prime-p *large-prime* 80)) ;80 internal iterations (default is 40 iterations)
                                                 ;fail-probability is multiplied by 1/4 per 1 iteration.

Evaluation took:
  0.305 seconds of real time
  0.280017 seconds of total run time (0.280017 user, 0.000000 system)
  91.80% CPU
  528,424,374 processor cycles
  23,120,432 bytes consed

=> T
=> 1/1461501637330902918203684832716283019655932542976  ;fail-probability = 1/4^80

;---------------------------------------
CL-REPL> (next-prime 13)

=> 17     ;prime next to 13
=> 0      ;fail-probability that 17 is not prime

CL-REPL> (time (next-prime *large-prime*))

Evaluation took:
  0.297 seconds of real time
  0.272017 seconds of total run time (0.272017 user, 0.000000 system)
  [ Run times consist of 0.008 seconds GC time, and 0.265 seconds non-GC time. ]
  91.58% CPU
  513,669,809 processor cycles
  20,098,064 bytes consed

=> 13407807929942597099574024998205846127479365820592393377723561443721764030073662768891111614362326998675040546094339320838419523375986027530441562135724377
=> 1/1208925819614629174706176    ;fail-probability that 1st return value is not prime

;---------------------------------------
CL-REPL> (time (random-maybe-prime 1024 50)) ;generate at most 1024 bits random maybe-prime
                                             ;2nd arg, 50 is number of internal iterations.
                                             ;with my environment, evaluation took about 3~20 secs
Evaluation took:
  11.386 seconds of real time
  11.132695 seconds of total run time (11.120695 user, 0.012000 system)
  [ Run times consist of 0.200 seconds GC time, and 10.933 seconds non-GC time. ]
  97.78% CPU
  19,736,151,953 processor cycles
  597,565,464 bytes consed

=> 108945840135923742130564083505526445433140176426330941961551653846616782288765158381686620440681901417758215096268693605750947179505428605975829268224930543719246712562743122437036113914838807622641095842485509851410861130502605547616368106242259980233843039792210190942122963975286403230380867396055994617213
=> 1/1267650600228229401496703205376   ;maximum probability that 1st return value is not prime, = 1/4^50

;---------------------------------------
CL-REPL> (factorize 1764)

=> (2 2 3 3 7 7)

CL-REPL> (time (factorize 35262714657262341))

Evaluation took:
  21.423 seconds of real time
  20.493281 seconds of total run time (20.369273 user, 0.124008 system)
  [ Run times consist of 1.120 seconds GC time, and 19.374 seconds non-GC time. ]
  95.66% CPU
  37,133,964,022 processor cycles
  3,004,551,168 bytes consed

=> (3 47 250090174874201)

CL-REPL> (time (maybe-factorize 35262714657262341))

Evaluation took:
  0.004 seconds of real time
  0.004000 seconds of total run time (0.004000 user, 0.000000 system)
  100.00% CPU
  6,297,569 processor cycles
  474,384 bytes consed

=> (3 47 250090174874201)
=> 1/1208925819614629174706176

;---------------------------------------
CL-REPL> (primes-below 13)

=> (2 3 5 7 11)

CL-REPL> (length (time (primes-below most-positive-fixnum)))

Evaluation took:
  30.852 seconds of real time
  30.377899 seconds of total run time (29.437840 user, 0.940059 system)
  [ Run times consist of 2.981 seconds GC time, and 27.397 seconds non-GC time. ]
  98.46% CPU
  53,474,013,006 processor cycles
  259,300,864 bytes consed

=> 28192750

;---------------------------------------
CL-REPL> (time (mod (expt *large-prime* *large-prime*) (1+ *large-prime*)))

Evaluation took:
  25.433 seconds of real time
; Evaluation aborted

CL-REPL> (time (mod-expt *large-prime* *large-prime* (1+ *large-prime*)))

Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  328,451 processor cycles
  36,760 bytes consed

=> 13407807929942597099574024998205846127479365820592393377723561443721764030073662768891111614362326998675040546094339320838419523375986027530441562135724301

;---------------------------------------
CL-REPL> (mod-inv 10 13)
=> 4    ;10*4 = 1 mod 13

;---------------------------------------
CL-REPL> (mod-sqrt 11 42)

quadratic-residue-p

CL-REPL> (mod-sqrt 11 42)


;---------------------------------------
CL-REPL> (xgcd 42 13)

=> 1
=> -4
=> 13    ; -4*42 + 13*13 = 1 = gcd(42,13)
```

;;====================================================================
