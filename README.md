Last modified : 2013-02-01 21:32:34 tkych

Version: 0.0.10 (Alpha: Under Development)


CL-MOD-PRIME: Modular Arithmetic and Primes Library for CL
==========================================================


Introduction
------------

CL-MOD-PRIME is a library of number theory for CL.
The number theory (which is the queen of mathematics, including modular arithmetic and theorems about primes) is important for modern cryptography.
For example, the theoretical basis of a public key cryptosystem (such as RSA based on trapdoor functions, or such as ElGamal based on Diffie-Hellman protocol) is the computational complexity of the prime factorization or the discrete logarithm (a sort of modulo logarithm).

The goal of this library is being fast for ordinary use (e.g. solving a problem in Project Euler, etc.).

I examined several algorithms that is suitable for the goal.
But there may be a better algorithm that I don't know.
I would appreciate if you tell me that better algorithm.
And I welcome bug reports, typoes, suggestions, optimizations, patches, benchmarks, whatever.


Download & Install
------------------

0. SHELL$   `git clone https://github.com/tkych/cl-mod-prime.git`
1. CL-REPL> `(push #p"/path-to-cl-mod-prime/cl-mod-prime/" asdf:*central-registry*)`
2. CL-REPL> `(ql:quickload :cl-mod-prime)`


Examples
--------

* My environment is: Linux-amd64, Core i3 2.20GHz, 4GB RAM, SBCL-1.3.3.

```lisp
CL-REPL> (use-package :mod-prime)    ;'mod-prime' is nickname for cl-mod-prime
=> T

CL-REPL> (factorize 42000000)
=> (2 2 2 2 2 2 2 3 5 5 5 5 5 5 7)   ;prime-factors
=> 0                                 ;fail probability
=> :SIMPLE-DIVISION                  ;algorithm

CL-REPL> (factorize 42000000 :group? t)
=> ((2 . 7) (3 . 1) (5 . 6) (7 . 1)) ;grouped-prime-factors
=> 0
=> :SIMPLE-DIVISION

CL-REPL> (factorize 42424242424242424242424242)
=> (2 3 7 53 79 859 265371653 1058313049)
=> 1/1267650600228229401496703205376
=> :RHO-METHOD

CL-REPL> (time (random-prime 1024))
Evaluation took:
  0.448 seconds of real time
  0.444027 seconds of total run time (0.444027 user, 0.000000 system)
  [ Run times consist of 0.012 seconds GC time, and 0.433 seconds non-GC time. ]
  99.11% CPU
  981,879,747 processor cycles
  151,722,288 bytes consed
  
=> 138887118337037179373987270174393996261443440883200142824729530101304676899255530725540449733203724419224498902439773767074362319130643291726464898871930836869876144519772157445996871560722567192534102362410201440406406553635556576910252943677026926228532581307334590673298564303609464269330957587451305799483
=> 1/1267650600228229401496703205376  ;fail probability

CL-REPL> (defparameter *large-prime* *)
=> *LARGE-PRIME*

CL-REPL> (time (mod (expt *large-prime* *large-prime*) (1- *large-prime*))) ;Don't type this!
Evaluation took:
  13.858 seconds of real time
  ...
  before it was aborted by a non-local transfer of control.
  
; Evaluation aborted on NIL.

CL-REPL> (time (mod-expt *large-prime* *large-prime* (1- *large-prime*)))
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  417,556 processor cycles
  32,768 bytes consed

=> 1

CL-REPL> (time (next-prime *large-prime*))
Evaluation took:
  3.814 seconds of real time
  3.808237 seconds of total run time (3.796237 user, 0.012000 system)
  [ Run times consist of 0.112 seconds GC time, and 3.697 seconds non-GC time. ]
  99.84% CPU
  8,372,238,984 processor cycles
  1,196,300,464 bytes consed
  
=> 138887118337037179373987270174393996261443440883200142824729530101304676899255530725540449733203724419224498902439773767074362319130643291726464898871930836869876144519772157445996871560722567192534102362410201440406406553635556576910252943677026926228532581307334590673298564303609464269330957587451305801701
=> 1/1267650600228229401496703205376

CL-REPL> (primes-below 13)
=> (2 3 5 7 11)

CL-REPL> (time (length (primes-below 500000000))) ;Don't type this without 'length'!
Evaluation took:
  5.161 seconds of real time
  5.152322 seconds of total run time (4.916307 user, 0.236015 system)
  [ Run times consist of 1.180 seconds GC time, and 3.973 seconds non-GC time. ]
  99.83% CPU
  11,324,355,662 processor cycles
  452,947,296 bytes consed
  
=> 26355867
```


Referece Manual
---------------

### Modular Arithmetic

* mod-mult
* mod-expt
* mod-inv
* mod-sqrt      <- under implementation
* mod-nth-root  <- under implementation
* dlog          <- under implementation


#### [Function] MOD-MULT &rest integers-divisor

Return multiple of INTEGERS modulo DIVISOR.


#### [Function] MOD-EXPT base power divisor

Return BASE raised to the POWER modulo DIVISOR.
BASE, POWER must be a non-negative intgers.
DIVISOR must be a positive integer.

Examples:

    (mod-expt 986 33 42) => 20
    (mod-expt 986 33 -1) => ERROR!!
    (mod-expt 986 -23 1) => ERROR!!


#### [Function] MOD-INV n divisor

Return inverse N mod DIVISOR if it exists.
N and DIVISOR must be intgers.

Examples:

    (mod-inv 7 13) => 2   ;c.f. (mod (* 2 7) 13) => 1
    (mod-inv 2 4)  => NIL


#### [Function] MOD-SQRT

Under Implementation


#### [Function] MOD-NTH-ROOT

Under Implementation


#### [Function] DLOG

Under Implementation


### Primes

* prime-p
* \*prime-p-swiching-limit\*
* \*prime-p-fail-probability=1/4^\*
* next-prime
* random-prime
* primes-below
* factorize
* \*factorize-swiching-limit\*
* random-factored


#### [Function] PRIME-P n

Return (values RESULT FAIL-PROBABILITY).
If N is a prime, then RESULT is always T.
Otherwise, if N is composite, then RESULT may be T or NIL.
At most FAIL-PROBABILITY, RESULT may be wrong (that is, N is composite and RESULT is T).
N must be a integer.

Note:

* If N is below \*PRIME-P-SWICHING-LIMIT\* (default is 35000000),
  then FAIL-PROBABILITY is always 0.
  For more details, see doc in \*PRIME-P-SWICHING-LIMIT\*.

* The parameter \*PRIME-P-FAIL-PROBABILITY=1/4^\* specifies a number of internal iterations.
  FAIL-PROBABILITY multipled by 1/4 per 1 iteration.
  For more details, see doc in \*PRIME-P-FAIL-PROBABILITY=1/4^\*.

Examples:

    (prime-p 42) => NIL, 0
    (prime-p 43) => T, 0   ;43 < *PRIME-P-SWICHING-LIMIT*
    (prime-p 843231983040012492664856905761567078617345413887258638877481099493016757762394510526492481)
         => T, 1/1267650600228229401496703205376


#### [Special Variable] \*PRIME-P-SWICHING-LIMIT\*

The function PRIME-P has internal dispatch that controls the algorithm of primality test.
A integer below \*PRIME-P-SWICHING-LIMIT\*, is checked primality by the Trial Division algorithm.
A integer above \*PRIME-P-SWICHING-LIMIT\*, is checked primality by the Miller-Rabin Probabilistic algorithm.

The default value of \*PRIME-P-SWICHING-LIMIT\* is 500000000.
According to my experience with my environment (see. /cl-mod-prime/test.lisp, compare-td-mr),
a integer below about 500000000, the Trial Division algorithm is more effecient than the Miller-Rabin algorithm.
If your environment is not, then config this parameter.
e.g. If you always prefer the Miller-Rabin algorithm, then (setf \*PRIME-P-SWICHING-LIMIT\* 0) .


#### [Special Variable] \*PRIME-P-FAIL-PROBABILITY=1/4^\*

The function PRIME-P has internal iterations for the Miller-Rabin probabilistic algorithm.
If primality checking integer N is over \*prime-p-swiching-limit\*,
then at most FAIL-PROBABILITY, result of (PRIME-P N) may be wrong (that is, N is composite and result is T).

The parameter \*PRIME-P-FAIL-PROBABILITY=1/4^\* specifies a number of internal iterations.
FAIL-PROBABILITY multipled by 1/4 per 1 iteration.
Default value of \*PRIME-P-FAIL-PROBABILITY=1/4^\* is 50, so fail probability of prime-p is at most 1/2^100.
If you prefer more regid primality test, increce this parameter within positive fixnums.


#### [Function] NEXT-PRIME n &optional (fail-probability=1/4^ 50)

Return (valuse PRIME FAIL-PROBABILITY).
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


#### [Function] RANDOM-PRIME num-bits &optional (fail-probability=1/4^ 50)

Return (values NUM-BITS-PRIME FAIL-PROBABILITY).
NUM-BITS-PRIME may be non-prime at most FAIL-PROBABILITY.
NUM-BITS must be a positive fixnum above 1.

     
#### [Function] PRIMES-BELOW n

Return a sorted list of all primes below N.
N must be a integer.

Examples:

    (primes-below 13) => (2 3 5 7 11)
    (primes-below -1) => NIL


#### [Function] FACTORIZE n &key (group? nil) (algorithm :auto)

Return (values PRIME-FACTORS FAIL-PROBABILITY ALGORITHM).
PRIME-FACTORS is sorted list of prime factors of N.
FAIL-PROBABILITY is probability that one of PRIME-FACTORS is worng.
ALGORITHM is algorithm that is used in factorize computation.

N must be a non-negative integer.
If keyword :GROUP? is t, then output PRIME-FACTORS is grouped.
Other keyword, :ALGORITHM specifies internal algorithm.
:simple-division, :division, :div are Simple-Division algorithm.
:rho-method, :rho are Rho-Method algorithm.
:auto is Simple-Division or Rho-Method algorithm whether n is below *\factorize-swiching-limit\* or not.

Note:

:rho-method algorithm is sutable for a large N, but there is a chance result is FAIL.
This FAIL has nothing to do with FAIL-PROBAILITY.
FAIL-PROBAILITY is came from primt-p with Miller-Rabin algorithm, wheras FAIL is intrinsic to Rho-Method algorithm.

Examples:

    (factorize 42)  => (2 3 7), 0, :SIMPLE-DIVISION
    (factorize 1)   => NIL, 0, :SIMPLE-DIVISION
    (factorize 0)   => NIL, 0, :SIMPLE-DIVISION
    (factorize -53) => ERROR!!

    (factorize 1024) => (2 2 2 2 2 2 2 2 2 2), 0, :SIMPLE-DIVISION
    (factorize 1024 :group? t) => ((2 . 10)), 0, :SIMPLE-DIVISION

    (factorize 35262714657262341)
       => (3 47 250090174874201), 1/1208925819614629174706176, :RHO-METHOD
    (factorize 25 :algorithm :rho)
       => NIL, 1/1208925819614629174706176, :RHO-METHOD  ;!! FAIL !!


#### [Special Variable] \*FACTORIZE-SWICHING-LIMIT\*

The function FACTORIZE has internal dispatch that controls algorithm of prime factorization.
A integer below \*FACTORIZE-SWICHING-LIMIT\*, is factorize by Simple-Divition algorithm.
A integer above \*FACTORIZE-SWICHING-LIMIT\*, is factorize by Rho-Method algorithm.
The default value of \*FACTORIZE-SWICHING-LIMIT\* is 50000000.
There is possibliliy that Rho-Method algorithm will be fail.
If you always prefer Rho-Method algorithm, then (setf \*FACTORIZE-SWICHING-LIMIT\* 0) .


#### [Function] RANDOM-FACTORED n &key (group? nil)

Return random factored number below N.
If group? is T, then factors will be grouped.
N must be a integer above 1.

Examples:

    (random-factored 1)  => ERROR!!
    (random-factored 2)  => NIL
    (random-factored 42) => (2 5)
    (random-factored 420000 :group? t) => ((2 . 3) (3 . 1) (3049 . 1))


### Miscellaneous

* Z\*
* xgcd
* phi
* ord           <- under implementation
* jacobi-symbol <- under implementation


#### [Function] Z\* n

Return set of invertible element below n.

Example:

    (Z* 42) => (1 5 11 13 17 19 23 25 29 31 37 41)


#### [Function] XGCD x y

Return (values U0 U1 U2), s.t. U1\*X + U2\*Y = U0 = gcd(X,Y).
X,Y must be integers.

Example:

    (xgcd 42 24) => 6, -1, 2   ;42*(-1) + 24*2 = 6


#### [Function] PHI n

Euler's totient function, phi(n) := |\{ i | 1<=i<n & gcd(n,i)=1 \}|.
Return a number of relatively prime to N from 1 below N.
N must be a integer.

Note:

* n = p1^e1 \* ... \* pr^er ,
* phi(n) = p1^(e1-1) \* (p1-1) \* ... \* pr^(er-1) \* (pr-1) .


#### [Function] ORD

Under Implementation


#### [Function] JACOBI-SYMBOL

Under Optimization


TODO
----

* MOD-NTH-ROOT: implement, optimaize, doc
* MOD-SQRT: implement, optimaize, doc
* DLOG: implement, optimaize, doc
* JACOBI-SYMBOL: optimaize, doc
* ORD: optimaize, doc
* FACTORIZE: quadratic sieve, number field sieve


Reference
---------

- V. Shoup. A Computational Introduction to Number Theory and Algebra.
            2nd ed., Cambridge University Press, 2008.
            available at http://shoup.net/ntb/ntb-v2.pdf

- D. Knuth. The Art of Computer Programming, vol.2: Seminumerical Algorithms.
            3rd ed., Addison-Wesley, 1997.

- Haruhiko Okumura, Arugorizumu Jiten, Gijyutsuhyo-ronsya, 1991.

- Project Euler, Problem Overview (in order to read, you are in need to login and to solve the problem),
  http://projecteuler.net/


Author
------

- Takaya Ochiai  <#.(reverse "moc.liamg@lper.hcykt")>


License
-------

- MIT License

