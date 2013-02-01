;;;; Last modified : 2013-02-01 20:57:35 tkych

;; cl-mod-prime/package.lisp


;;====================================================================
;; Package for CL-MOD-PRIME
;;====================================================================
(in-package :cl-user)

(defpackage #:cl-mod-prime
  (:nicknames #:mod-prime)
  (:use :cl)
  ;; misc
  (:export #:xgcd
           #:phi
           #:z*
           ;; #:ord
           )
  ;; modular arithmetic
  (:export #:mod-mult
           #:mod-expt
           #:mod-inv
           ;; #:mod-sqrt
           ;; #:mod-nth-root
           ;; #:dlog
           )
  ;; prime
  (:export #:prime-p
           #:*prime-p-swiching-limit*
           #:*prime-p-fail-probability=1/4^*
           #:next-prime
           #:random-prime
           #:primes-below
           #:factorize
           #:*factorize-swiching-limit*
           #:random-factored
           )
  )

;;====================================================================
