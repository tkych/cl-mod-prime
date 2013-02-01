;;;; Last modified : 2013-02-01 20:56:25 tkych

;; cl-mod-prime/cl-mod-prime.asd


;;====================================================================
;; CL-MOD-PRIME: Modular Arithmetic and Primes Library for CL
;;====================================================================
;; cl-mod-prime/
;;   cl-mod-prime.asd -- system definition (this file)
;;   package.lisp     -- package for cl-mod-prime
;;   mod.lisp         -- modular arithmetic lib
;;   prime.lisp       -- prime lib
;;   misc.lip         -- miscellaneous lib (xgcd, euler-phi, dlog, etc.)
;;   README.md        -- readme file
;;   dev-note.lip     -- development note


;;====================================================================
;; System for CL-MOD-PRIME
;;====================================================================

(in-package :cl-user)


(asdf:defsystem #:cl-mod-prime
  :name        "cl-mod-prime"
  :description "Modular Arithmetic and Primes Library for Common Lisp"
  :version     "0.0.10"
  :licence     "MIT licence"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>"
  :serial      t
  :components  ((:file "package")
                (:file "misc")
                (:file "mod")
                (:file "prime"))
  )

;;====================================================================
