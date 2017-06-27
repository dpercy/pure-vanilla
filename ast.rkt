#lang racket

(provide (all-defined-out))

(require "constructor.rkt")

(tag-structs
 "Core"

 (Global mod name)
 (Local name number)
 (Lit value)

 (Func params body)

 (Call func args)

 ;;
 )
