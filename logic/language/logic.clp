; hitagilang
; Copyright (c) 2024, Joshua Scoggins
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defrule parser:identify-structures::expression->specific-type
         (annotation (kind expression-conversion-decl)
                     (target ?title)
                     (args ?type))
         ?obj <- (object (is-a expression)
                         (name ?name)
                         (parent ?p)
                         (title ?title)
                         (contents $?conditions))
         =>
         (unmake-instance ?obj)
         (make-instance ?name of ?type
                        (parent ?p)
                        (contents $?conditions)))

(defrule parser:identify-structures::construct-expressions
         "If the first element of a list is a symbol then it is most likely an expression of some kind, even if it isn't then it is at least safe to do the conversion"
         ?obj <- (object (is-a list)
                         (name ?name)
                         (parent ?p)
                         (contents ?first&:(symbolp ?first)
                                   $?rest))
         =>
         (unmake-instance ?obj)
         (make-instance ?name of expression
                        (parent ?p)
                        (title ?first)
                        (contents ?rest)))


(defrule parser:identify-structures::identify-argument-blocks
         ?obj <- (object (is-a expression)
                         (name ?name)
                         (parent ?p)
                         (title args)
                         (contents $?rest))
         =>
         (unmake-instance ?obj)
         (make-instance ?name of argument-block
                        (parent ?p)
                        (contents $?rest)))
(defrule parser:identify-structures::identify-individual-arguments
         ?obj <- (object (is-a list)
                         (name ?name)
                         (parent ?p)
                         (contents ?var
                                   $?conditions))
         (object (is-a argument-block)
                 (name ?p))
         =>
         (unmake-instance ?obj)
         (make-instance ?name of individual-argument
                        (parent ?p)
                        (title ?var)
                        (contents $?conditions)))

(defrule parser:identify-structures::construct-function-declaration-generic
         (annotation (kind execution-block-translation)
                     (target ?keyword)
                     (args ?type))
         ?obj <- (object (is-a expression)
                         (name ?name)
                         (parent ?p)
                         (title ?keyword)
                         (contents ?title 
                                   ?args 
                                   ?returns
                                   ?body))
         (object (is-a argument-block)
                 (name ?args))
         (object (is-a returns-expression)
                 (name ?returns))
         (object (is-a body-expression)
                 (name ?body))
         =>
         (unmake-instance ?obj)
         (make-instance ?name of ?type
                        (parent ?p)
                        (title ?title)
                        (arguments ?args)
                        (returns ?returns)
                        (body ?body)))

(defrule parser:identify-structures::identify-if-then-else-statement
         "An if statement with then and else components"
         ?obj <- (object (is-a expression)
                         (name ?name)
                         (parent ?p)
                         (title if)
                         (contents ?condition 
                                   ?then
                                   ?else))
         (object (is-a conditional-expression)
                 (name ?condition))

         (object (is-a body-expression)
                 (name ?then))
         (object (is-a body-expression)
                 (name ?else))
         =>
         (unmake-instance ?obj)
         (make-instance ?name of if-expression
                        (condition ?condition)
                        (then ?then)
                        (else ?else)))
(defrule parser:identify-structures::identify-if-then-statement
         "An if statement without an else found in the list of then expressions"
         ?obj <- (object (is-a expression)
                         (name ?name)
                         (parent ?p)
                         (title if)
                         (contents ?condition 
                                   ?then))
         (object (is-a body-expression)
                 (name ?then))

         =>
         (unmake-instance ?obj)
         (make-instance ?name of if-expression
                        (condition ?condition)
                        (then ?then)
                        (else FALSE)))

(defrule parser:identify-structures::identify-bind-expression
         ?obj <- (object (is-a expression)
                         (title bind)
                         (name ?name)
                         (parent ?p)
                         (contents ?variable
                                   $?contents))
         =>
         (unmake-instance ?obj)
         (make-instance ?name of bind-expression
                        (parent ?p)
                        (title ?variable)
                        (contents ?contents)))


(defrule parser:identify-structures::identify-call-expression
         ?obj <- (object (is-a expression)
                         (title call)
                         (name ?name)
                         (parent ?p)
                         (contents ?function
                                   $?args))
         =>
         (unmake-instance ?obj)
         (make-instance ?name of call-expression
                        (parent ?p)
                        (title ?function)
                        (contents ?args)))
(defrule parser:identify-structures::identify-lambda-expression
         ?obj <- (object (is-a expression)
                         (title lambda)
                         (name ?name)
                         (parent ?p)
                         (contents ?args 
                                   ?returns
                                   ?body))
         (object (is-a argument-block)
                 (name ?args))
         (object (is-a returns-expression)
                 (name ?returns))
         (object (is-a body-expression)
                 (name ?body))

         =>
         (unmake-instance ?obj)
         (make-instance ?name of lambda-expression
                        (parent ?p)
                        (arguments ?args)
                        (returns ?returns)
                        (body ?body)))

(defrule parser:identify-structures::identify-unary-expressions
         (annotation (kind unary-expression-match)
                     (target ?keyword)
                     (args ?class-name))
         ?obj <- (object (is-a expression)
                         (name ?expr)
                         (parent ?p)
                         (title ?keyword)
                         (contents ?value))
         =>
         (unmake-instance ?obj)
         (make-instance ?expr of ?class-name 
                        (parent ?p)
                        (title ?keyword)
                        (argument ?value)))

(defrule parser:identify-structures::identify-binary-expressions
         (root-expression-match (class-kind binary-expression)
                                (keyword ?keyword))
         ?obj <- (object (is-a expression)
                         (name ?expr)
                         (parent ?p)
                         (title ?keyword)
                         (contents ?left
                                   ?right
                                   $?rest))
         =>
         (unmake-instance ?obj)
         (make-instance ?expr of binary-expression
                        (parent ?p)
                        (title ?keyword)
                        (left-argument ?left)
                        (right-argument ?right
                                        $?rest)))
(defrule parser:identify-structures::expand-right-argument-in-binary-expression
         (root-expression-match (class-kind binary-expression)
                                (keyword ?keyword)
                                (variable-length TRUE)
                                (combine-using ?combine))
         ?obj <- (object (is-a binary-expression)
                         (name ?name)
                         (title ?keyword)
                         (right-argument ?left
                                         ?right
                                         $?rest))
         =>
         (modify-instance ?obj
                          (right-argument (make-instance of binary-expression
                                                         (parent ?name)
                                                         (title ?combine)
                                                         (left-argument ?left)
                                                         (right-argument ?right
                                                                         ?rest)))))


;(defrule parser:identify-structures::identify-while-expression
