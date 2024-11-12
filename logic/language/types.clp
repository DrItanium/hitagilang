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

(include logic/common/types.clp)
(include logic/parser/types.clp)

(defclass MAIN::container-expression
  (is-a has-parent
        has-contents))
(defclass MAIN::argument-block
  (is-a container-expression))
(defclass MAIN::individual-argument
  (is-a container-expression
        has-title)
  (slot title
        (source composite)
        (type SYMBOL
              INSTANCE)))
        

(defclass MAIN::returns-expression
  (is-a container-expression 
        has-title)
  (slot title
        (source composite)
        (storage shared)
        (default returns)))
(defclass MAIN::body-expression
  (is-a container-expression))
(defclass MAIN::index-expression
  (is-a container-expression))

(defclass MAIN::has-body
  (is-a USER)
  (slot body
        (type INSTANCE)
        (allowed-classes body-expression)
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass MAIN::has-index
  (is-a USER)
  (slot index
        (type INSTANCE)
        (allowed-classes index-expression)
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass MAIN::generic-body-expression
  (is-a has-parent
        has-body))
(defclass MAIN::execution-block-declaration
  (is-a generic-body-expression
        has-title)
  (slot kind
        (type SYMBOL)
        (storage shared)
        (visibility public)
        (access read-only)
        (default FALSE))
  (slot body
        (type INSTANCE)
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass MAIN::function-declaration
  (is-a execution-block-declaration)
  (slot returns
        (type INSTANCE)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot arguments
        (type INSTANCE)
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass MAIN::leaf-function-declaration
  (is-a function-declaration)
  (slot kind
        (source composite)
        (default leaf)))

(defclass MAIN::window-function-declaration
  (is-a function-declaration)
  (slot kind
        (source composite)
        (default window)))

(defclass MAIN::site-declaration
  (is-a function-declaration)
  (slot kind
        (source composite)
        (default site)))

(defclass MAIN::expression
  (is-a container-expression
        has-title))
(defclass MAIN::conditional-expression
  (is-a container-expression))

(defclass MAIN::has-condition
  (is-a USER)
  (slot condition
        (type INSTANCE)
        (allowed-classes conditional-expression)
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass MAIN::if-expression
  (is-a has-parent
        has-condition)
  (slot then
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot else 
        (storage local)
        (visibility public)
        (default ?NONE)))

(defclass MAIN::block-expression
  "Like a list or expression but _just_ a list of subexpressions"
  (is-a container-expression))

(defclass MAIN::bind-expression
  (is-a expression)
  (slot title
        (source composite)
        (type INSTANCE)
        (default ?NONE)))
; in this language, everything must be explicitly prescribed one way or another for simplicity
(defclass MAIN::call-expression
  (is-a expression)
  (slot title
        (source composite)
        (type INSTANCE
              SYMBOL)
        (default ?NONE)))

(defclass MAIN::lambda-expression
  (is-a function-declaration)
  (slot title
        (source composite)
        (default-dynamic (gensym*)))
  (slot kind
        (source composite)
        (default lambda)))

(defclass MAIN::unary-expression
  (is-a has-parent
        has-title)
  (slot argument
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass MAIN::binary-expression
  (is-a has-parent
        has-title)
  (slot left-argument
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot right-argument
             (storage local)
             (visibility public)
             (default ?NONE)))


(deftemplate MAIN::root-expression-match
             (slot keyword
                   (type SYMBOL)
                   (default ?NONE))
             (slot class-kind
                   (type SYMBOL)
                   (default ?NONE))
             (slot variable-length
                   (type SYMBOL)
                   (allowed-symbols FALSE
                                    TRUE)
                   (default FALSE))
             (slot combine-using
                   (type SYMBOL)
                   (default FALSE))
             )
(defclass MAIN::not-expression
  (is-a unary-expression))
(defclass MAIN::load-expression
  (is-a unary-expression))

(defclass MAIN::conditional-body-expression
  (is-a generic-body-expression
        has-condition))
(defclass MAIN::while-expression
  (is-a conditional-body-expression))
(defclass MAIN::switch-case-expression
  (is-a conditional-body-expression))

(defclass MAIN::switch-expression
  (is-a generic-body-expression
        has-index))
(defclass MAIN::variable-alias
  (is-a has-parent)
  (slot linkage
        (storage local)
        (visibility public)
        (default ?NONE)))

(deffacts MAIN::match-annotations
          (annotation (kind execution-block-translation)
                      (reversible FALSE)
                      (target defleaf)
                      (args leaf-function-declaration))
          (annotation (kind execution-block-translation)
                      (reversible FALSE)
                      (target defwindow)
                      (args window-function-declaration))
          (annotation (kind execution-block-translation)
                      (reversible FALSE)
                      (target defsite)
                      (args site-declaration))
          (annotation (kind unary-expression-match)
                      (reversible FALSE)
                      (target not)
                      (args not-expression))
          (annotation (kind unary-expression-match)
                      (reversible FALSE)
                      (target load)
                      (args load-expression))
          (root-expression-match (class-kind binary-expression)
                                 (variable-length TRUE)
                                 (combine-using +)
                                 (keyword +))
          (root-expression-match (class-kind binary-expression)
                                 (variable-length TRUE)
                                 (combine-using -)
                                 (keyword -))
          (root-expression-match (class-kind binary-expression)
                                 (variable-length TRUE)
                                 (combine-using *)
                                 (keyword *))
          (root-expression-match (class-kind binary-expression)
                                 (variable-length TRUE)
                                 (combine-using /)
                                 (keyword /))
          (root-expression-match (class-kind binary-expression)
                                 (keyword eq))
          (root-expression-match (class-kind binary-expression)
                                 (keyword neq))
          (root-expression-match (class-kind binary-expression)
                                 (variable-length TRUE)
                                 (combine-using and)
                                 (keyword and))
          (root-expression-match (class-kind binary-expression)
                                 (variable-length TRUE)
                                 (combine-using or)
                                 (keyword or))
          (root-expression-match (class-kind binary-expression)
                                 (keyword xor))
          (root-expression-match (class-kind binary-expression)
                                 (keyword nand))
          (root-expression-match (class-kind binary-expression)
                                 (keyword nor))
          (root-expression-match (class-kind binary-expression)
                                 (keyword xnor))
          (root-expression-match (class-kind binary-expression)
                                 (variable-length TRUE)
                                 (combine-using and)
                                 (keyword <))
          (root-expression-match (class-kind binary-expression)
                                 (variable-length TRUE)
                                 (combine-using and)
                                 (keyword >))
          (root-expression-match (class-kind binary-expression)
                                 (variable-length TRUE)
                                 (combine-using and)
                                 (keyword <=))
          (root-expression-match (class-kind binary-expression)
                                 (variable-length TRUE)
                                 (combine-using and)
                                 (keyword >=))
          (root-expression-match (class-kind binary-expression)
                                 (keyword left-shift))
          (root-expression-match (class-kind binary-expression)
                                 (keyword right-shift))
          (root-expression-match (class-kind binary-expression)
                                 (keyword remainder))
          (root-expression-match (class-kind binary-expression)
                                 (keyword store))
          (root-expression-match (class-kind binary-expression)
                                 (keyword cast))
          (annotation (kind expression-conversion-decl)
                      (target returns)
                      (reversible FALSE)
                      (args returns-expression))
          (annotation (kind expression-conversion-decl)
                      (target body)
                      (reversible FALSE)
                      (args body-expression))
          (annotation (kind expression-conversion-decl)
                      (target condition)
                      (reversible FALSE)
                      (args conditional-expression))
          (annotation (kind expression-conversion-decl)
                      (target args)
                      (reversible FALSE)
                      (args argument-block))
          (annotation (kind expression-conversion-decl)
                      (target index)
                      (reversible FALSE)
                      (args index-expression))
          (annotation (kind conditional-body-decl)
                      (target case)
                      (reversible FALSE)
                      (args switch-case-expression))
          (annotation (kind conditional-body-decl)
                      (target while)
                      (reversible FALSE)
                      (args while-expression))
          )
        
(deffacts MAIN::language-focus-facts
          (annotation (kind focus-on-stage)
                      (target process-language)
                      (reversible FALSE)
                      (args language:convert-structures
                            language:associate-variables
                            )))
