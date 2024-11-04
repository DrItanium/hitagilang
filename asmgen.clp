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
; Generic routines for generating i960 style assembly instructions in gnu syntax
(defmessage-handler STRING to-string primary
                    ()
                    ?self)
(defmessage-handler NUMBER to-string primary
                    ()
                    (str-cat ?self))
(defmessage-handler SYMBOL to-string primary
                    ()
                    (str-cat ?self))
(defclass MAIN::reg/lit
  (is-a USER))
(defclass MAIN::freg/flit
  (is-a USER))
(defclass MAIN::freg
  (is-a freg/flit))
(defclass MAIN::literal
  (is-a reg/lit)
  (slot value
        (type INTEGER)
        (range 0 31)
        (access initialize-only)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler to-string primary))
(defmessage-handler MAIN::literal to-string primary
                    ()
                    (str-cat ?self:value))
(defclass MAIN::float-literal
  (is-a freg/flit)
  (slot value
        (type FLOAT)
        (allowed-values 0.0
                        1.0)
        (access initialize-only)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler to-string primary))

(defmessage-handler MAIN::float-literal to-string primary
                    ()
                    (str-cat ?self:value))

(defclass MAIN::register
  (is-a reg/lit
        freg)
  (slot title
        (type SYMBOL)
        (access initialize-only)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot valid-long-register-target
        (type SYMBOL)
        (access initialize-only)
        (storage local)
        (visibility public)
        (allowed-symbols FALSE
                         TRUE))
  (slot valid-triple-register-target 
        (type SYMBOL)
        (access initialize-only)
        (storage local)
        (visibility public)
        (allowed-symbols FALSE
                         TRUE))
  (slot valid-quad-register-target
        (type SYMBOL)
        (access initialize-only)
        (storage local)
        (visibility public)
        (allowed-symbols FALSE
                         TRUE))
  (slot next-register
        (type INSTANCE
              SYMBOL)
        (access initialize-only)
        (storage local)
        (visibility public)
        (allowed-symbols FALSE)
        (default-dynamic FALSE))
  (message-handler get-next-long-register primary)
  (message-handler to-string primary))
(defmessage-handler MAIN::register get-next-long-register primary
                    ()
                    (send ?self:next-register
                          get-next-register))
(defmessage-handler MAIN::register to-string primary
                    ()
                    (str-cat ?self:title))
(defclass float-register
  (is-a freg)
  (slot title
        (type SYMBOL)
        (access initialize-only)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler to-string primary))
(defmessage-handler MAIN::float-register to-string primary
                    ()
                    (str-cat ?self:title))

(defclass MAIN::label
  (is-a USER)
  (slot title
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler to-string primary))

(defmessage-handler MAIN::label to-string primary
                    ()
                    (format nil
                            "%s:"
                            ?self:title))

(deffunction MAIN::deflabel
             (?title)
             (make-instance of label
                            (title ?title)))
(defclass MAIN::operand-list
  (is-a USER)
  (multislot contents
             (storage local)
             (visibility public)
             (default ?NONE))
  (message-handler to-string primary))
(defmessage-handler MAIN::operand-list to-string primary
                    ()
                    (switch (length$ ?self:contents)
                            (case 0 then "")
                            (case 1 then (send (nth$ 1 
                                                     ?self:contents) 
                                               to-string))
                            (default (bind ?contents 
                                           (format nil
                                                   "%s, %s"
                                                   (send (nth$ 1 
                                                               ?self:contents)
                                                         to-string)
                                                   (send (nth$ 2 
                                                               ?self:contents)
                                                         to-string)))
                                     (loop-for-count (?idx 3 (length$ ?self:contents)) do
                                                     (bind ?contents
                                                           (format nil 
                                                                   "%s, %s"
                                                                   ?contents
                                                                   (send (nth$ ?idx
                                                                               ?self:contents)
                                                                         to-string))))
                                     ?contents)))

(defclass MAIN::instruction
  (is-a USER)
  (slot operation
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot operands 
        (type INSTANCE
              SYMBOL
              STRING)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (message-handler init after)
  (message-handler to-string primary))

(defmessage-handler MAIN::instruction init after
                    ()
                    (if (and (symbolp ?self:operands)
                             ?self:operands) then
                      (dynamic-put operands
                                   (str-cat ?self:operands)))
                    )

(defmessage-handler MAIN::instruction to-string primary
                    ()
                    (if ?self:operands then
                      (format nil
                              "%s %s"
                              ?self:operation
                              (send ?self:operands
                                    to-string))
                      else
                      ?self:operation))
(defglobal MAIN
           ?*format-builder-output* = stdout)
(defgeneric MAIN::definstruction)
(defmethod MAIN::definstruction
  ((?opcode SYMBOL))
  (make-instance of instruction
                 (operation ?opcode)))
(defmethod MAIN::definstruction
  ((?opcode SYMBOL)
   (?operands STRING
              operand-list))
  (make-instance of instruction
                 (operation ?opcode)
                 (operands ?operands)))

(defmethod MAIN::definstruction
  ((?opcode SYMBOL)
   (?arguments MULTIFIELD))
  (definstruction ?opcode
                  (make-instance of operand-list
                                 (contents ?arguments))))
(defmethod MAIN::definstruction
  ((?opcode SYMBOL)
   $?rest)
  (definstruction ?opcode
                  ?rest))
(deffunction MAIN::forward-declare-opcode
             (?op)
             (build (format ?*format-builder-output*
                            "(defgeneric MAIN::*%s)%n"
                            ?op)))
(defmethod MAIN::generic-opcode-decl
  ((?op SYMBOL)
   (?args STRING)
   (?body STRING))
  (build (format ?*format-builder-output*
                 "(defmethod MAIN::*%s%n (%s)%n (definstruction %s%n%s))%n"
                 ?op
                 ?args
                 ?op
                 ?body)))
(defmethod MAIN::generic-opcode-decl
  ((?op SYMBOL))
  (generic-opcode-decl ?op
                       ""
                       ""))
(defgeneric MAIN::convert-register)
(defgeneric MAIN::convert-literal)
(defgeneric MAIN::convert-reg/lit)
(deffunction MAIN::is-valid-register
             (?value)
             (switch (type ?value)
                     (case register then TRUE)
                     (case SYMBOL then (and (instance-existp (bind ?inst
                                                                   (symbol-to-instance-name ?value)))
                                            (eq (type ?inst)
                                                register)))
                     (default FALSE)))
(deffunction MAIN::is-valid-literal
             (?value)
             (switch (type ?value)
                     (case literal then TRUE)
                     (case INTEGER then (<= 0 ?value 31))
                     (default FALSE)))
(deffunction MAIN::is-valid-reg-literal
             (?value)
             (or (is-valid-register ?value)
                 (is-valid-literal ?value)))
(deffunction MAIN::is-valid-long-register
             (?reg)
             (switch (type ?reg)
                     (case float-literal then TRUE)
                     (case float-register then TRUE)
                     (case literal then TRUE)
                     (case register then (send ?reg 
                                               get-valid-long-register-target))
                     (case SYMBOL then
                       (and ?reg
                            (is-valid-register ?reg)
                            (is-valid-long-register (convert-register ?reg))))
                     (default FALSE)))
(deffunction MAIN::is-valid-triple-register
             (?reg)
             (switch (type ?reg)
                     (case float-literal then TRUE)
                     (case float-register then TRUE)
                     (case literal then TRUE)
                     (case register then 
                       (send ?reg get-valid-triple-register-target))
                     (case SYMBOL then
                       (and ?reg
                            (is-valid-register ?reg)
                            (is-valid-triple-register (convert-register ?reg))))
                     (default FALSE)))

(deffunction MAIN::is-valid-quad-register
             (?reg)
             (switch (type ?reg)
                     (case float-literal then TRUE)
                     (case float-register then TRUE)
                     (case literal then TRUE)
                     (case register then (send ?reg get-valid-quad-register-target))
                     (case SYMBOL then
                       (and ?reg
                            (is-valid-register ?reg)
                            (is-valid-quad-register (convert-register ?reg))))
                     (default FALSE)))

(defmethod MAIN::convert-register
  ((?var register))
  ?var)
(defmethod MAIN::convert-register
  ((?var SYMBOL
         (is-valid-register ?current-argument)))
  (symbol-to-instance-name ?var))

(defmethod MAIN::convert-literal
  ((?var literal))
  ?var)
(defmethod MAIN::convert-literal
  ((?var INTEGER
         (is-valid-literal ?current-argument)))
  (symbol-to-instance-name (sym-cat ?var l)))

(defmethod MAIN::convert-reg/lit
  ((?var register
         SYMBOL
         (is-valid-register ?current-argument)))
  (convert-register ?var))
(defmethod MAIN::convert-reg/lit
  ((?var literal
         INTEGER
         (is-valid-literal ?current-argument)))
  (convert-literal ?var))

; -------------------
(defmethod MAIN::is-valid-float-literal
  ((?lit FLOAT))
  (not (neq ?lit +1.0 +0.0)))
(defmethod MAIN::is-valid-float-literal
  ((?lit float-literal))
  TRUE)
(defmethod MAIN::convert-float-literal
  ((?lit FLOAT
         (is-valid-float-literal ?current-argument)))
  (symbol-to-instance-name (sym-cat ?lit 
                                    f)))
(defmethod MAIN::is-valid-float-register 
  ((?lit freg)) 
  TRUE)
(defmethod MAIN::is-valid-float-register
  ((?lit SYMBOL
         (superclassp freg 
                      (symbol-to-instance-name ?current-argument))))
  TRUE)

(defgeneric MAIN::mkblock)
(defmethod MAIN::mkblock
  ((?contents MULTIFIELD))
  (create$ ?contents))
(defmethod MAIN::mkblock
  ($?contents)
  (mkblock ?contents))
(defmessage-handler MULTIFIELD to-string primary
                    ()
                    (bind ?contents 
                          (create$))
                    (progn$ (?a ?self)
                            (bind ?contents
                                  ?contents
                                  (send ?a to-string)))
                    ?contents)

;synthetic instructions
; make this last to be sure
(definstances MAIN::literals-and-registers
              (g0 of register
                  (valid-long-register-target TRUE)
                  (valid-triple-register-target TRUE)
                  (valid-quad-register-target TRUE)
                  (next-register [g1])
                  (title g0))
              (g1 of register
                  (next-register [g2])
                  (title g1))
              (g2 of register
                  (valid-long-register-target TRUE)
                  (next-register [g3])
                  (title g2))
              (g3 of register
                  (title g3))
              (g4 of register
                  (valid-triple-register-target TRUE)
                  (valid-quad-register-target TRUE)
                  (valid-long-register-target TRUE)
                  (next-register [g5])
                  (title g4))
              (g5 of register
                  (next-register [g6])
                  (title g5))
              (g6 of register
                  (valid-long-register-target TRUE)
                  (next-register [g7])
                  (title g6))
              (g7 of register
                  (title g7))
              (g8 of register
                  (valid-triple-register-target TRUE)
                  (valid-quad-register-target TRUE)
                  (valid-long-register-target TRUE)
                  (next-register [g9])
                  (title g8))
              (g9 of register
                  (next-register [g10])
                  (title g9))
              (g10 of register
                   (valid-long-register-target TRUE)
                   (next-register [g11])
                   (title g10))
              (g11 of register
                   (title g11))
              (g12 of register
                   (valid-triple-register-target TRUE)
                   (valid-quad-register-target TRUE)
                   (valid-long-register-target TRUE)
                   (next-register [g13])
                   (title g12))
              (g13 of register
                   (next-register [g14])
                   (title g13))
              (g14 of register
                   (next-register [fp])
                   (valid-long-register-target TRUE)
                   (title g14))
              ; link register is a special form of g14
              ; it is a special form
              (lr of register
                   (title g14))
              (fp of register
                  (title fp))
              (pfp of register
                   (valid-long-register-target TRUE)
                   (valid-triple-register-target TRUE)
                   (valid-quad-register-target TRUE)
                   (next-register [sp])
                   (title pfp))
              (sp of register
                  (next-register [rip])
                  (title sp))
              (rip of register
                   (valid-long-register-target TRUE)
                   (next-register [r3])
                   (title rip))
              (r3 of register
                  (title r3))
              ; at is the assembler temporary that I have set
              (at of register
                  (title r3))
              (r4 of register
                  (next-register [r5])
                  (valid-long-register-target TRUE)
                  (valid-triple-register-target TRUE)
                  (valid-quad-register-target TRUE)
                  (title r4))
              (r5 of register
                  (next-register [r6])
                  (title r5))
              (r6 of register
                  (next-register [r7])
                  (valid-long-register-target TRUE)
                  (title r6))
              (r7 of register
                  (title r7))
              (r8 of register
                  (next-register [r9])
                  (valid-long-register-target TRUE)
                  (valid-triple-register-target TRUE)
                  (valid-quad-register-target TRUE)
                  (title r8))
              (r9 of register
                  (next-register [r10])
                  (title r9))
              (r10 of register
                   (valid-long-register-target TRUE)
                   (next-register [r11])
                   (title r10))
              (r11 of register
                   (title r11))
              (r12 of register
                   (next-register [r13])
                   (valid-long-register-target TRUE)
                   (valid-triple-register-target TRUE)
                   (valid-quad-register-target TRUE)
                   (title r12))
              (r13 of register
                   (next-register [r14])
                   (title r13))
              (r14 of register
                   (next-register [r15])
                   (valid-long-register-target TRUE)
                   (title r14))
              (r15 of register
                   (title r15))
              (fp0 of float-register
                   (title fp0))
              (fp1 of float-register
                   (title fp1))
              (fp2 of float-register
                   (title fp2))
              (fp3 of float-register
                   (title fp3))
              (0l of literal
               (value 0))
              (1l of literal
               (value 1))
              (2l of literal
               (value 2))
              (3l of literal
               (value 3))
              (4l of literal
               (value 4))
              (5l of literal
               (value 5))
              (6l of literal
               (value 6))
              (7l of literal
               (value 7))
              (8l of literal
               (value 8))
              (9l of literal
               (value 9))
              (10l of literal
               (value 10))
              (11l of literal
               (value 11))
              (12l of literal
               (value 12))
              (13l of literal
               (value 13))
              (14l of literal
               (value 14))
              (15l of literal
               (value 15))
              (16l of literal
               (value 16))
              (17l of literal
               (value 17))
              (18l of literal
               (value 18))
              (19l of literal
               (value 19))
              (20l of literal
               (value 20))
              (21l of literal
               (value 21))
              (22l of literal
               (value 22))
              (23l of literal
               (value 23))
              (24l of literal
               (value 24))
              (25l of literal
               (value 25))
              (26l of literal
               (value 26))
              (27l of literal
               (value 27))
              (28l of literal
               (value 28))
              (29l of literal
               (value 29))
              (30l of literal
               (value 30))
              (31l of literal
               (value 31))
              ; different ways to view the same thing so that dynamic type construction will work right
              (+0.0f of float-literal 
                     (value +0.0))
              (+1.0f of float-literal 
                     (value +1.0))
              (0.0f of float-literal
               (value +0.0))
              (1.0f of float-literal
               (value +1.0))
              )

