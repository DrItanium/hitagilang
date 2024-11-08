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
(include logic/common/types.clp)

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
        (type SYMBOL
              INTEGER)
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


(defmethod MAIN::*b
  ((?targ SYMBOL))
  (definstruction b
                  ?targ))
(defmethod MAIN::*call
  ((?targ SYMBOL))
  (definstruction call
                  ?targ))
(defmethod MAIN::*ret () (definstruction ret))
(defmethod MAIN::*bal
  ((?targ SYMBOL))
  (definstruction bal
                  ?targ))
(defmethod MAIN::*bno
  ((?targ SYMBOL))
  (definstruction bno
                  ?targ))
(defmethod MAIN::*bg
  ((?targ SYMBOL))
  (definstruction bg
                  ?targ))
(defmethod MAIN::*be
  ((?targ SYMBOL))
  (definstruction be
                  ?targ))
(defmethod MAIN::*bge
  ((?targ SYMBOL))
  (definstruction bge
                  ?targ))
(defmethod MAIN::*bl
  ((?targ SYMBOL))
  (definstruction bl
                  ?targ))
(defmethod MAIN::*bne
  ((?targ SYMBOL))
  (definstruction bne
                  ?targ))
(defmethod MAIN::*ble
  ((?targ SYMBOL))
  (definstruction ble
                  ?targ))
(defmethod MAIN::*bo
  ((?targ SYMBOL))
  (definstruction bo
                  ?targ))

(defmethod MAIN::*faultno () (definstruction faultno))
(defmethod MAIN::*faultg () (definstruction faultg))
(defmethod MAIN::*faulte () (definstruction faulte))
(defmethod MAIN::*faultge () (definstruction faultge))
(defmethod MAIN::*faultl () (definstruction faultl))
(defmethod MAIN::*faultne () (definstruction faultne))
(defmethod MAIN::*faultle () (definstruction faultle))
(defmethod MAIN::*faulto () (definstruction faulto))
(defmethod MAIN::*bbs
  ((?bitpos reg/lit
            INTEGER
            SYMBOL
            (is-valid-reg-literal ?current-argument)) 
   (?src register
         SYMBOL
         (is-valid-register ?current-argument)) 
   (?targ SYMBOL))
  (definstruction bbs
                  (convert-reg/lit ?bitpos)
                  (convert-register ?src)
                  ?targ))
(defmethod MAIN::*bbc
  ((?bitpos reg/lit
            INTEGER
            SYMBOL
            (is-valid-reg-literal ?current-argument)) (?src register
            SYMBOL
            (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction bbc
                  (convert-reg/lit ?bitpos)
                  (convert-register ?src)
                  ?targ))
(defmethod MAIN::*cmpibe
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) 
   (?targ SYMBOL))
  (definstruction cmpibe
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*teste
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction teste
                  (convert-register ?dst)))
(defmethod MAIN::*cmpibne
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) 
   (?targ SYMBOL))
  (definstruction cmpibne
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*testne
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction testne
                  (convert-register ?dst)))
(defmethod MAIN::*cmpibl
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) 
   (?targ SYMBOL))
  (definstruction cmpibl
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*testl
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction testl
                  (convert-register ?dst)))
(defmethod MAIN::*cmpible
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) 
   (?targ SYMBOL))
  (definstruction cmpible
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))

(defmethod MAIN::*testle
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction testle
                  (convert-register ?dst)))

(defmethod MAIN::*cmpibg
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) 
   (?targ SYMBOL))
  (definstruction cmpibg
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*testg
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction testg
                  (convert-register ?dst)))
(defmethod MAIN::*cmpibge
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction cmpibge
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*testge
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction testge
                  (convert-register ?dst)))
(defmethod MAIN::*cmpibo
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) 
   (?targ SYMBOL))
  (definstruction cmpibo
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*testo
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction testo
                  (convert-register ?dst)))
(defmethod MAIN::*cmpibno
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) 
   (?targ SYMBOL))
  (definstruction cmpibno
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*testno
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction testno
                  (convert-register ?dst)))
(defmethod MAIN::*cmpobe
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) 
   (?targ SYMBOL))
  (definstruction cmpobe
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*cmpobne
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) 
   (?targ SYMBOL))
  (definstruction cmpobne
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*cmpobl
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) 
   (?targ SYMBOL))
  (definstruction cmpobl
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*cmpoble
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) 
   (?targ SYMBOL))
  (definstruction cmpoble
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*cmpobg
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) 
   (?targ SYMBOL))
  (definstruction cmpobg
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*cmpobge
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) 
   (?targ SYMBOL))
  (definstruction cmpobge
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))

(defclass MAIN::mem-format-argument
  (is-a USER)
  (slot displacement
        (type SYMBOL
              INTEGER)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (slot abase
        (type INSTANCE
              SYMBOL)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (slot index
        (type INSTANCE
              SYMBOL)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (slot scale
        (type INTEGER)
        (allowed-integers 1
                          2
                          4
                          8
                          16)
        (storage local)
        (visibility public)
        (default-dynamic 1))
  (message-handler init after)
  (message-handler to-string primary))
(defmessage-handler MAIN::mem-format-argument init after
                    ()
                    (if (and ?self:abase 
                             (is-valid-register ?self:index)) then
                      (dynamic-put abase
                                   (convert-register (dynamic-get abase))))
                    (if (and ?self:index
                             (is-valid-register ?self:index)) then
                      (dynamic-put index
                                   (convert-register (dynamic-get index))))
                    )

(defmessage-handler MAIN::mem-format-argument to-string primary
                    ()
                    (format nil
                            "%s%s%s"
                            (if ?self:displacement then
                              (str-cat ?self:displacement)
                              else
                              "")
                            (if ?self:abase then
                              (format nil 
                                      "(%s)"
                                      (send ?self:abase
                                            to-string))
                              else
                              "")
                            (if ?self:index then
                              (format nil
                                      "[%s%s]"
                                      (send ?self:index
                                            to-string)
                                      (if (<> ?self:scale 1) then
                                        (format nil 
                                                "*%d"
                                                ?self:scale)
                                        else
                                        ""))
                              else
                              "")
                            ))
(defgeneric MAIN::mem-format-arg
            "construct the mem-format-argument object in a cleaner way")
; MEMA format
; -------------------
; Register Indirect with Offset { abase + offset }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?abase-txt SYMBOL
               (eq ?current-argument
                   abase:))
   (?abase SYMBOL
           register
           (is-valid-register ?current-argument))
   (?offset-txt SYMBOL
                (eq ?current-argument
                    offset:))
   (?offset INTEGER
            (<= 0 ?current-argument 4095)))
  (make-instance of mem-format-argument
                 (abase ?abase)
                 (displacement ?offset)))
; -------------------
; Absolute Offset { offset }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?offset-txt SYMBOL
                (eq ?current-argument
                    offset:))
   (?offset INTEGER
            (<= 0 ?current-argument 4095)))
  (make-instance of mem-format-argument
                 (displacement ?offset)))
; -------------------
; MEMB Format
; -------------------
; Register Indirect with Index and Displacement { (abase) + (index) * 2^scale + displacement }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?txt-displacement SYMBOL
                      (eq ?current-argument 
                          displacement:))
   (?displacement SYMBOL
                  INTEGER)
   (?txt-abase SYMBOL
               (eq ?current-argument
                   abase:))
   (?abase SYMBOL
           register
           (is-valid-register ?current-argument))
   (?txt-index SYMBOL
               (eq ?current-argument
                   index:))
   (?index SYMBOL
           register
           (is-valid-register ?current-argument))
   (?txt-scale SYMBOL
               (eq ?current-argument
                   scale:))
   (?scale INTEGER))
  ; if displacement is zero then do not include it to save a word
  ; use Register Indirect with Index form
  (make-instance of mem-format-argument
                 (displacement (if (eq ?displacement 
                                       0) then 
                                 FALSE 
                                 else 
                                 ?displacement))
                 (abase ?abase)
                 (index ?index)
                 (scale (switch ?scale
                                (case 1 then ?scale)
                                (case 2 then ?scale)
                                (case 4 then ?scale)
                                (case 8 then ?scale)
                                (case 16 then ?scale)
                                (default FALSE)))))
; -------------------
; Index with Displacement { (index) * 2^scale + displacement }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?txt-displacement SYMBOL
                      (eq ?current-argument 
                          displacement:))
   (?displacement SYMBOL
                  INTEGER)
   (?txt-index SYMBOL
               (eq ?current-argument
                   index:))
   (?index SYMBOL
           register
           (is-valid-register ?current-argument))
   (?txt-scale SYMBOL
               (eq ?current-argument
                   scale:))
   (?scale INTEGER))
  (make-instance of mem-format-argument
                 (displacement ?displacement)
                 (index ?index)
                 (scale (switch ?scale
                                (case 1 then ?scale)
                                (case 2 then ?scale)
                                (case 4 then ?scale)
                                (case 8 then ?scale)
                                (case 16 then ?scale)
                                (default FALSE)))))
; -------------------
; Register Indirect with Displacement { (abase) + displacement }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?txt-displacement SYMBOL
                      (eq ?current-argument 
                          displacement:))
   (?displacement SYMBOL
                  INTEGER)
   (?txt-abase SYMBOL
               (eq ?current-argument
                   abase:))
   (?abase SYMBOL
           register
           (is-valid-register ?current-argument)))
  ; if displacement is zero then change the form to Register Indirect
  ; save instruction space
  (make-instance of mem-format-argument
                 (displacement (if (eq ?displacement 
                                       0) then 
                                 FALSE 
                                 else 
                                 ?displacement))
                 (abase ?abase)))
; -------------------
; Absolute Displacement { displacement } / IP with Displacement  { (IP) + displacement + 8 }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?txt-displacement SYMBOL
                      (eq ?current-argument 
                          displacement:))
   (?displacement SYMBOL
                  INTEGER))
  (make-instance of mem-format-argument
                 (displacement ?displacement)))
; -------------------
; Register Indirect with Index { (abase) + (index) * 2^scale }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?txt-abase SYMBOL
               (eq ?current-argument
                   abase:))
   (?abase SYMBOL
           register
           (is-valid-register ?current-argument))
   (?txt-index SYMBOL
               (eq ?current-argument
                   index:))
   (?index SYMBOL
           register
           (is-valid-register ?current-argument))
   (?txt-scale SYMBOL
               (eq ?current-argument
                   scale:))
   (?scale INTEGER))
  (make-instance of mem-format-argument
                 (abase ?abase)
                 (index ?index)
                 (scale (switch ?scale
                                (case 1 then ?scale)
                                (case 2 then ?scale)
                                (case 4 then ?scale)
                                (case 8 then ?scale)
                                (case 16 then ?scale)
                                (default FALSE)))))
; -------------------
; Register Indirect { (abase) }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?txt-abase SYMBOL
               (eq ?current-argument
                   abase:))
   (?abase SYMBOL
           register
           (is-valid-register ?current-argument)))
  (make-instance of mem-format-argument
                 (abase ?abase)))

; -------------------
; lda
; -------------------
(defmethod MAIN::*lda
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction lda
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*lda
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   (?rest MULTIFIELD))
  (*lda (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*lda
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   $?rest)
  (*lda ?txt-dest
        ?dest
        ?rest))

; -------------------
; balx
; -------------------
(defmethod MAIN::*balx
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction balx
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*balx
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   (?rest MULTIFIELD))
  (*balx (mem-format-arg (expand$ ?rest))
         ?dest))
(defmethod MAIN::*balx
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   $?rest)
  (*balx ?txt-dest ?dest
         ?rest))
; -------------------
; ldl
; -------------------
(defmethod MAIN::*ldl
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument)))
  (definstruction ldl
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldl
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?rest MULTIFIELD))
  (*ldl (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ldl
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument))
   $?rest)
  (*ldl ?txt-dest
        ?dest
        ?rest))

; -------------------
; ldt
; -------------------
(defmethod MAIN::*ldt
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-triple-register ?current-argument)))
  (definstruction ldt
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldt
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?rest MULTIFIELD))
  (*ldt (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ldt
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   $?rest)
  (*ldt ?txt-dest
        ?dest
        ?rest))

; -------------------
; ldq
; -------------------
(defmethod MAIN::*ldq
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-quad-register ?current-argument)))
  (definstruction ldq
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldq
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?rest MULTIFIELD))
  (*ldq (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ldq
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   $?rest)
  (*ldq ?txt-dest
        ?dest
        ?rest))

; -------------------
; ld
; -------------------
(defmethod MAIN::*ld
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction ld
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ld
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   (?rest MULTIFIELD))
  (*ld (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ld
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   $?rest)
  (*ld ?txt-dest
        ?dest
        ?rest))

; -------------------
; ldis
; -------------------
(defmethod MAIN::*ldis
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction ldis
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldis
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   (?rest MULTIFIELD))
  (*ldis (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ldis
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   $?rest)
  (*ldis ?txt-dest
        ?dest
        ?rest))

; -------------------
; ldos
; -------------------
(defmethod MAIN::*ldos
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction ldos
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldos
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   (?rest MULTIFIELD))
  (*ldos (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ldos
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   $?rest)
  (*ldos ?txt-dest
        ?dest
        ?rest))

; -------------------
; ldib
; -------------------
(defmethod MAIN::*ldib
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction ldib
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldib
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   (?rest MULTIFIELD))
  (*ldib (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ldib
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   $?rest)
  (*ldib ?txt-dest
        ?dest
        ?rest))

; -------------------
; ldob
; -------------------
(defmethod MAIN::*ldob
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction ldob
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldob
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   (?rest MULTIFIELD))
  (*ldob (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ldob
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   $?rest)
  (*ldob ?txt-dest
        ?dest
        ?rest))

; -------------------
; bx
; -------------------
(defmethod MAIN::*bx
  ((?targ mem-format-argument))
  (definstruction bx
                  ?targ))
(defmethod MAIN::*bx
  ((?rest MULTIFIELD))
  (*bx (mem-format-arg (expand$ ?rest))))
(defmethod MAIN::*bx
  ($?rest)
  (*bx ?rest))
; -------------------
; callx
; -------------------
(defmethod MAIN::*callx
  ((?targ mem-format-argument))
  (definstruction callx
                  ?targ))
(defmethod MAIN::*callx
  ((?rest MULTIFIELD))
  (*callx (mem-format-arg (expand$ ?rest))))
(defmethod MAIN::*callx
  ($?rest)
  (*callx ?rest))
(defmethod MAIN::*callx
  ((?targ SYMBOL))
  (*callx displacement: ?targ))


; mem instructions
; -------------------
; stl
; -------------------
(defmethod MAIN::*stl
  ((?src register
         SYMBOL
         (is-valid-long-register ?current-argument))
   (?dest mem-format-argument))
  (definstruction stl
                  (convert-register ?src)
                  ?dest))
(defmethod MAIN::*stl
  ((?src register
         SYMBOL
         (is-valid-long-register ?current-argument))
   (?rest MULTIFIELD))
  (*stl ?src
        (mem-format-arg (expand$ ?rest))))

(defmethod MAIN::*stl
  ((?src register
         SYMBOL
         (is-valid-long-register ?current-argument))
   $?rest)
  (*stl ?src
        ?rest))

; -------------------
; stt
; -------------------
(defmethod MAIN::*stt
  ((?src register
         SYMBOL
         (is-valid-triple-register ?current-argument))
   (?dest mem-format-argument))
  (definstruction stt
                  (convert-register ?src)
                  ?dest))
(defmethod MAIN::*stt
  ((?src register
         SYMBOL
         (is-valid-triple-register ?current-argument))
   (?rest MULTIFIELD))
  (*stt ?src
        (mem-format-arg (expand$ ?rest))))
(defmethod MAIN::*stt
  ((?src register
         SYMBOL
         (is-valid-triple-register ?current-argument))
   $?rest)
  (*stt ?src
        ?rest))
; -------------------
; stq
; -------------------
(defmethod MAIN::*stq
  ((?src register
         SYMBOL
         (is-valid-quad-register ?current-argument))
   (?dest mem-format-argument))
  (definstruction stq
                  (convert-register ?src)
                  ?dest))

(defmethod MAIN::*stq
  ((?src register
         SYMBOL
         (is-valid-quad-register ?current-argument))
   (?rest MULTIFIELD))
  (*stq ?src
        (mem-format-arg (expand$ ?rest))))

(defmethod MAIN::*stq
  ((?src register
         SYMBOL
         (is-valid-quad-register ?current-argument))
   $?rest)
  (*stq ?src
        (mem-format-arg (expand$ ?rest))))

; -------------------
; st
; -------------------
(defmethod MAIN::*st
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst mem-format-argument))
  (definstruction st
                  (convert-register ?src)
                  ?dst))
(defmethod MAIN::*st
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst MULTIFIELD))
  (*st ?src
       (mem-format-arg (expand$ ?dst))))
(defmethod MAIN::*st
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   $?dst)
  (*st ?src
       ?dst))
; -------------------
; stob
; -------------------
(defmethod MAIN::*stob
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst mem-format-argument))
  (definstruction stob
                  (convert-register ?src)
                  ?dst))
(defmethod MAIN::*stob
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst MULTIFIELD))
  (*stob ?src
       (mem-format-arg (expand$ ?dst))))
(defmethod MAIN::*stob
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   $?dst)
  (*stob ?src
       ?dst))
; -------------------
; stib
; -------------------
(defmethod MAIN::*stib
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst mem-format-argument))
  (definstruction stib
                  (convert-register ?src)
                  ?dst))
(defmethod MAIN::*stib
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst MULTIFIELD))
  (*stib ?src
       (mem-format-arg (expand$ ?dst))))
(defmethod MAIN::*stib
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   $?dst)
  (*stib ?src
       ?dst))
; -------------------
; stos
; -------------------
(defmethod MAIN::*stos
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst mem-format-argument))
  (definstruction stos
                  (convert-register ?src)
                  ?dst))
(defmethod MAIN::*stos
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst MULTIFIELD))
  (*stos ?src
       (mem-format-arg (expand$ ?dst))))
(defmethod MAIN::*stos
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   $?dst)
  (*stos ?src
       ?dst))
; -------------------
; stis
; -------------------
(defmethod MAIN::*stis
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst mem-format-argument))
  (definstruction stis
                  (convert-register ?src)
                  ?dst))
(defmethod MAIN::*stis
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst MULTIFIELD))
  (*stis ?src
       (mem-format-arg (expand$ ?dst))))
(defmethod MAIN::*stis
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   $?dst)
  (*stis ?src
       ?dst))

; -------------------
(defmethod MAIN::*addc
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction addc
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*addo
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction addo
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*addi
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction addi
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

; @todo real instructions need to be enhanced
(defmethod MAIN::*addr
  ((?src1 freg/flit)
   (?src2 freg/flit) 
   (?dst freg))
  (definstruction addr
                  ?src1 ?src2 ?dst))
(defmethod MAIN::*addrl
  ((?src1 freg/flit (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit (is-valid-long-register ?current-argument)) 
   (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction addrl
                  ?src1 ?src2 ?dst))

(defmethod MAIN::*alterbit
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction alterbit
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*and
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction and
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*andnot
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction andnot
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*atadd
  ((?src/dst register
             SYMBOL
             (is-valid-register ?current-argument))
   (?src reg/lit
         SYMBOL
         INTEGER
         (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction atadd 
                  (convert-register ?src/dst)
                  (convert-reg/lit ?src)
                  (convert-register ?dst)))

(defmethod MAIN::*atanr
  ((?src1 freg/flit) (?src2 freg/flit) (?dst freg))
  (definstruction atanr
                  ?src1 ?src2 ?dst))
(defmethod MAIN::*atanrl
  ((?src1 freg/flit (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit (is-valid-long-register ?current-argument)) 
   (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction atanrl
                  ?src1 ?src2 ?dst))

(defmethod MAIN::*atmod
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?mask reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src/dest register
              SYMBOL
              (is-valid-register ?current-argument)))
  (definstruction atmod 
                  (convert-register ?src)
                  (convert-reg/lit ?mask)
                  (convert-register ?src/dest)))

(defmethod MAIN::*calls
  ((?targ reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)))
  (definstruction calls
                  (convert-reg/lit ?targ)))

(defmethod MAIN::*chkbit
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction chkbit
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*classr
  ((?src freg/flit))
  (definstruction classr
                  ?src))
(defmethod MAIN::*classrl
  ((?src freg/flit 
         (is-valid-long-register ?current-argument)))
  (definstruction classrl
                  ?src))

(defmethod MAIN::*clrbit
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction clrbit
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*cmpi
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)))
  (definstruction cmpi
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  ))
(defmethod MAIN::*cmpo
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)))
  (definstruction cmpo
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  ))

(defmethod MAIN::*cmpdeci
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction cmpdeci
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*cmpdeco
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction cmpdeco
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*cmpinci
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction cmpinci
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*cmpinco
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction cmpinco
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*cmpor
  ((?src1 freg/flit) 
   (?src2 freg/flit))
  (definstruction cmpor
                  ?src1 
                  ?src2))
(defmethod MAIN::*cmporl
  ((?src1 freg/flit 
          (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit 
          (is-valid-long-register ?current-argument)))
  (definstruction cmporl
                  ?src1 
                  ?src2))


(defmethod MAIN::*cmpr
  ((?src1 freg/flit) 
   (?src2 freg/flit))
  (definstruction cmpr
                  ?src1 
                  ?src2))
(defmethod MAIN::*cmprl
  ((?src1 freg/flit 
          (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit 
          (is-valid-long-register ?current-argument)))
  (definstruction cmprl
                  ?src1 
                  ?src2))

(defmethod MAIN::*cmpstr
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument))
   (?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?len reg/lit
         SYMBOL
         INTEGER
         (is-valid-reg-literal ?current-argument)))
  (definstruction cmpstr
                  (convert-register ?dst)
                  (convert-register ?src)
                  (convert-reg/lit ?len)))

(defmethod MAIN::*concmpo
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)))
  (definstruction concmpo
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)))
(defmethod MAIN::*concmpi
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)))
  (definstruction concmpi
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)))

(defmethod MAIN::*condrec
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction condrec
                  (convert-register ?src)
                  (convert-register ?dest)))

(defmethod MAIN::*condwait
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction condwait
                  (convert-register ?src)))

(defmethod MAIN::*cosr
  ((?src freg/flit) 
   (?dst freg))
  (definstruction cosr
                  ?src 
                  ?dst))
(defmethod MAIN::*cosrl
  ((?src freg/flit 
         (is-valid-long-register ?current-argument)) 
   (?dst freg 
         (is-valid-long-register ?current-argument)))
  (definstruction cosrl
                  ?src 
                  ?dst))

(defmethod MAIN::*cpysre
  ((?src1 freg/flit
          (is-valid-triple-register ?current-argument))
   (?src2 freg/flit
          (is-valid-triple-register ?current-argument))
   (?dst freg
         (is-valid-triple-register ?current-argument)))
  (definstruction cpysre
                  ?src1
                  ?src2
                  ?dst))
(defmethod MAIN::*cpyrsre
  ((?src1 freg/flit
          (is-valid-triple-register ?current-argument))
   (?src2 freg/flit
          (is-valid-triple-register ?current-argument))
   (?dst freg
         (is-valid-triple-register ?current-argument)))
  (definstruction cpyrsre
                  ?src1
                  ?src2
                  ?dst))

(defmethod MAIN::*cvtilr
  ((?src reg/lit
         SYMBOL
         INTEGER
         (is-valid-long-register ?current-argument))
   (?dst freg))
  (definstruction cvtilr
                  (convert-register ?src)
                  ?dst))

(defmethod MAIN::*cvtir
  ((?src reg/lit
         SYMBOL
         INTEGER
         (is-valid-reg-literal ?current-argument))
   (?dst freg))
  (definstruction cvtir
                  (convert-reg/lit ?src)
                  ?dst))

(defmethod MAIN::*cvtri
  ((?src freg/flit)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction cvtri
                  ?src
                  (convert-register ?dest)))

(defmethod MAIN::*cvtril
  ((?src freg/flit)
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument)))
  (definstruction cvtril
                  ?src
                  (convert-register ?dest)))

(defmethod MAIN::*cvtzri
  ((?src freg/flit)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction cvtzri
                  ?src
                  (convert-register ?dest)))

(defmethod MAIN::*cvtzril
  ((?src freg/flit)
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument)))
  (definstruction cvtzril
                  ?src
                  (convert-register ?dest)))

(defmethod MAIN::*daddc
  ((?src1 register
          SYMBOL
          (is-valid-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction daddc
                  (convert-register ?src1)
                  (convert-register ?src2)
                  (convert-register ?dest)))

(defmethod MAIN::*divo
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction divo
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*divi
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction divi
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*divr
  ((?src1 freg/flit) 
   (?src2 freg/flit) 
   (?dst freg))
  (definstruction divr
                  ?src1 
                  ?src2 
                  ?dst))
(defmethod MAIN::*divrl
  ((?src1 freg/flit 
          (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit 
          (is-valid-long-register ?current-argument)) 
   (?dst freg 
         (is-valid-long-register ?current-argument)))
  (definstruction divrl
                  ?src1 
                  ?src2 
                  ?dst))

(defmethod MAIN::*dmovt
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction dmovt
                  (convert-register ?src)
                  (convert-register ?dest)))

(defmethod MAIN::*dsubc
  ((?src1 register
          SYMBOL
          (is-valid-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction dsubc
                  (convert-register ?src1)
                  (convert-register ?src2)
                  (convert-register ?dest)))

(defmethod MAIN::*ediv
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-long-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument)))
  (definstruction ediv
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dest)))

(defmethod MAIN::*emul
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument)))
  (definstruction emul
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dest)))

(defmethod MAIN::*expr
  ((?src freg/flit) 
   (?dst freg))
  (definstruction expr
                  ?src 
                  ?dst))
(defmethod MAIN::*exprl
  ((?src freg/flit 
         (is-valid-long-register ?current-argument)) 
   (?dst freg 
         (is-valid-long-register ?current-argument)))
  (definstruction exprl
                  ?src 
                  ?dst))


(defmethod MAIN::*extract
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction extract
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*fill
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument))
   (?value reg/lit
           SYMBOL
           INTEGER
           (is-valid-reg-literal ?current-argument))
   (?len reg/lit
         SYMBOL
         INTEGER
         (is-valid-reg-literal ?current-argument)))
  (definstruction fill
                  (convert-register ?dst)
                  (convert-reg/lit ?value)
                  (convert-reg/lit ?len)))

(defmethod MAIN::*flushreg 
  () 
  (definstruction flushreg))

(defmethod MAIN::*fmark 
  () 
  (definstruction fmark))

(defmethod MAIN::*inspacc
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction inspacc
                  (convert-register ?src)
                  (convert-register ?dest)))

(defmethod MAIN::*ldphy
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction ldphy
                  (convert-register ?src)
                  (convert-register ?dest)))

(defmethod MAIN::*ldtime
  ((?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction ldtime
                  (convert-register ?dest)))

(defmethod MAIN::*logbnr
  ((?src freg/flit) 
   (?dst freg))
  (definstruction logbnr
                  ?src 
                  ?dst))
(defmethod MAIN::*logbnrl
  ((?src freg/flit 
         (is-valid-long-register ?current-argument)) 
   (?dst freg 
         (is-valid-long-register ?current-argument)))
  (definstruction logbnrl
                  ?src 
                  ?dst))

(defmethod MAIN::*logepr
  ((?src1 freg/flit) 
   (?src2 freg/flit) 
   (?dst freg))
  (definstruction logepr
                  ?src1 
                  ?src2 
                  ?dst))
(defmethod MAIN::*logeprl
  ((?src1 freg/flit 
          (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit 
          (is-valid-long-register ?current-argument)) 
   (?dst freg 
         (is-valid-long-register ?current-argument)))
  (definstruction logeprl
                  ?src1 
                  ?src2 
                  ?dst))

(defmethod MAIN::*logr
  ((?src1 freg/flit) 
   (?src2 freg/flit) 
   (?dst freg))
  (definstruction logr
                  ?src1 
                  ?src2 
                  ?dst))
(defmethod MAIN::*logrl
  ((?src1 freg/flit 
          (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit 
          (is-valid-long-register ?current-argument)) 
   (?dst freg 
         (is-valid-long-register ?current-argument)))
  (definstruction logrl
                  ?src1 
                  ?src2 
                  ?dst))

(defmethod MAIN::*mark 
  () 
  (definstruction mark))


(defmethod MAIN::*modac
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction modac
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*modi
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction modi
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*modify
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction modify
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*modpc
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction modpc
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
; according to the manuals, the modpc instruction expects src to be the same as mask. Src is a dummy operand
(defmethod MAIN::*modpc
  ((?mask reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (*modpc ?mask
          ?mask
          ?dst))

(defmethod MAIN::*modtc
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction modtc
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*mov
  ((?src reg/lit
         SYMBOL
         INTEGER
         (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction mov
                  (convert-reg/lit ?src)
                  (convert-register ?dst)))

(defmethod MAIN::*movl
  ((?src reg/lit
         SYMBOL
         INTEGER
         (is-valid-long-register ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-long-register ?current-argument)))
  (definstruction movl
                  (convert-reg/lit ?src)
                  (convert-register ?dst)))
(defmethod MAIN::*movt
  ((?src reg/lit
         SYMBOL
         INTEGER
         (is-valid-triple-register ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-triple-register ?current-argument)))
  (definstruction movt
                  (convert-reg/lit ?src)
                  (convert-register ?dst)))
(defmethod MAIN::*movq
  ((?src reg/lit
         SYMBOL
         INTEGER
         (is-valid-quad-register ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-quad-register ?current-argument)))
  (definstruction movq
                  (convert-reg/lit ?src)
                  (convert-register ?dst)))

(defmethod MAIN::*movqstr
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument))
   (?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?len reg/lit
         SYMBOL
         INTEGER
         (is-valid-reg-literal ?current-argument)))
  (definstruction movqstr
                  (convert-register ?dst)
                  (convert-register ?src)
                  (convert-reg/lit ?len)))

(defmethod MAIN::*muli
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction muli
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*mulo
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction mulo
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*movr
  ((?src freg/flit) 
   (?dst freg))
  (definstruction movr
                  ?src 
                  ?dst))
(defmethod MAIN::*movrl
  ((?src freg/flit 
         (is-valid-long-register ?current-argument)) 
   (?dst freg 
         (is-valid-long-register ?current-argument)))
  (definstruction movrl
                  ?src 
                  ?dst))

(defmethod MAIN::*movre
  ((?src freg/flit
         (is-valid-triple-register ?current-argument))
   (?dst freg
         (is-valid-triple-register ?current-argument)))
  (definstruction movre
                  ?src
                  ?dst))
(defmethod MAIN::*movstr
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument))
   (?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?len reg/lit
         SYMBOL
         INTEGER
         (is-valid-reg-literal ?current-argument)))
  (definstruction movstr
                  (convert-register ?dst)
                  (convert-register ?src)
                  (convert-reg/lit ?len)))
(defmethod MAIN::*mulr
  ((?src1 freg/flit) 
   (?src2 freg/flit) 
   (?dst freg))
  (definstruction mulr
                  ?src1 
                  ?src2 
                  ?dst))
(defmethod MAIN::*mulrl
  ((?src1 freg/flit 
          (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit 
          (is-valid-long-register ?current-argument)) 
   (?dst freg 
         (is-valid-long-register ?current-argument)))
  (definstruction mulrl
                  ?src1 
                  ?src2 
                  ?dst))
(defmethod MAIN::*nand
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction nand
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*nor
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction nor
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*not
  ((?src reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction not
                  (convert-reg/lit ?src)
                  (convert-register ?dst)))
(defmethod MAIN::*notand
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction notand
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*notbit
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction notbit
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*notor
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction notor
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*or
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction or
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*ornot
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction ornot
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*receive
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction receive
                  (convert-register ?src)
                  (convert-register ?dest)))
(defmethod MAIN::*remi
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction remi
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*remo
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction remo
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*remr
  ((?src1 freg/flit) 
   (?src2 freg/flit) 
   (?dst freg))
  (definstruction remr
                  ?src1 
                  ?src2 
                  ?dst))
(defmethod MAIN::*remrl
  ((?src1 freg/flit 
          (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit 
          (is-valid-long-register ?current-argument)) 
   (?dst freg 
         (is-valid-long-register ?current-argument)))
  (definstruction remrl
                  ?src1 
                  ?src2 
                  ?dst))
(defmethod MAIN::*resumprcs
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction resumprcs
                  (convert-register ?src)))

(defmethod MAIN::*rotate
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction rotate
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*roundr
  ((?src freg/flit) 
   (?dst freg))
  (definstruction roundr
                  ?src 
                  ?dst))
(defmethod MAIN::*roundrl
  ((?src freg/flit 
         (is-valid-long-register ?current-argument)) 
   (?dst freg 
         (is-valid-long-register ?current-argument)))
  (definstruction roundrl
                  ?src 
                  ?dst))
(defmethod MAIN::*saveprcs
  ()
  (definstruction saveprcs))
(defmethod MAIN::*scaler
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 freg/flit)
   (?dst freg))
  (definstruction scaler
                  (convert-register ?src1)
                  ?src2
                  ?dst))
(defmethod MAIN::*scalerl
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-long-register ?current-argument))
   (?src2 freg/flit)
   (?dst freg))
  (definstruction scalerl
                  (convert-reg/lit ?src1)
                  ?src2
                  ?dst))

(defmethod MAIN::*scanbit
  ((?src reg/lit
         SYMBOL
         INTEGER
         (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-literal ?current-argument)))
  (definstruction scanbit
                  (convert-reg/lit ?src)
                  (convert-register ?dst)))

(defmethod MAIN::*scanbyte
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) 
   (?src2 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)))
  (definstruction scanbyte
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)))
(defmethod MAIN::*schedprcs
  ((?src register
         SYMBOL
         (is-valid-reg-literal ?current-argument)))
  (definstruction schedprcs
                  (convert-register ?src)))

(defmethod MAIN::*send
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument))
   (?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction send
                  (convert-register ?dst)
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)))
(defmethod MAIN::*sendserv
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction sendserv
                  (convert-register ?src)))
(defmethod MAIN::*setbit
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction setbit 
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*shlo
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction shlo 
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*shro
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction shro 
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*shli
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction shli 
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*shri
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction shri 
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*shrdi
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction shrdi 
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*signal
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction signal
                  (convert-register ?src)))
(defmethod MAIN::*sinr
  ((?src freg/flit) 
   (?dst freg))
  (definstruction sinr
                  ?src 
                  ?dst))
(defmethod MAIN::*sinrl
  ((?src freg/flit 
         (is-valid-long-register ?current-argument)) 
   (?dst freg 
         (is-valid-long-register ?current-argument)))
  (definstruction sinrl
                  ?src 
                  ?dst))

(defmethod MAIN::*spanbit
  ((?src reg/lit
         SYMBOL
         INTEGER
         (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-literal ?current-argument)))
  (definstruction spanbit
                  (convert-reg/lit ?src)
                  (convert-register ?dst)))

(defmethod MAIN::*sqrtr
  ((?src freg/flit) 
   (?dst freg))
  (definstruction sqrtr
                  ?src 
                  ?dst))
(defmethod MAIN::*sqrtrl
  ((?src freg/flit 
         (is-valid-long-register ?current-argument)) 
   (?dst freg 
         (is-valid-long-register ?current-argument)))
  (definstruction sqrtrl
                  ?src 
                  ?dst))

(defmethod MAIN::*subc
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction subc
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*subi
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction subi
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*subo
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction subo
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defmethod MAIN::*subr
  ((?src1 freg/flit) 
   (?src2 freg/flit) 
   (?dst freg))
  (definstruction subr
                  ?src1 
                  ?src2 
                  ?dst))

(defmethod MAIN::*subrl
  ((?src1 freg/flit 
          (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit 
          (is-valid-long-register ?current-argument)) 
   (?dst freg 
         (is-valid-long-register ?current-argument)))
  (definstruction subrl
                  ?src1 
                  ?src2 
                  ?dst))

(defmethod MAIN::*syncf 
  () 
  (definstruction syncf))

(defmethod MAIN::*synld
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction synld
                  (convert-register ?src)
                  (convert-register ?dest)))

(defmethod MAIN::*synmov
  ((?dest register
         SYMBOL
         (is-valid-register ?current-argument))
   (?src register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction synmov
                  (convert-register ?src)
                  (convert-register ?dest)))

(defmethod MAIN::*synmovl
  ((?dest register
         SYMBOL
         (is-valid-register ?current-argument))
   (?src register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction synmovl
                  (convert-register ?src)
                  (convert-register ?dest)))
(defmethod MAIN::*synmovq
  ((?dest register
         SYMBOL
         (is-valid-register ?current-argument))
   (?src register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction synmovq
                  (convert-register ?src)
                  (convert-register ?dest)))

(defmethod MAIN::*tanr
  ((?src freg/flit) 
   (?dst freg))
  (definstruction tanr
                  ?src 
                  ?dst))
(defmethod MAIN::*tanrl
  ((?src freg/flit 
         (is-valid-long-register ?current-argument)) 
   (?dst freg 
         (is-valid-long-register ?current-argument)))
  (definstruction tanrl
                  ?src 
                  ?dst))

(defmethod MAIN::*wait
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction wait
                  (convert-register ?src)))

(defmethod MAIN::*xor
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction xor
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))
(defmethod MAIN::*xnor
  ((?src1 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?src2 reg/lit
          SYMBOL
          INTEGER
          (is-valid-reg-literal ?current-argument))
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction xnor
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  (convert-register ?dst)))

(defclass MAIN::string-directive
  (is-a USER)
  (role abstract)
  (pattern-match non-reactive)
  (slot operation
        (type SYMBOL)
        (storage shared)
        (visibility public)
        (access read-only)
        (default ILLEGAL-OPERATION))
  (slot data
        (type STRING)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler to-string primary))
(defmessage-handler MAIN::string-directive to-string primary
                    ()
                    (format nil
                            "%s \"%s\""
                            (dynamic-get operation)
                            (dynamic-get data)))
(defclass MAIN::asciz-directive
  (is-a string-directive)
  (role concrete)
  (pattern-match reactive)
  (slot operation
        (source composite)
        (default .asciz)))
(defclass MAIN::ascii-directive
  (is-a string-directive)
  (role concrete)
  (pattern-match reactive)
  (slot operation
        (source composite)
        (default .ascii)))

(defgeneric MAIN::defdirective
            "Define a directive")
(defmethod MAIN::defdirective
  ((?operation SYMBOL)
   (?args MULTIFIELD))
  (definstruction ?operation
                  (expand$ ?args)))


(defmethod MAIN::defdirective
  ((?operation SYMBOL)
   $?args)
  (defdirective ?operation
                ?args))

(defmethod MAIN::.text () (defdirective .text))
(defmethod MAIN::.data () (defdirective .data))
(defmethod MAIN::.global
  ((?target SYMBOL))
  (defdirective .global
                ?target))
(defmethod MAIN::.word
  ((?target LEXEME
            INTEGER))
  (defdirective .word
                ?target))
(defmethod MAIN::.word
  ((?target LEXEME
            INTEGER)
   (?rest MULTIFIELD))
  (defdirective .word
                ?target
                (expand$ ?rest)))
(defmethod MAIN::.word
  ((?target LEXEME
            INTEGER)
   $?rest)
  (.word ?target
         ?rest))
(defmethod MAIN::.align
  ((?value INTEGER))
  (defdirective .align
                ?value))
(defmethod MAIN::.asciz
  ((?data STRING))
  (make-instance of asciz-directive
                 (data ?data)))

(defmethod MAIN::.ascii
  ((?data STRING))
  (make-instance of ascii-directive
                 (data ?data)))

(defmethod MAIN::.bss () (defdirective .bss))
(defmethod MAIN::.bss 
  ((?title SYMBOL)
   (?size INTEGER
          SYMBOL)
   (?alignment INTEGER))
  (defdirective .bss
                ?title
                ?size
                ?alignment))

(defmethod MAIN::.skip
  ((?count INTEGER))
  (defdirective .skip
                ?count))

(defmethod MAIN::.space
  ((?count INTEGER))
  (defdirective .space
                ?count))

(defmethod MAIN::defglobal-label
  ((?name SYMBOL))
  (create$ (.global ?name)
           (deflabel ?name)))

(defmethod MAIN::.fill
  ((?count INTEGER)
   (?size INTEGER)
   (?value INTEGER))
  (defdirective .fill
                ?count
                ?size
                ?value))


(defclass MAIN::synthetic-instruction
  (is-a instruction)
  (multislot sub-instructions
             (storage local)
             (visibility public))
  (message-handler to-string primary))
(defmessage-handler MAIN::synthetic-instruction to-string primary
                    ()
                    (send ?self:sub-instructions
                          to-string))
(defgeneric MAIN::defsynthetic-instruction)
(defmethod MAIN::defsynthetic-instruction
  ((?opcode SYMBOL)
   (?arguments STRING
               operand-list)
   (?real-instructions MULTIFIELD))
  (make-instance of synthetic-instruction
                 (operation ?opcode)
                 (operands ?arguments)
                 (sub-instructions ?real-instructions)))
(defmethod MAIN::defsynthetic-instruction
  ((?opcode SYMBOL)
   (?arguments MULTIFIELD)
   (?real-instructions MULTIFIELD))
  (defsynthetic-instruction ?opcode
                            (make-instance of operand-list
                                           (contents ?arguments))
                            ?real-instructions))
(defmethod MAIN::defsynthetic-instruction
  ((?opcode SYMBOL)
   (?arguments STRING
               operand-list
               MULTIFIELD)
   $?real-instructions)
  (defsynthetic-instruction ?opcode
                            ?arguments
                            ?real-instructions))

; synthetic instructions
(defmethod MAIN::*ldconst
  ((?value NUMBER
           SYMBOL)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction ldconst
                  ?value
                  (convert-register ?dest)))

(defmethod MAIN::*cmpibez ((?src2 register) (?targ SYMBOL)) (*cmpibe [0l] ?src2 ?targ))
(defmethod MAIN::*cmpobez ((?src2 register) (?targ SYMBOL)) (*cmpobe [0l] ?src2 ?targ))
(defmethod MAIN::*cmpibnez ((?src2 register) (?targ SYMBOL)) (*cmpibne [0l] ?src2 ?targ))
(defmethod MAIN::*cmpobnez ((?src2 register) (?targ SYMBOL)) (*cmpobne [0l] ?src2 ?targ))
(defmethod MAIN::*cmpiblz ((?src2 register) (?targ SYMBOL)) (*cmpibl [0l] ?src2 ?targ))
(defmethod MAIN::*cmpiblez ((?src2 register) (?targ SYMBOL)) (*cmpible [0l] ?src2 ?targ))
(defmethod MAIN::*cmpibgz ((?src2 register) (?targ SYMBOL)) (*cmpibg [0l] ?src2 ?targ))
(defmethod MAIN::*cmpibgez ((?src2 register) (?targ SYMBOL)) (*cmpibge [0l] ?src2 ?targ))
; Is a given register value between two other register values
; equivalent to (<= ?lo ?target ?hi) in CLIPS
(defmethod MAIN::*twixto 
  ((?lo reg/lit
        SYMBOL
        INTEGER
        (is-valid-reg-literal ?current-argument))
   (?target reg/lit
            SYMBOL
            INTEGER
            (is-valid-reg-literal ?current-argument))
   (?hi reg/lit
        SYMBOL
        INTEGER
        (is-valid-reg-literal ?current-argument)))
  (defsynthetic-instruction twixto
                            (apply-to-each$ convert-reg/lit 
                                            ?lo
                                            ?target
                                            ?hi)
                            (*cmpo ?hi
                                   ?target)
                            (*concmpo ?lo
                                      ?target)))
(defmethod MAIN::*twixti
  ((?lo reg/lit
        SYMBOL
        INTEGER
        (is-valid-reg-literal ?current-argument))
   (?target reg/lit
            SYMBOL
            INTEGER
            (is-valid-reg-literal ?current-argument))
   (?hi reg/lit
        SYMBOL
        INTEGER
        (is-valid-reg-literal ?current-argument)))
  (defsynthetic-instruction twixti
                            (apply-to-each$ convert-reg/lit 
                                            ?lo
                                            ?target
                                            ?hi)
                            (*cmpi ?hi
                                   ?target)
                            (*concmpi ?lo
                                      ?target)))

; compare with zero
(defmethod MAIN::*cmpiz ((?src2 reg/lit)) (*cmpi [0l] ?src2))
(defmethod MAIN::*cmpoz ((?src2 reg/lit)) (*cmpo [0l] ?src2))

(defmethod MAIN::*extract-byte
  ((?bitpos literal)
   (?src/dest register))
  (*extract ?bitpos
            [8l]
            ?src/dest))
(defmethod MAIN::*extract-lowest-byte
  ((?src/dest register))
  (*extract-byte [0l]
                 ?src/dest))

(defmethod MAIN::*extract-lower-byte
  ((?src/dest register))
  (*extract-byte [8l]
                 ?src/dest))

(defmethod MAIN::*extract-higher-byte
  ((?src/dest register))
  (*extract-byte [16l]
                 ?src/dest))

(defmethod MAIN::*extract-highest-byte
  ((?src/dest register))
  (*extract-byte [24l]
                 ?src/dest))
(defmethod MAIN::*extract-lower-half
  ((?src/dest register))
  (*extract [0l]
            [16l]
            ?src/dest))

(defmethod MAIN::*extract-upper-half
  ((?src/dest register))
  (*extract [16l]
            [16l]
            ?src/dest))


(defmethod MAIN::*get-pc
  ((?dest register))
  (*modpc [0l] 
          ?dest))

(defmethod MAIN::*get-ac
  ((?dest register))
  (*modac [0l]
          [0l]
          ?dest))

(defmethod MAIN::*get-tc
  ((?dest register))
  (*modtc [0l]
          [0l]
          ?dest))

(defmethod MAIN::*xorl
  ((?src1 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument)))
  (defsynthetic-instruction *xorl
                            (apply-to-each$ convert-register
                                            ?src1
                                            ?src2
                                            ?dest)
                            (*xor (convert-register ?src1)
                                  (convert-register ?src2)
                                  (convert-register ?dest))
                            (*xor (send (convert-register ?src1)
                                        get-next-register)
                                  (send (convert-register ?src2)
                                        get-next-register)
                                  (send (convert-register ?dest)
                                        get-next-register))))

(defmethod MAIN::*xort
  ((?src1 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-triple-register ?current-argument)))
  (defsynthetic-instruction *xort
                            (apply-to-each$ convert-register
                                            ?src1
                                            ?src2
                                            ?dest)
                            (*xorl ?src1
                                   ?src2
                                   ?dest)
                            (*xor (send (convert-register ?src1)
                                        get-next-long-register)
                                  (send (convert-register ?src2)
                                        get-next-long-register)
                                  (send (convert-register ?dest)
                                        get-next-long-register))))

(defmethod MAIN::*xorq
  ((?src1 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-quad-register ?current-argument)))
  (defsynthetic-instruction *xorq
                            (apply-to-each$ convert-register
                                            ?src1
                                            ?src2
                                            ?dest)
                            (*xorl ?src1
                                   ?src2
                                   ?dest)
                            (*xorl (send (convert-register ?src1)
                                         get-next-long-register)
                                   (send (convert-register ?src2)
                                         get-next-long-register)
                                   (send (convert-register ?dest)
                                         get-next-long-register))))

(defmethod MAIN::*xnorl
  ((?src1 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument)))
  (create$ (*xnor (convert-register ?src1)
                  (convert-register ?src2)
                  (convert-register ?dest))
           (*xnor (send (convert-register ?src1)
                        get-next-register)
                  (send (convert-register ?src2)
                        get-next-register)
                  (send (convert-register ?dest)
                        get-next-register))))

(defmethod MAIN::*xnort
  ((?src1 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-triple-register ?current-argument)))
  (create$ (*xnorl ?src1
                   ?src2
                   ?dest)
           (*xnor (send (convert-register ?src1)
                        get-next-long-register)
                  (send (convert-register ?src2)
                        get-next-long-register)
                  (send (convert-register ?dest)
                        get-next-long-register))))

(defmethod MAIN::*xnorq
  ((?src1 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-quad-register ?current-argument)))
  (create$ (*xnorl ?src1
                   ?src2
                   ?dest)
           (*xnorl (send (convert-register ?src1)
                         get-next-long-register)
                   (send (convert-register ?src2)
                         get-next-long-register)
                   (send (convert-register ?dest)
                         get-next-long-register))))

(defmethod MAIN::*norl
  ((?src1 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument)))
  (create$ (*nor (convert-register ?src1)
                 (convert-register ?src2)
                 (convert-register ?dest))
           (*nor (send (convert-register ?src1)
                       get-next-register)
                 (send (convert-register ?src2)
                       get-next-register)
                 (send (convert-register ?dest)
                       get-next-register))))

(defmethod MAIN::*nort
  ((?src1 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-triple-register ?current-argument)))
  (create$ (*norl ?src1
                  ?src2
                  ?dest)
           (*nor (send (convert-register ?src1)
                       get-next-long-register)
                 (send (convert-register ?src2)
                       get-next-long-register)
                 (send (convert-register ?dest)
                       get-next-long-register))))

(defmethod MAIN::*norq
  ((?src1 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-quad-register ?current-argument)))
  (create$ (*norl ?src1
                  ?src2
                  ?dest)
           (*norl (send (convert-register ?src1)
                        get-next-long-register)
                  (send (convert-register ?src2)
                        get-next-long-register)
                  (send (convert-register ?dest)
                        get-next-long-register))))

(defmethod MAIN::*nandl
  ((?src1 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument)))
  (create$ (*nand (convert-register ?src1)
                  (convert-register ?src2)
                  (convert-register ?dest))
           (*nand (send (convert-register ?src1)
                        get-next-register)
                  (send (convert-register ?src2)
                        get-next-register)
                  (send (convert-register ?dest)
                        get-next-register))))

(defmethod MAIN::*nandt
  ((?src1 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-triple-register ?current-argument)))
  (create$ (*nandl ?src1
                   ?src2
                   ?dest)
           (*nand (send (convert-register ?src1)
                        get-next-long-register)
                  (send (convert-register ?src2)
                        get-next-long-register)
                  (send (convert-register ?dest)
                        get-next-long-register))))

(defmethod MAIN::*nandq
  ((?src1 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-quad-register ?current-argument)))
  (create$ (*nandl ?src1
                   ?src2
                   ?dest)
           (*nandl (send (convert-register ?src1)
                         get-next-long-register)
                   (send (convert-register ?src2)
                         get-next-long-register)
                   (send (convert-register ?dest)
                         get-next-long-register))))
(defmethod MAIN::*andl
  ((?src1 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument)))
  (create$ (*and (convert-register ?src1)
                 (convert-register ?src2)
                 (convert-register ?dest))
           (*and (send (convert-register ?src1)
                       get-next-register)
                 (send (convert-register ?src2)
                       get-next-register)
                 (send (convert-register ?dest)
                       get-next-register))))

(defmethod MAIN::*andt
  ((?src1 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-triple-register ?current-argument)))
  (create$ (*andl ?src1
                  ?src2
                  ?dest)
           (*and (send (convert-register ?src1)
                       get-next-long-register)
                 (send (convert-register ?src2)
                       get-next-long-register)
                 (send (convert-register ?dest)
                       get-next-long-register))))

(defmethod MAIN::*andq
  ((?src1 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-quad-register ?current-argument)))
  (create$ (*andl ?src1
                  ?src2
                  ?dest)
           (*andl (send (convert-register ?src1)
                        get-next-long-register)
                  (send (convert-register ?src2)
                        get-next-long-register)
                  (send (convert-register ?dest)
                        get-next-long-register))))
(defmethod MAIN::*orl
  ((?src1 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument)))
  (create$ (*or (convert-register ?src1)
                (convert-register ?src2)
                (convert-register ?dest))
           (*or (send (convert-register ?src1)
                      get-next-register)
                (send (convert-register ?src2)
                      get-next-register)
                (send (convert-register ?dest)
                      get-next-register))))

(defmethod MAIN::*ort
  ((?src1 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-triple-register ?current-argument)))
  (create$ (*orl ?src1
                 ?src2
                 ?dest)
           (*or (send (convert-register ?src1)
                      get-next-long-register)
                (send (convert-register ?src2)
                      get-next-long-register)
                (send (convert-register ?dest)
                      get-next-long-register))))

(defmethod MAIN::*orq
  ((?src1 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-quad-register ?current-argument)))
  (create$ (*orl ?src1
                 ?src2
                 ?dest)
           (*orl (send (convert-register ?src1)
                       get-next-long-register)
                 (send (convert-register ?src2)
                       get-next-long-register)
                 (send (convert-register ?dest)
                       get-next-long-register))))

(defmethod MAIN::*nop
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument)))
  (*or 0 
       ?src 
       ?src))
(defmethod MAIN::*nop () (*nop r15))

(defmethod MAIN::*inci 
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (*addi 1
         ?src
         ?dest))

(defmethod MAIN::*inco
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (*addo 1
         ?src
         ?dest))

(defmethod MAIN::*deci 
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (*subi 1
         ?src
         ?dest))

(defmethod MAIN::*deco
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (*subo 1
         ?src
         ?dest))

(defmethod MAIN::*calls
  ((?targ INTEGER
          (<= 32 ?current-argument 259)))
  (defsynthetic-instruction *calls
                            (create$ ?targ)
                            (*ldconst ?targ
                                      g13)
                            (*calls g13)))

(defmethod MAIN::defroutine:window
  ((?name SYMBOL)
   (?body MULTIFIELD))
  (mkblock (deflabel ?name)
           ?body
           (*ret)))
(defmethod MAIN::defroutine:window
  ((?name SYMBOL)
   $?rest)
  (defroutine:window ?name 
                     ?rest))

(defmethod MAIN::defroutine:leaf
  ((?name SYMBOL)
   (?body MULTIFIELD))
  (mkblock (deflabel ?name)
           ?body
           (*bx abase: lr)))
(defmethod MAIN::defroutine:leaf
  ((?name SYMBOL)
   $?rest)
  (defroutine:leaf ?name
                   ?rest))
(defmethod MAIN::defsite
  ((?name SYMBOL)
   (?body MULTIFIELD))
  (mkblock (deflabel ?name)
           ?body))
(defmethod MAIN::defsite
  ((?name SYMBOL)
   $?body)
  (defsite ?name
           ?body))


(defmethod MAIN::*cmpolbne
  ((?src1 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?targ SYMBOL))
  (defsynthetic-instruction cmpolbne
                            (create$ (bind ?r1
                                           (convert-register ?src1))
                                     (bind ?r2
                                           (convert-register ?src2))
                                     ?targ)
                            (*cmpobne ?r1
                                      ?r2
                                      ?targ)
                            (*cmpobne (send ?r1
                                            get-next-register)
                                      (send ?r2
                                            get-next-register)
                                      ?targ)))
(defmethod MAIN::*cmpotbne
  ((?src1 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?targ SYMBOL))
  (create$ (*cmpolbne ?src1 ?src2 ?targ)
           (*cmpobne (send (convert-register ?src1)
                           get-next-long-register)
                     (send (convert-register ?src2)
                           get-next-long-register)
                     ?targ)))
(defmethod MAIN::*cmpoqbne
  ((?src1 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?targ SYMBOL))
  (create$ (*cmpolbne ?src1 ?src2 ?targ)
           (*cmpolbne (send (convert-register ?src1)
                            get-next-long-register)
                      (send (convert-register ?src2)
                            get-next-long-register)
                      ?targ)))

(defmethod MAIN::*cmpolbe
  ((?src1 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?targ SYMBOL))
  (create$ (*cmpobe (convert-register ?src1)
                    (convert-register ?src2)
                    ?targ)
           (*cmpobe (send (convert-register ?src1)
                          get-next-register)
                    (send (convert-register ?src2)
                          get-next-register)
                    ?targ)))
(defmethod MAIN::*cmpotbe
  ((?src1 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?targ SYMBOL))
  (create$ (*cmpolbe ?src1 ?src2 ?targ)
           (*cmpobe (send (convert-register ?src1)
                          get-next-long-register)
                    (send (convert-register ?src2)
                          get-next-long-register)
                    ?targ)))
(defmethod MAIN::*cmpoqbe
  ((?src1 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?targ SYMBOL))
  (create$ (*cmpolbe ?src1 ?src2 ?targ)
           (*cmpolbe (send (convert-register ?src1)
                           get-next-long-register)
                     (send (convert-register ?src2)
                           get-next-long-register)
                     ?targ)))

(defmethod MAIN::*cmpolble
  ((?src1 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?targ SYMBOL))
  (create$ (*cmpoble (convert-register ?src1)
                     (convert-register ?src2)
                     ?targ)
           (*cmpoble (send (convert-register ?src1)
                           get-next-register)
                     (send (convert-register ?src2)
                           get-next-register)
                     ?targ)))
(defmethod MAIN::*cmpotble
  ((?src1 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?targ SYMBOL))
  (create$ (*cmpolble ?src1 ?src2 ?targ)
           (*cmpoble (send (convert-register ?src1)
                           get-next-long-register)
                     (send (convert-register ?src2)
                           get-next-long-register)
                     ?targ)))
(defmethod MAIN::*cmpoqble
  ((?src1 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?targ SYMBOL))
  (create$ (*cmpolble ?src1 ?src2 ?targ)
           (*cmpolble (send (convert-register ?src1)
                            get-next-long-register)
                      (send (convert-register ?src2)
                            get-next-long-register)
                      ?targ)))

(defmethod MAIN::*cmpolbl
  ((?src1 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?targ SYMBOL))
  (create$ (*cmpobl (convert-register ?src1)
                    (convert-register ?src2)
                    ?targ)
           (*cmpobl (send (convert-register ?src1)
                          get-next-register)
                    (send (convert-register ?src2)
                          get-next-register)
                    ?targ)))
(defmethod MAIN::*cmpotbl
  ((?src1 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?targ SYMBOL))
  (create$ (*cmpolbl ?src1 ?src2 ?targ)
           (*cmpobl (send (convert-register ?src1)
                          get-next-long-register)
                    (send (convert-register ?src2)
                          get-next-long-register)
                    ?targ)))
(defmethod MAIN::*cmpoqbl
  ((?src1 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?targ SYMBOL))
  (create$ (*cmpolbl ?src1 ?src2 ?targ)
           (*cmpolbl (send (convert-register ?src1)
                           get-next-long-register)
                     (send (convert-register ?src2)
                           get-next-long-register)
                     ?targ)))


(defmethod MAIN::.asciznl
  ((?contents STRING))
  (.asciz (str-cat ?contents "\\n")))

(defmethod MAIN::deflabeled-string
  ((?title SYMBOL)
   (?contents STRING))
  (create$ (deflabel ?title)
           (.asciz ?contents)))

(defmethod MAIN::deflabeled-string-newline
  ((?title SYMBOL)
   (?contents STRING))
  (create$ (deflabel ?title)
           (.asciznl ?contents)))
