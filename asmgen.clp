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
  (message-handler to-string primary))
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
(deffunction MAIN::forward-declare-opcode
             (?op)
             (build (format nil
                            "(defgeneric MAIN::*%s)"
                            ?op)))
(defmethod MAIN::generic-opcode-decl
  ((?op SYMBOL)
   (?args STRING)
   (?body STRING))
  (build (format nil
                 "(defmethod MAIN::*%s (%s) (definstruction %s %s))"
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
                     (case register then (send ?reg get-valid-long-register-target))
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
                              (if (integerp ?self:displacement) then
                                (format nil
                                        "0x%x"
                                        ?self:displacement)
                                else
                                ?self:displacement)
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


; ctrl instructions
(defglobal MAIN 
           ?*ctrl-standard-instructions* = (create$ b
                                                    call
                                                    bal
                                                    bno
                                                    bg
                                                    be
                                                    bge
                                                    bl
                                                    bne
                                                    ble
                                                    bo)
           ?*bit-check-operations* = (create$ bbs
                                              bbc)
           ?*cond-kinds-all* = (create$ e
                                        ne
                                        l
                                        le
                                        g
                                        ge
                                        o
                                        no)
           ?*cmpob-kinds* = (create$ e
                                     ne
                                     l
                                     le
                                     g
                                     ge)
           ?*two-register-lit-operations* = (create$ concmpo
                                                     concmpi
                                                     cmpi
                                                     cmpo
                                                     scanbyte)
           ?*two-register-only-operations* = (create$ dmovt
                                                      synld
                                                      condrec
                                                      receive
                                                      ; these instructions do not require aligned registers to function
                                                      ; they are special
                                                      synmov
                                                      synmovl
                                                      synmovq
                                                      )
           ?*three-register-only-operations* = (create$ daddc
                                                        dsubc)
           ?*standard-three-arg-real-ops* = (create$ atanr
                                                     addr
                                                     mulr
                                                     remr
                                                     logr
                                                     logepr
                                                     divr
                                                     subr)
           ?*common-reg-instructions* = (create$ notbit
                                                 and
                                                 andnot
                                                 setbit
                                                 notand
                                                 xor
                                                 or
                                                 nor
                                                 xnor
                                                 ornot
                                                 clrbit
                                                 notor
                                                 nand
                                                 alterbit
                                                 addo
                                                 addi
                                                 subo
                                                 subi
                                                 shro
                                                 shrdi
                                                 shri
                                                 shlo
                                                 rotate
                                                 shli
                                                 chkbit
                                                 cmpinco
                                                 cmpinci
                                                 cmpdeco
                                                 cmpdeci
                                                 addc
                                                 subc
                                                 modac
                                                 modify
                                                 extract
                                                 modtc
                                                 modpc
                                                 mulo
                                                 remo
                                                 divo
                                                 muli
                                                 remi
                                                 modi
                                                 divi
                                                 )
           ?*no-argument-instructions* = (create$ ret
                                                  fmark
                                                  mark
                                                  syncf
                                                  flushreg
                                                  faultno
                                                  faultg
                                                  faulte
                                                  faultge
                                                  faultl
                                                  faultne
                                                  faultle
                                                  faulto
                                                  saveprcs)
           ?*two-arg-reg-instructions* = (create$ not
                                                  mov
                                                  spanbit
                                                  scanbit)
           ?*compare-ops-real* = (create$ cmpr
                                          cmpor)
           ?*transfer-instructions* = (create$ movqstr
                                               movstr
                                               cmpstr)
           ?*single-freg-ops* = (create$ classr)
           ?*single-reg-ops* = (create$ condwait
                                        schedprcs
                                        sendserv
                                        signal
                                        wait
                                        resumprcs)
           ?*freglit-flit-ops* = (create$ movr
                                          cosr
                                          sinr
                                          expr
                                          logbnr
                                          roundr
                                          sqrtr
                                          tanr
                                          )
           ?*arg-reg-lit-field* = "(%s reg/lit INTEGER SYMBOL (is-valid-reg-literal ?current-argument))"
           ?*arg-reg-field* = "(%s register SYMBOL (is-valid-register ?current-argument))"
           ?*call-convert-reg-lit* = "(convert-reg/lit %s)"
           ?*call-convert-register* = "(convert-register %s)"
           )
(deffunction MAIN::string-call-convert-reg-lit
             (?arg)
             (format nil
                     ?*call-convert-reg-lit*
                     ?arg))
(deffunction MAIN::string-call-convert-register
             (?arg)
             (format nil
                     ?*call-convert-register*
                     ?arg))
(deffunction MAIN::string-arg-reg-lit-field
             (?arg)
             (format nil
                     ?*arg-reg-lit-field*
                     ?arg))
(deffunction MAIN::string-arg-reg-field
             (?arg)
             (format nil
                     ?*arg-reg-field*
                     ?arg))
(progn$ (?operation ?*ctrl-standard-instructions*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             "(?targ SYMBOL)"
                             "?targ"))
(progn$ (?operation ?*bit-check-operations*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             (format nil
                                     "%s %s (?targ SYMBOL)"
                                     (string-arg-reg-lit-field "?bitpos")
                                     (string-arg-reg-field "?src"))
                             (format nil
                                     "%s %s ?targ"
                                     (string-call-convert-reg-lit "?bitpos") 
                                     (string-call-convert-register "?src") )))
(progn$ (?fragment ?*cond-kinds-all*)
        (bind ?op0
              (sym-cat cmpib
                       ?fragment))
        (bind ?op1
              (sym-cat test
                       ?fragment))
        (forward-declare-opcode ?op0)
        (forward-declare-opcode ?op1)
        (generic-opcode-decl ?op0
                             (format nil
                                     "%s %s (?targ SYMBOL)"
                                     (string-arg-reg-lit-field "?src1")
                                     (string-arg-reg-field "?src2"))
                             (format nil
                                     "%s %s ?targ"
                                     (string-call-convert-reg-lit "?src1")
                                     (string-call-convert-register "?src2")))

        (generic-opcode-decl ?op1
                             (string-arg-reg-field "?dst")
                             (string-call-convert-register "?dst")))

(progn$ (?fragment ?*cmpob-kinds*)
        (bind ?operation
              (sym-cat cmpob
                       ?fragment))
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             (format nil
                                     "%s %s (?targ SYMBOL)"
                                     (string-arg-reg-lit-field "?src1")
                                     (string-arg-reg-field "?src2"))
                             (format nil
                                     "%s %s ?targ"
                                     (string-call-convert-reg-lit "?src1")
                                     (string-call-convert-register "?src2"))))

(progn$ (?operation ?*two-register-lit-operations*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             (format nil
                                     "%s %s"
                                     (string-arg-reg-lit-field "?src1")
                                     (string-arg-reg-lit-field "?src2"))
                             (format nil
                                     "%s %s"
                                     (string-call-convert-reg-lit "?src1")
                                     (string-call-convert-reg-lit "?src2"))))
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


(progn$ (?operation ?*standard-three-arg-real-ops*)
        ; construct the long versions manually
        (bind ?long-operation
              (sym-cat ?operation
                       l))
        (forward-declare-opcode ?operation)
        (forward-declare-opcode ?long-operation)
        (generic-opcode-decl ?operation
                             "(?src1 freg/flit) (?src2 freg/flit) (?dst freg)"
                             "?src1 ?src2 ?dst")
        (generic-opcode-decl ?long-operation
                             "(?src1 freg/flit (is-valid-long-register ?current-argument)) 
                             (?src2 freg/flit (is-valid-long-register ?current-argument)) 
                             (?dst freg (is-valid-long-register ?current-argument))"
                             "?src1 ?src2 ?dst"))

(progn$ (?operation ?*two-register-only-operations*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             "(?src register) (?dest register)"
                             "?src ?dest"))
(progn$ (?operation ?*three-register-only-operations*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             "(?src1 register) (?src2 register) (?dest register)"
                             "?src1 ?src2 ?dest"))

(progn$ (?operation ?*common-reg-instructions*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             "(?src1 reg/lit) (?src2 reg/lit) (?dst register)"
                             "?src1 ?src2 ?dst"))
(progn$ (?operation ?*no-argument-instructions*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation))

(progn$ (?operation ?*two-arg-reg-instructions*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             "(?src reg/lit) (?dst register)"
                             "?src ?dst"))
(progn$ (?operation ?*compare-ops-real*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             "(?src1 freg/flit) (?src2 freg/flit)"
                             "?src1 ?src2")
        (bind ?long-form
              (sym-cat ?operation
                       l))
        (forward-declare-opcode ?long-form)
        (generic-opcode-decl ?long-form
                             "(?src1 freg/flit (is-valid-long-register ?current-argument)) (?src2 freg/flit (is-valid-long-register ?current-argument))"
                             "?src1 ?src2"))
(progn$ (?operation ?*transfer-instructions*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             "(?dst register) (?src register) (?len reg/lit)"
                             "?dst ?src ?len"))
(progn$ (?operation ?*freglit-flit-ops*)
        (bind ?long-operation
              (sym-cat ?operation
                       l))
        (forward-declare-opcode ?operation)
        (forward-declare-opcode ?long-operation)
        (generic-opcode-decl ?operation
                             "(?src freg/flit) (?dst freg)"
                             "?src ?dst")
        (generic-opcode-decl ?long-operation
                             "(?src freg/flit (is-valid-long-register ?current-argument)) (?dst freg (is-valid-long-register ?current-argument))"
                             "?src ?dst"))
(progn$ (?operation ?*single-freg-ops*)
        (bind ?long-operation
              (sym-cat ?operation
                       l))
        (forward-declare-opcode ?operation)
        (forward-declare-opcode ?long-operation)
        (generic-opcode-decl ?operation
                             "(?src freg/flit)"
                             "?src")
        (generic-opcode-decl ?long-operation
                             "(?src freg/flit (is-valid-long-register ?current-argument))"
                             "?src"))
(progn$ (?operation ?*single-reg-ops*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             "(?src register)"
                             "?src"))

(defmethod MAIN::*ediv
  ((?src1 reg/lit)
   (?src2 reg/lit
          (is-valid-long-register ?current-argument))
   (?dest register
          (is-valid-long-register ?current-argument)))
  (definstruction ediv
                  ?src1
                  ?src2
                  ?dest))

(defmethod MAIN::*emul
  ((?src1 reg/lit)
   (?src2 reg/lit)
   (?dest register
          (is-valid-long-register ?current-argument)))
  (definstruction emul
                  ?src1
                  ?src2
                  ?dest))

(defmethod MAIN::*movl
  ((?src reg/lit
         (is-valid-long-register ?current-argument))
   (?dst register
         (is-valid-long-register ?current-argument)))
  (definstruction movl
                  ?src
                  ?dst))
(defmethod MAIN::*movt
  ((?src reg/lit
         (is-valid-triple-register ?current-argument))
   (?dst register
         (is-valid-triple-register ?current-argument)))
  (definstruction movt
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
(defmethod MAIN::*movq
  ((?src reg/lit
         (is-valid-quad-register ?current-argument))
   (?dst register
         (is-valid-quad-register ?current-argument)))
  (definstruction movq
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

(defmethod MAIN::*atmod
  ((?src register)
   (?mask reg/lit)
   (?src/dst register))
  (definstruction atmod
                  ?src
                  ?mask
                  ?src/dst))

(defmethod MAIN::*atadd
  ((?src/dst register)
   (?src reg/lit)
   (?dst register))
  (definstruction atadd 
                  ?src/dst
                  ?src
                  ?dst))
(defmethod MAIN::*send
  ((?dst register)
   (?src1 reg/lit)
   (?src2 register))
  (definstruction send
                  ?dst
                  ?src1
                  ?src2))
; mem instructions
(defglobal MAIN 
           ?*store-ops* = (create$ st
                                   stob
                                   stib
                                   stos
                                   stis)
           ?*load-ops* = (create$ ld
                                  ldob
                                  ldib
                                  ldos
                                  ldis)
           ?*mem-other* = (create$ bx
                                   callx)

           )
(progn$ (?operation ?*mem-other*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             "(?targ mem-format-argument)"
                             "?targ"))
(progn$ (?operation ?*load-ops*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             (format nil
                                     "(?src mem-format-argument) %s"
                                     (string-arg-reg-field "?dst"))
                             (format nil
                                     "?src %s"
                                     (string-call-convert-register "?dst"))))

(progn$ (?operation ?*store-ops*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             (format nil
                                     "%s (?dst mem-format-argument)"
                                     (string-arg-reg-field "?src"))
                             (format nil
                                     "%s ?dst"
                                     (string-call-convert-register "?src"))))
(defmethod MAIN::*balx
  ((?targ mem-format-argument)
   (?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction balx
                  ?targ
                  (convert-register ?dst)))
(defmethod MAIN::*lda
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction lda
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldl
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument)))
  (definstruction ldl
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldt
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-triple-register ?current-argument)))
  (definstruction ldt
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldq
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-quad-register ?current-argument)))
  (definstruction ldq
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*stl
  ((?src register
         SYMBOL
         (is-valid-long-register ?current-argument))
   (?dest mem-format-argument))
  (definstruction stl
                  (convert-register ?src)
                  ?dest))
(defmethod MAIN::*stt
  ((?src register
         SYMBOL
         (is-valid-triple-register ?current-argument))
   (?dest mem-format-argument))
  (definstruction stt
                  (convert-register ?src)
                  ?dest))
(defmethod MAIN::*stq
  ((?src register
         SYMBOL
         (is-valid-quad-register ?current-argument))
   (?dest mem-format-argument))
  (definstruction stq
                  (convert-register ?src)
                  ?dest))
;synthetic instructions
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
  ((?lo reg/lit)
   (?target reg/lit)
   (?hi reg/lit))
  (create$ (*cmpo ?hi 
                  ?target)
           (*concmpo ?lo
                     ?target)))

(defmethod MAIN::*twixti
  ((?lo reg/lit)
   (?target reg/lit)
   (?hi reg/lit))
  (create$ (*cmpi ?hi 
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

; according to the manuals, the modpc instruction expects src to be the same as mask. Src is a dummy operand
(defmethod MAIN::*modpc
  ((?mask reg/lit)
   (?src/dest register))
  (*modpc ?mask
          ?mask
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
; make this last to be sure
(definstances MAIN::literals-and-registers
              (g0 of register
                  (valid-long-register-target TRUE)
                  (valid-triple-register-target TRUE)
                  (valid-quad-register-target TRUE)
                  (title g0))
              (g1 of register
                  (title g1))
              (g2 of register
                  (valid-long-register-target TRUE)
                  (title g2))
              (g3 of register
                  (title g3))
              (g4 of register
                  (valid-triple-register-target TRUE)
                  (valid-quad-register-target TRUE)
                  (valid-long-register-target TRUE)
                  (title g4))
              (g5 of register
                  (title g5))
              (g6 of register
                  (valid-long-register-target TRUE)
                  (title g6))
              (g7 of register
                  (title g7))
              (g8 of register
                  (valid-triple-register-target TRUE)
                  (valid-quad-register-target TRUE)
                  (valid-long-register-target TRUE)
                  (title g8))
              (g9 of register
                  (title g9))
              (g10 of register
                   (valid-long-register-target TRUE)
                   (title g10))
              (g11 of register
                   (title g11))
              (g12 of register
                   (valid-triple-register-target TRUE)
                   (valid-quad-register-target TRUE)
                   (valid-long-register-target TRUE)
                   (title g12))
              (g13 of register
                   (title g13))
              (g14 of register
                   (valid-long-register-target TRUE)
                   (title g14))
              (lr of register
                  (valid-long-register-target TRUE)
                  (title g14))
              (fp of register
                  (title fp))
              (pfp of register
                   (valid-long-register-target TRUE)
                   (valid-triple-register-target TRUE)
                   (valid-quad-register-target TRUE)
                   (title pfp))
              (sp of register
                  (title sp))
              (rip of register
                   (valid-long-register-target TRUE)
                   (title rip))
              (r3 of register
                  (title r3))
              (r4 of register
                  (valid-long-register-target TRUE)
                  (valid-triple-register-target TRUE)
                  (valid-quad-register-target TRUE)
                  (title r4))
              (r5 of register
                  (title r5))
              (r6 of register
                  (valid-long-register-target TRUE)
                  (title r6))
              (r7 of register
                  (title r7))
              (r8 of register
                  (valid-long-register-target TRUE)
                  (valid-triple-register-target TRUE)
                  (valid-quad-register-target TRUE)
                  (title r8))
              (r9 of register
                  (title r9))
              (r10 of register
                   (valid-long-register-target TRUE)
                   (title r10))
              (r11 of register
                   (title r11))
              (r12 of register
                   (valid-long-register-target TRUE)
                   (valid-triple-register-target TRUE)
                   (valid-quad-register-target TRUE)
                   (title r12))
              (r13 of register
                   (title r13))
              (r14 of register
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
