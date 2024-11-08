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
; routines to generate all of the instructions
(batch* asmgen.clp)
(defglobal MAIN
           ?*format-builder-output* = stdout
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
                                                      ldphy
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
                                        ldtime
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
           ?*arg-reg-lit-field* = "(%s reg/lit%nINTEGER%nSYMBOL%n(is-valid-reg-literal ?current-argument))"
           ?*arg-reg-field* = "(%s register%nSYMBOL%n(is-valid-register ?current-argument))"
           ?*call-convert-reg-lit* = "(convert-reg/lit %s)%n"
           ?*call-convert-register* = "(convert-register %s)%n"
           )
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
                                     "(?src mem-format-argument)%n%s"
                                     (string-arg-reg-field "?dst"))
                             (format nil
                                     "?src %s"
                                     (string-call-convert-register "?dst"))))

(progn$ (?operation ?*store-ops*)
        (forward-declare-opcode ?operation)
        (generic-opcode-decl ?operation
                             (format nil
                                     "%s%n(?dst mem-format-argument)"
                                     (string-arg-reg-field "?src"))
                             (format nil
                                     "%s ?dst"
                                     (string-call-convert-register "?src"))))
