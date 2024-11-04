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
(include asmgen.clp)

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
          (is-valid-reg-literal ?current-argument)) (?src2 reg/lit
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
          (is-valid-reg-literal ?current-argument)) (?src2 reg/lit
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

