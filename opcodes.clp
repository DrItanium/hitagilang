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

(defmethod MAIN::*send
  ((?dst register)
   (?src1 reg/lit)
   (?src2 register))
  (definstruction send
                  ?dst
                  ?src1
                  ?src2))
(defgeneric MAIN::*b)
(defmethod MAIN::*b
  ((?targ SYMBOL))
  (definstruction b
                  ?targ))
(defgeneric MAIN::*call)
(defmethod MAIN::*call
  ((?targ SYMBOL))
  (definstruction call
                  ?targ))
(defgeneric MAIN::*bal)
(defmethod MAIN::*bal
  ((?targ SYMBOL))
  (definstruction bal
                  ?targ))
(defgeneric MAIN::*bno)
(defmethod MAIN::*bno
  ((?targ SYMBOL))
  (definstruction bno
                  ?targ))
(defgeneric MAIN::*bg)
(defmethod MAIN::*bg
  ((?targ SYMBOL))
  (definstruction bg
                  ?targ))
(defgeneric MAIN::*be)
(defmethod MAIN::*be
  ((?targ SYMBOL))
  (definstruction be
                  ?targ))
(defgeneric MAIN::*bge)
(defmethod MAIN::*bge
  ((?targ SYMBOL))
  (definstruction bge
                  ?targ))
(defgeneric MAIN::*bl)
(defmethod MAIN::*bl
  ((?targ SYMBOL))
  (definstruction bl
                  ?targ))
(defgeneric MAIN::*bne)
(defmethod MAIN::*bne
  ((?targ SYMBOL))
  (definstruction bne
                  ?targ))
(defgeneric MAIN::*ble)
(defmethod MAIN::*ble
  ((?targ SYMBOL))
  (definstruction ble
                  ?targ))
(defgeneric MAIN::*bo)
(defmethod MAIN::*bo
  ((?targ SYMBOL))
  (definstruction bo
                  ?targ))
(defgeneric MAIN::*bbs)
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
(defgeneric MAIN::*bbc)
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
(defgeneric MAIN::*cmpibe)
(defgeneric MAIN::*teste)
(defmethod MAIN::*cmpibe
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction cmpibe
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*teste
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction teste
                  (convert-register ?dst)
                  ))
(defgeneric MAIN::*cmpibne)
(defgeneric MAIN::*testne)
(defmethod MAIN::*cmpibne
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction cmpibne
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*testne
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction testne
                  (convert-register ?dst)
                  ))
(defgeneric MAIN::*cmpibl)
(defgeneric MAIN::*testl)
(defmethod MAIN::*cmpibl
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction cmpibl
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*testl
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction testl
                  (convert-register ?dst)
                  ))
(defgeneric MAIN::*cmpible)
(defgeneric MAIN::*testle)
(defmethod MAIN::*cmpible
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction cmpible
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*testle
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction testle
                  (convert-register ?dst)
                  ))
(defgeneric MAIN::*cmpibg)
(defgeneric MAIN::*testg)
(defmethod MAIN::*cmpibg
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction cmpibg
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*testg
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction testg
                  (convert-register ?dst)
                  ))
(defgeneric MAIN::*cmpibge)
(defgeneric MAIN::*testge)
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
                  (convert-register ?dst)
                  ))
(defgeneric MAIN::*cmpibo)
(defgeneric MAIN::*testo)
(defmethod MAIN::*cmpibo
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction cmpibo
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*testo
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction testo
                  (convert-register ?dst)
                  ))
(defgeneric MAIN::*cmpibno)
(defgeneric MAIN::*testno)
(defmethod MAIN::*cmpibno
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction cmpibno
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defmethod MAIN::*testno
  ((?dst register
         SYMBOL
         (is-valid-register ?current-argument)))
  (definstruction testno
                  (convert-register ?dst)
                  ))
(defgeneric MAIN::*cmpobe)
(defmethod MAIN::*cmpobe
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction cmpobe
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defgeneric MAIN::*cmpobne)
(defmethod MAIN::*cmpobne
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction cmpobne
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defgeneric MAIN::*cmpobl)
(defmethod MAIN::*cmpobl
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction cmpobl
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defgeneric MAIN::*cmpoble)
(defmethod MAIN::*cmpoble
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction cmpoble
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defgeneric MAIN::*cmpobg)
(defmethod MAIN::*cmpobg
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction cmpobg
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defgeneric MAIN::*cmpobge)
(defmethod MAIN::*cmpobge
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 register
          SYMBOL
          (is-valid-register ?current-argument)) (?targ SYMBOL))
  (definstruction cmpobge
                  (convert-reg/lit ?src1)
                  (convert-register ?src2)
                  ?targ))
(defgeneric MAIN::*scanbyte)
(defmethod MAIN::*scanbyte
  ((?src1 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)) (?src2 reg/lit
          INTEGER
          SYMBOL
          (is-valid-reg-literal ?current-argument)))
  (definstruction scanbyte
                  (convert-reg/lit ?src1)
                  (convert-reg/lit ?src2)
                  ))
(defgeneric MAIN::*mulr)
(defgeneric MAIN::*mulrl)
(defmethod MAIN::*mulr
  ((?src1 freg/flit) (?src2 freg/flit) (?dst freg))
  (definstruction mulr
                  ?src1 ?src2 ?dst))
(defmethod MAIN::*mulrl
  ((?src1 freg/flit (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit (is-valid-long-register ?current-argument)) 
   (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction mulrl
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*remr)
(defgeneric MAIN::*remrl)
(defmethod MAIN::*remr
  ((?src1 freg/flit) (?src2 freg/flit) (?dst freg))
  (definstruction remr
                  ?src1 ?src2 ?dst))
(defmethod MAIN::*remrl
  ((?src1 freg/flit (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit (is-valid-long-register ?current-argument)) 
   (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction remrl
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*logr)
(defgeneric MAIN::*logrl)
(defmethod MAIN::*logr
  ((?src1 freg/flit) (?src2 freg/flit) (?dst freg))
  (definstruction logr
                  ?src1 ?src2 ?dst))
(defmethod MAIN::*logrl
  ((?src1 freg/flit (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit (is-valid-long-register ?current-argument)) 
   (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction logrl
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*logepr)
(defgeneric MAIN::*logeprl)
(defmethod MAIN::*logepr
  ((?src1 freg/flit) (?src2 freg/flit) (?dst freg))
  (definstruction logepr
                  ?src1 ?src2 ?dst))
(defmethod MAIN::*logeprl
  ((?src1 freg/flit (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit (is-valid-long-register ?current-argument)) 
   (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction logeprl
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*divr)
(defgeneric MAIN::*divrl)
(defmethod MAIN::*divr
  ((?src1 freg/flit) (?src2 freg/flit) (?dst freg))
  (definstruction divr
                  ?src1 ?src2 ?dst))
(defmethod MAIN::*divrl
  ((?src1 freg/flit (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit (is-valid-long-register ?current-argument)) 
   (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction divrl
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*subr)
(defgeneric MAIN::*subrl)
(defmethod MAIN::*subr
  ((?src1 freg/flit) (?src2 freg/flit) (?dst freg))
  (definstruction subr
                  ?src1 ?src2 ?dst))
(defmethod MAIN::*subrl
  ((?src1 freg/flit (is-valid-long-register ?current-argument)) 
   (?src2 freg/flit (is-valid-long-register ?current-argument)) 
   (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction subrl
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*dmovt)
(defmethod MAIN::*dmovt
  ((?src register) (?dest register))
  (definstruction dmovt
                  ?src ?dest))
(defgeneric MAIN::*synld)
(defmethod MAIN::*synld
  ((?src register) (?dest register))
  (definstruction synld
                  ?src ?dest))
(defgeneric MAIN::*condrec)
(defmethod MAIN::*condrec
  ((?src register) (?dest register))
  (definstruction condrec
                  ?src ?dest))
(defgeneric MAIN::*receive)
(defmethod MAIN::*receive
  ((?src register) (?dest register))
  (definstruction receive
                  ?src ?dest))
(defgeneric MAIN::*synmov)
(defmethod MAIN::*synmov
  ((?src register) (?dest register))
  (definstruction synmov
                  ?src ?dest))
(defgeneric MAIN::*synmovl)
(defmethod MAIN::*synmovl
  ((?src register) (?dest register))
  (definstruction synmovl
                  ?src ?dest))
(defgeneric MAIN::*synmovq)
(defmethod MAIN::*synmovq
  ((?src register) (?dest register))
  (definstruction synmovq
                  ?src ?dest))
(defgeneric MAIN::*ldphy)
(defmethod MAIN::*ldphy
  ((?src register) (?dest register))
  (definstruction ldphy
                  ?src ?dest))
(defgeneric MAIN::*daddc)
(defmethod MAIN::*daddc
  ((?src1 register) (?src2 register) (?dest register))
  (definstruction daddc
                  ?src1 ?src2 ?dest))
(defgeneric MAIN::*dsubc)
(defmethod MAIN::*dsubc
  ((?src1 register) (?src2 register) (?dest register))
  (definstruction dsubc
                  ?src1 ?src2 ?dest))
(defgeneric MAIN::*notbit)
(defmethod MAIN::*notbit
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction notbit
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*setbit)
(defmethod MAIN::*setbit
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction setbit
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*notand)
(defmethod MAIN::*notand
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction notand
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*xor)
(defmethod MAIN::*xor
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction xor
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*or)
(defmethod MAIN::*or
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction or
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*nor)
(defmethod MAIN::*nor
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction nor
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*xnor)
(defmethod MAIN::*xnor
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction xnor
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*ornot)
(defmethod MAIN::*ornot
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction ornot
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*notor)
(defmethod MAIN::*notor
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction notor
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*nand)
(defmethod MAIN::*nand
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction nand
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*subo)
(defmethod MAIN::*subo
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction subo
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*subi)
(defmethod MAIN::*subi
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction subi
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*shro)
(defmethod MAIN::*shro
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction shro
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*shrdi)
(defmethod MAIN::*shrdi
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction shrdi
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*shri)
(defmethod MAIN::*shri
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction shri
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*shlo)
(defmethod MAIN::*shlo
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction shlo
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*rotate)
(defmethod MAIN::*rotate
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction rotate
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*shli)
(defmethod MAIN::*shli
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction shli
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*subc)
(defmethod MAIN::*subc
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction subc
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*modac)
(defmethod MAIN::*modac
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction modac
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*modify)
(defmethod MAIN::*modify
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction modify
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*extract)
(defmethod MAIN::*extract
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction extract
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*modtc)
(defmethod MAIN::*modtc
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction modtc
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*modpc)
(defmethod MAIN::*modpc
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction modpc
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*mulo)
(defmethod MAIN::*mulo
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction mulo
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*remo)
(defmethod MAIN::*remo
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction remo
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*divo)
(defmethod MAIN::*divo
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction divo
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*muli)
(defmethod MAIN::*muli
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction muli
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*remi)
(defmethod MAIN::*remi
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction remi
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*modi)
(defmethod MAIN::*modi
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction modi
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*divi)
(defmethod MAIN::*divi
  ((?src1 reg/lit) (?src2 reg/lit) (?dst register))
  (definstruction divi
                  ?src1 ?src2 ?dst))
(defgeneric MAIN::*ret)
(defmethod MAIN::*ret
  ()
  (definstruction ret
                  ))
(defgeneric MAIN::*fmark)
(defmethod MAIN::*fmark
  ()
  (definstruction fmark
                  ))
(defgeneric MAIN::*mark)
(defmethod MAIN::*mark
  ()
  (definstruction mark
                  ))
(defgeneric MAIN::*syncf)
(defmethod MAIN::*syncf
  ()
  (definstruction syncf
                  ))
(defgeneric MAIN::*flushreg)
(defmethod MAIN::*flushreg
  ()
  (definstruction flushreg
                  ))
(defgeneric MAIN::*faultno)
(defmethod MAIN::*faultno
  ()
  (definstruction faultno
                  ))
(defgeneric MAIN::*faultg)
(defmethod MAIN::*faultg
  ()
  (definstruction faultg
                  ))
(defgeneric MAIN::*faulte)
(defmethod MAIN::*faulte
  ()
  (definstruction faulte
                  ))
(defgeneric MAIN::*faultge)
(defmethod MAIN::*faultge
  ()
  (definstruction faultge
                  ))
(defgeneric MAIN::*faultl)
(defmethod MAIN::*faultl
  ()
  (definstruction faultl
                  ))
(defgeneric MAIN::*faultne)
(defmethod MAIN::*faultne
  ()
  (definstruction faultne
                  ))
(defgeneric MAIN::*faultle)
(defmethod MAIN::*faultle
  ()
  (definstruction faultle
                  ))
(defgeneric MAIN::*faulto)
(defmethod MAIN::*faulto
  ()
  (definstruction faulto
                  ))
(defgeneric MAIN::*saveprcs)
(defmethod MAIN::*saveprcs
  ()
  (definstruction saveprcs
                  ))
(defgeneric MAIN::*not)
(defmethod MAIN::*not
  ((?src reg/lit) (?dst register))
  (definstruction not
                  ?src ?dst))
(defgeneric MAIN::*mov)
(defmethod MAIN::*mov
  ((?src reg/lit) (?dst register))
  (definstruction mov
                  ?src ?dst))
(defgeneric MAIN::*spanbit)
(defmethod MAIN::*spanbit
  ((?src reg/lit) (?dst register))
  (definstruction spanbit
                  ?src ?dst))
(defgeneric MAIN::*scanbit)
(defmethod MAIN::*scanbit
  ((?src reg/lit) (?dst register))
  (definstruction scanbit
                  ?src ?dst))
(defgeneric MAIN::*movqstr)
(defmethod MAIN::*movqstr
  ((?dst register) (?src register) (?len reg/lit))
  (definstruction movqstr
                  ?dst ?src ?len))
(defgeneric MAIN::*movstr)
(defmethod MAIN::*movstr
  ((?dst register) (?src register) (?len reg/lit))
  (definstruction movstr
                  ?dst ?src ?len))
(defgeneric MAIN::*movr)
(defgeneric MAIN::*movrl)
(defmethod MAIN::*movr
  ((?src freg/flit) (?dst freg))
  (definstruction movr
                  ?src ?dst))
(defmethod MAIN::*movrl
  ((?src freg/flit (is-valid-long-register ?current-argument)) (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction movrl
                  ?src ?dst))
(defgeneric MAIN::*cosr)
(defgeneric MAIN::*cosrl)
(defmethod MAIN::*cosr
  ((?src freg/flit) (?dst freg))
  (definstruction cosr
                  ?src ?dst))
(defmethod MAIN::*cosrl
  ((?src freg/flit (is-valid-long-register ?current-argument)) (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction cosrl
                  ?src ?dst))
(defgeneric MAIN::*sinr)
(defgeneric MAIN::*sinrl)
(defmethod MAIN::*sinr
  ((?src freg/flit) (?dst freg))
  (definstruction sinr
                  ?src ?dst))
(defmethod MAIN::*sinrl
  ((?src freg/flit (is-valid-long-register ?current-argument)) (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction sinrl
                  ?src ?dst))
(defgeneric MAIN::*expr)
(defgeneric MAIN::*exprl)
(defmethod MAIN::*expr
  ((?src freg/flit) (?dst freg))
  (definstruction expr
                  ?src ?dst))
(defmethod MAIN::*exprl
  ((?src freg/flit (is-valid-long-register ?current-argument)) (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction exprl
                  ?src ?dst))
(defgeneric MAIN::*logbnr)
(defgeneric MAIN::*logbnrl)
(defmethod MAIN::*logbnr
  ((?src freg/flit) (?dst freg))
  (definstruction logbnr
                  ?src ?dst))
(defmethod MAIN::*logbnrl
  ((?src freg/flit (is-valid-long-register ?current-argument)) (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction logbnrl
                  ?src ?dst))
(defgeneric MAIN::*roundr)
(defgeneric MAIN::*roundrl)
(defmethod MAIN::*roundr
  ((?src freg/flit) (?dst freg))
  (definstruction roundr
                  ?src ?dst))
(defmethod MAIN::*roundrl
  ((?src freg/flit (is-valid-long-register ?current-argument)) (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction roundrl
                  ?src ?dst))
(defgeneric MAIN::*sqrtr)
(defgeneric MAIN::*sqrtrl)
(defmethod MAIN::*sqrtr
  ((?src freg/flit) (?dst freg))
  (definstruction sqrtr
                  ?src ?dst))
(defmethod MAIN::*sqrtrl
  ((?src freg/flit (is-valid-long-register ?current-argument)) (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction sqrtrl
                  ?src ?dst))
(defgeneric MAIN::*tanr)
(defgeneric MAIN::*tanrl)
(defmethod MAIN::*tanr
  ((?src freg/flit) (?dst freg))
  (definstruction tanr
                  ?src ?dst))
(defmethod MAIN::*tanrl
  ((?src freg/flit (is-valid-long-register ?current-argument)) (?dst freg (is-valid-long-register ?current-argument)))
  (definstruction tanrl
                  ?src ?dst))
(defgeneric MAIN::*condwait)
(defmethod MAIN::*condwait
  ((?src register))
  (definstruction condwait
                  ?src))
(defgeneric MAIN::*schedprcs)
(defmethod MAIN::*schedprcs
  ((?src register))
  (definstruction schedprcs
                  ?src))
(defgeneric MAIN::*sendserv)
(defmethod MAIN::*sendserv
  ((?src register))
  (definstruction sendserv
                  ?src))
(defgeneric MAIN::*signal)
(defmethod MAIN::*signal
  ((?src register))
  (definstruction signal
                  ?src))
(defgeneric MAIN::*wait)
(defmethod MAIN::*wait
  ((?src register))
  (definstruction wait
                  ?src))
(defgeneric MAIN::*ldtime)
(defmethod MAIN::*ldtime
  ((?src register))
  (definstruction ldtime
                  ?src))
(defgeneric MAIN::*resumprcs)
(defmethod MAIN::*resumprcs
  ((?src register))
  (definstruction resumprcs
                  ?src))
