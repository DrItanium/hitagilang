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
