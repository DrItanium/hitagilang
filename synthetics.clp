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
(include opcodes.clp)
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
  (create$ (*xor (convert-register ?src1)
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
  (create$ (*xorl ?src1
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
  (create$ (*xorl ?src1
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
