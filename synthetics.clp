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
  (mkblock (*ldconst ?targ
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

(defmethod MAIN::*cmpolbne
  ((?src1 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?src2 register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?targ SYMBOL))
  (create$ (*cmpobne (convert-register ?src1)
                     (convert-register ?src2)
                     ?targ)
           (*cmpobne (send (convert-register ?src1)
                           get-next-register)
                     (send (convert-register ?src2)
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
