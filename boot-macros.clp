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
(include assembler.clp)
(deffunction MAIN::clear-g14 () (*ldconst 0 g14))
(deffunction MAIN::save-globals
             ()
             (mkblock (*ldconst 64 r4)
                      (*addo sp r4 sp)
                      (*stq g0 
                            (mem-format-arg displacement: -64
                                            abase: sp))
                      (*stq g4 
                            (mem-format-arg displacement: -48
                                            abase: sp))
                      (*stq g8 
                            (mem-format-arg displacement: -32
                                            abase: sp))
                      (*stt g12 
                            (mem-format-arg displacement: -16
                                            abase: sp))))
(deffunction MAIN::restore-globals
             ()
             (mkblock (*ldq (mem-format-arg displacement: -64
                                            abase: sp)
                            g0)
                      (*ldq (mem-format-arg displacement: -48
                                            abase: sp)
                            g4)
                      (*ldq (mem-format-arg displacement: -32
                                            abase: sp)
                            g8)
                      (*ldt (mem-format-arg displacement: -16
                                            abase: sp)
                            g12)))

(deffunction MAIN::clear-call
             (?function)
             (mkblock (clear-g14)
                      (*call ?function)))
(deffunction MAIN::clear-callx
             (?function)
             (mkblock (clear-g14)
                      (*callx (mem-format-arg displacement: ?function))))

(deffunction MAIN::def-system-call
             (?index ?name)
             (mkblock (.text)
                      (.align 4)
                      (defglobal-label ?name)
                      (*calls ?index)
                      (*ret)))
(deffunction MAIN::declare-segment
             (?a ?b ?c ?d)
             (.word ?a 
                    ?b 
                    ?c 
                    ?d))
(deffunction MAIN::segment-selector
             (?base)
             (.word (format nil
                            "((%d)<<6) | 0x3f"
                            ?base)))
(deffunction MAIN::simple-region
             (?address)
             (declare-segment 0 
                              0 
                              ?address 
                              0x00fc00a3))
(deffunction MAIN::paged-region
             (?address ?size)
             (mkblock (.space 8)
                      (.word ?address
                             (format nil
                                     "((%d) << 18) | 0x5"
                                     ?size))))
(deffunction MAIN::bipaged-region
             (?address ?size)
             (mkblock (.space 8)
                      (.word ?address
                             (format nil
                                     "((%d) << 18) | 0x7"
                                     ?size))))

(deffunction MAIN::small-segment-table
             (?address ?size)
             (mkblock (.space 8)
                      (.word ?address
                             "(0x3f << 18) | 0xfb")))

(deffunction MAIN::page-entry
             (?address)
             (.word (str-cat "((" ?address ") | 0xc7)")))
(deffunction MAIN::port-segment
             (?address)
             (declare-segment 0 
                              0
                              ?address
                              0x204000fb))
(deffunction MAIN::def-interrupt-handler
             (?name ?to-call)
             (mkblock (defglobal-label ?name)
                      (save-globals)
                      (clear-call (sym-cat _vect_ 
                                           ?to-call))
                      (restore-globals)
                      (*ret)))
