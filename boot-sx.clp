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
             (create$ (*ldconst 64 r4)
                      (*addo sp r4 sp)
                      (*stq g0 
                            (make-instance of mem-format-argument 
                                              (displacement -64) 
                                              (abase sp)))
                      (*stq g4 
                            (make-instance of mem-format-argument 
                                           (displacement -48) 
                                           (abase sp)))
                      (*stq g8 
                            (make-instance of mem-format-argument 
                                           (displacement -32) 
                                           (abase sp)))
                      (*stt g12 
                            (make-instance of mem-format-argument 
                                           (displacement -16) 
                                           (abase sp)))))
(deffunction MAIN::restore-globals
             ()
             (create$ (*ldq (make-instance of mem-format-argument 
                                           (displacement -64) 
                                           (abase sp))
                            g0)
                      (*ldq (make-instance of mem-format-argument 
                                           (displacement -48) 
                                           (abase sp))
                            g4)
                      (*ldq (make-instance of mem-format-argument 
                                           (displacement -32) 
                                           (abase sp))
                            g8)
                      (*ldt (make-instance of mem-format-argument 
                                           (displacement -16) 
                                           (abase sp))
                            g12)))

(deffunction MAIN:c-call
             (?function)
             (create$ (clear-g14)
                      (*call ?function)))
(deffunction MAIN::c-callx
             (?function)
             (create$ (clear-g14)
                      (*callx (make-instance of mem-format-argument
                                             (displacement ?function)))))

(deffunction MAIN::def-system-call
             (?index ?name)
             (create$ (.text)
                      (.align 4)
                      (.global ?name)
                      (deflabel ?name)
                      (if (<= 0 ?index 31) then
                        (*calls ?index)
                        else
                        (create$ (*ldconst ?index
                                  g13)
                                 (*calls g13)))
                      (*ret)))

