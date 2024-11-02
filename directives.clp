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
  ((?target SYMBOL
            INTEGER))
  (defdirective .word
                ?target))
(defmethod MAIN::.word
  ((?target SYMBOL
            INTEGER)
   (?rest MULTIFIELD))
  (defdirective .word
                ?target
                (expand$ ?rest)))
(defmethod MAIN::.word
  ((?target SYMBOL
            INTEGER)
   $?rest)
  (.word ?target
         ?rest))
