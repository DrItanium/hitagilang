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

(defclass MAIN::has-parent
  (is-a USER)
  (slot parent
        (type SYMBOL
              INSTANCE)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local)))
(defclass MAIN::has-title
  (is-a USER)
  (slot title
        (type LEXEME)
        (visibility public)
        (storage local)
        (default-dynamic (gensym*))))
(defclass MAIN::has-scope
  (is-a USER)
  (multislot contents
             (storage local)
             (visibility public)))
(defclass MAIN::has-arguments
  (is-a USER)
  (multislot arguments
             (storage local)
             (visibility public)))

(defclass MAIN::invocation
  (is-a has-parent
        has-arguments)
  (slot destination
        (type LEXEME
              INSTANCE)
        (storage local)
        (visibility public)
        (default ?NONE)))

(defclass MAIN::executable-sequence
  (is-a has-parent
        has-title
        has-scope))
(defclass MAIN::executable-sequence-with-arguments
  (is-a executable-sequence
        has-arguments))
; A site is a block of executable code which once left cannot be returned to from the point being left
; It is an error to return to the site or fallthrough it. All execution flow must be explicitly handled. 
; When the i960 starts up, it conceptually starts executing from a site. 
;
; Call sites must do everything manually so you enter them through normal branches or conditional jumps. 
;
(defclass MAIN::site
  (is-a executable-sequence-with-arguments))

; A leaf function is a self contained routine which does _not_ take advantage of the register window concept of the i960. 
; Execution can fallthrough the bottom as it will use the bx to the target register. It is safe to pass control to other executable-sequences
; A leaf function is implied to be returned to from a subinvocation. The instructions used to enter a leaf are bal/balx with bx used to leave the leaf
(defclass MAIN::leaf
  (is-a executable-sequence-with-arguments))

; A method is a routine which uses the register window concept of the i960. This is the usual routine type so it has a very normal name.
; One can return to methods and invoke other types of executable sequences. A method is entered via call/callx and left via ret. Routines in the 
; interrupt and fault tables must be methods. This allows for proper functionality according to how the i960 works.
(defclass MAIN::method
  (is-a executable-sequence-with-arguments))

