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
(defclass MAIN::string-directive
  (is-a USER)
  (role abstract)
  (pattern-match non-reactive)
  (slot operation
        (type SYMBOL)
        (storage shared)
        (visibility public)
        (access read-only)
        (default ILLEGAL-OPERATION))
  (slot data
        (type STRING)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler to-string primary))
(defmessage-handler MAIN::string-directive to-string primary
                    ()
                    (format nil
                            "%s \"%s\""
                            (dynamic-get operation)
                            (dynamic-get data)))
(defclass MAIN::asciz-directive
  (is-a string-directive)
  (role concrete)
  (pattern-match reactive)
  (slot operation
        (source composite)
        (default .asciz)))
(defclass MAIN::ascii-directive
  (is-a string-directive)
  (role concrete)
  (pattern-match reactive)
  (slot operation
        (source composite)
        (default .ascii)))

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
  ((?target LEXEME
            INTEGER))
  (defdirective .word
                ?target))
(defmethod MAIN::.word
  ((?target LEXEME
            INTEGER)
   (?rest MULTIFIELD))
  (defdirective .word
                ?target
                (expand$ ?rest)))
(defmethod MAIN::.word
  ((?target LEXEME
            INTEGER)
   $?rest)
  (.word ?target
         ?rest))
(defmethod MAIN::.align
  ((?value INTEGER))
  (defdirective .align
                ?value))
(defmethod MAIN::.asciz
  ((?data STRING))
  (make-instance of asciz-directive
                 (data ?data)))

(defmethod MAIN::.ascii
  ((?data STRING))
  (make-instance of ascii-directive
                 (data ?data)))

(defmethod MAIN::.bss () (defdirective .bss))
(defmethod MAIN::.bss 
  ((?title SYMBOL)
   (?size INTEGER
          SYMBOL)
   (?alignment INTEGER))
  (defdirective .bss
                ?title
                ?size
                ?alignment))

(defmethod MAIN::.skip
  ((?count INTEGER))
  (defdirective .skip
                ?count))

(defmethod MAIN::.space
  ((?count INTEGER))
  (defdirective .space
                ?count))

(defmethod MAIN::defglobal-label
  ((?name SYMBOL))
  (create$ (.global ?name)
           (deflabel ?name)))

(defmethod MAIN::.fill
  ((?count INTEGER)
   (?size INTEGER)
   (?value INTEGER))
  (defdirective .fill
                ?count
                ?size
                ?value))
