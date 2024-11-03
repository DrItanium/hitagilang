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
(defclass MAIN::mem-format-argument
  (is-a USER)
  (slot displacement
        (type SYMBOL
              INTEGER)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (slot abase
        (type INSTANCE
              SYMBOL)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (slot index
        (type INSTANCE
              SYMBOL)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (slot scale
        (type INTEGER)
        (allowed-integers 1
                          2
                          4
                          8
                          16)
        (storage local)
        (visibility public)
        (default-dynamic 1))
  (message-handler init after)
  (message-handler to-string primary))
(defmessage-handler MAIN::mem-format-argument init after
                    ()
                    (if (and ?self:abase 
                             (is-valid-register ?self:index)) then
                      (dynamic-put abase
                                   (convert-register (dynamic-get abase))))
                    (if (and ?self:index
                             (is-valid-register ?self:index)) then
                      (dynamic-put index
                                   (convert-register (dynamic-get index))))
                    )

(defmessage-handler MAIN::mem-format-argument to-string primary
                    ()
                    (format nil
                            "%s%s%s"
                            (if ?self:displacement then
                              (if (integerp ?self:displacement) then
                                (format nil
                                        "0x%x"
                                        ?self:displacement)
                                else
                                ?self:displacement)
                              else
                              "")
                            (if ?self:abase then
                              (format nil 
                                      "(%s)"
                                      (send ?self:abase
                                            to-string))
                              else
                              "")
                            (if ?self:index then
                              (format nil
                                      "[%s%s]"
                                      (send ?self:index
                                            to-string)
                                      (if (<> ?self:scale 1) then
                                        (format nil 
                                                "*%d"
                                                ?self:scale)
                                        else
                                        ""))
                              else
                              "")
                            ))
(defgeneric MAIN::mem-format-arg
            "construct the mem-format-argument object in a cleaner way")
; MEMA format
; -------------------
; Register Indirect with Offset { abase + offset }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?abase-txt SYMBOL
               (eq ?current-argument
                   abase:))
   (?abase SYMBOL
           register
           (is-valid-register ?current-argument))
   (?offset-txt SYMBOL
                (eq ?current-argument
                    offset:))
   (?offset INTEGER
            (<= 0 ?current-argument 4095)))
  (make-instance of mem-format-argument
                 (abase ?abase)
                 (displacement ?offset)))
; -------------------
; Absolute Offset { offset }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?offset-txt SYMBOL
                (eq ?current-argument
                    offset:))
   (?offset INTEGER
            (<= 0 ?current-argument 4095)))
  (make-instance of mem-format-argument
                 (displacement ?offset)))
; -------------------
; MEMB Format
; -------------------
; Register Indirect with Index and Displacement { (abase) + (index) * 2^scale + displacement }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?txt-displacement SYMBOL
                      (eq ?current-argument 
                          displacement:))
   (?displacement SYMBOL
                  INTEGER)
   (?txt-abase SYMBOL
               (eq ?current-argument
                   abase:))
   (?abase SYMBOL
           register
           (is-valid-register ?current-argument))
   (?txt-index SYMBOL
               (eq ?current-argument
                   index:))
   (?index SYMBOL
           register
           (is-valid-register ?current-argument))
   (?txt-scale SYMBOL
               (eq ?current-argument
                   scale:))
   (?scale INTEGER))
  ; if displacement is zero then do not include it to save a word
  ; use Register Indirect with Index form
  (make-instance of mem-format-argument
                 (displacement (if (eq ?displacement 
                                       0) then 
                                 FALSE 
                                 else 
                                 ?displacement))
                 (abase ?abase)
                 (index ?index)
                 (scale (switch ?scale
                                (case 2 then ?scale)
                                (case 4 then ?scale)
                                (case 8 then ?scale)
                                (case 16 then ?scale)
                                (default FALSE)))))
; -------------------
; Index with Displacement { (index) * 2^scale + displacement }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?txt-displacement SYMBOL
                      (eq ?current-argument 
                          displacement:))
   (?displacement SYMBOL
                  INTEGER)
   (?txt-index SYMBOL
               (eq ?current-argument
                   index:))
   (?index SYMBOL
           register
           (is-valid-register ?current-argument))
   (?txt-scale SYMBOL
               (eq ?current-argument
                   scale:))
   (?scale INTEGER))
  (make-instance of mem-format-argument
                 (displacement ?displacement)
                 (index ?index)
                 (scale (switch ?scale
                                (case 2 then ?scale)
                                (case 4 then ?scale)
                                (case 8 then ?scale)
                                (case 16 then ?scale)
                                (default FALSE)))))
; -------------------
; Register Indirect with Displacement { (abase) + displacement }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?txt-displacement SYMBOL
                      (eq ?current-argument 
                          displacement:))
   (?displacement SYMBOL
                  INTEGER)
   (?txt-abase SYMBOL
               (eq ?current-argument
                   abase:))
   (?abase SYMBOL
           register
           (is-valid-register ?current-argument)))
  ; if displacement is zero then change the form to Register Indirect
  ; save instruction space
  (make-instance of mem-format-argument
                 (displacement (if (eq ?displacement 
                                       0) then 
                                 FALSE 
                                 else 
                                 ?displacement))
                 (abase ?abase)))
; -------------------
; Absolute Displacement { displacement } / IP with Displacement  { (IP) + displacement + 8 }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?txt-displacement SYMBOL
                      (eq ?current-argument 
                          displacement:))
   (?displacement SYMBOL
                  INTEGER))
  (make-instance of mem-format-argument
                 (displacement ?displacement)))
; -------------------
; Register Indirect with Index { (abase) + (index) * 2^scale }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?txt-abase SYMBOL
               (eq ?current-argument
                   abase:))
   (?abase SYMBOL
           register
           (is-valid-register ?current-argument))
   (?txt-index SYMBOL
               (eq ?current-argument
                   index:))
   (?index SYMBOL
           register
           (is-valid-register ?current-argument))
   (?txt-scale SYMBOL
               (eq ?current-argument
                   scale:))
   (?scale INTEGER))
  (make-instance of mem-format-argument
                 (abase ?abase)
                 (index ?index)
                 (scale (switch ?scale
                                (case 2 then ?scale)
                                (case 4 then ?scale)
                                (case 8 then ?scale)
                                (case 16 then ?scale)
                                (default FALSE)))))
; -------------------
; Register Indirect { (abase) }
; -------------------
(defmethod MAIN::mem-format-arg
  ((?txt-abase SYMBOL
               (eq ?current-argument
                   abase:))
   (?abase SYMBOL
           register
           (is-valid-register ?current-argument)))
  (make-instance of mem-format-argument
                 (abase ?abase)))

; -------------------
(defgeneric MAIN::*bx)
(defgeneric MAIN::*callx)
(defgeneric MAIN::*ld)
(defgeneric MAIN::*lda)
(defgeneric MAIN::*ld)
(defgeneric MAIN::*ldl)
(defgeneric MAIN::*ldt)
(defgeneric MAIN::*ldq)
(defgeneric MAIN::*ldos)
(defgeneric MAIN::*ldis)
(defgeneric MAIN::*ldob)
(defgeneric MAIN::*ldib)
; -------------------
; lda
; -------------------
(defmethod MAIN::*lda
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction lda
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*lda
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   (?rest MULTIFIELD))
  (*lda (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*lda
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   $?rest)
  (*lda ?txt-dest
        ?dest
        ?rest))

; -------------------
; balx
; -------------------
(defmethod MAIN::*balx
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction balx
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*balx
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   (?rest MULTIFIELD))
  (*balx (mem-format-arg (expand$ ?rest))
         ?dest))
(defmethod MAIN::*balx
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   $?rest)
  (*balx ?txt-dest ?dest
         ?rest))
; -------------------
; ldl
; -------------------
(defmethod MAIN::*ldl
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument)))
  (definstruction ldl
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldl
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument))
   (?rest MULTIFIELD))
  (*ldl (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ldl
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-long-register ?current-argument))
   $?rest)
  (*ldl ?txt-dest
        ?dest
        ?rest))

; -------------------
; ldt
; -------------------
(defmethod MAIN::*ldt
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-triple-register ?current-argument)))
  (definstruction ldt
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldt
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   (?rest MULTIFIELD))
  (*ldt (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ldt
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-triple-register ?current-argument))
   $?rest)
  (*ldt ?txt-dest
        ?dest
        ?rest))

; -------------------
; ldq
; -------------------
(defmethod MAIN::*ldq
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-quad-register ?current-argument)))
  (definstruction ldq
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldq
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   (?rest MULTIFIELD))
  (*ldq (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ldq
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-quad-register ?current-argument))
   $?rest)
  (*ldq ?txt-dest
        ?dest
        ?rest))

; -------------------
; ld
; -------------------
(defmethod MAIN::*ld
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction ld
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ld
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   (?rest MULTIFIELD))
  (*ld (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ld
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   $?rest)
  (*ld ?txt-dest
        ?dest
        ?rest))

; -------------------
; ldis
; -------------------
(defmethod MAIN::*ldis
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction ldis
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldis
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   (?rest MULTIFIELD))
  (*ldis (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ldis
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   $?rest)
  (*ldis ?txt-dest
        ?dest
        ?rest))

; -------------------
; ldos
; -------------------
(defmethod MAIN::*ldos
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction ldos
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldos
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   (?rest MULTIFIELD))
  (*ldos (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ldos
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   $?rest)
  (*ldos ?txt-dest
        ?dest
        ?rest))

; -------------------
; ldib
; -------------------
(defmethod MAIN::*ldib
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction ldib
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldib
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   (?rest MULTIFIELD))
  (*ldib (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ldib
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   $?rest)
  (*ldib ?txt-dest
        ?dest
        ?rest))

; -------------------
; ldob
; -------------------
(defmethod MAIN::*ldob
  ((?src mem-format-argument)
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument)))
  (definstruction ldob
                  ?src
                  (convert-register ?dest)))
(defmethod MAIN::*ldob
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   (?rest MULTIFIELD))
  (*ldob (mem-format-arg (expand$ ?rest))
        ?dest))
(defmethod MAIN::*ldob
  ((?txt-dest SYMBOL
              (eq ?current-argument
                  dest:))
   (?dest register
          SYMBOL
          (is-valid-register ?current-argument))
   $?rest)
  (*ldob ?txt-dest
        ?dest
        ?rest))

; -------------------
; bx
; -------------------
(defmethod MAIN::*bx
  ((?targ mem-format-argument))
  (definstruction bx
                  ?targ))
(defmethod MAIN::*bx
  ((?rest MULTIFIELD))
  (*bx (mem-format-arg (expand$ ?rest))))
(defmethod MAIN::*bx
  ($?rest)
  (*bx ?rest))
; -------------------
; callx
; -------------------
(defmethod MAIN::*callx
  ((?targ mem-format-argument))
  (definstruction callx
                  ?targ))
(defmethod MAIN::*callx
  ((?rest MULTIFIELD))
  (*callx (mem-format-arg (expand$ ?rest))))
(defmethod MAIN::*callx
  ($?rest)
  (*callx ?rest))


; mem instructions
; -------------------
; stl
; -------------------
(defmethod MAIN::*stl
  ((?src register
         SYMBOL
         (is-valid-long-register ?current-argument))
   (?dest mem-format-argument))
  (definstruction stl
                  (convert-register ?src)
                  ?dest))
(defmethod MAIN::*stl
  ((?src register
         SYMBOL
         (is-valid-long-register ?current-argument))
   (?rest MULTIFIELD))
  (*stl ?src
        (mem-format-arg (expand$ ?rest))))

(defmethod MAIN::*stl
  ((?src register
         SYMBOL
         (is-valid-long-register ?current-argument))
   $?rest)
  (*stl ?src
        ?rest))

; -------------------
; stt
; -------------------
(defmethod MAIN::*stt
  ((?src register
         SYMBOL
         (is-valid-triple-register ?current-argument))
   (?dest mem-format-argument))
  (definstruction stt
                  (convert-register ?src)
                  ?dest))
(defmethod MAIN::*stt
  ((?src register
         SYMBOL
         (is-valid-triple-register ?current-argument))
   (?rest MULTIFIELD))
  (*stt ?src
        (mem-format-arg (expand$ ?rest))))
(defmethod MAIN::*stt
  ((?src register
         SYMBOL
         (is-valid-triple-register ?current-argument))
   $?rest)
  (*stt ?src
        ?rest))
; -------------------
; stq
; -------------------
(defmethod MAIN::*stq
  ((?src register
         SYMBOL
         (is-valid-quad-register ?current-argument))
   (?dest mem-format-argument))
  (definstruction stq
                  (convert-register ?src)
                  ?dest))

(defmethod MAIN::*stq
  ((?src register
         SYMBOL
         (is-valid-quad-register ?current-argument))
   (?rest MULTIFIELD))
  (*stq ?src
        (mem-format-arg (expand$ ?rest))))

(defmethod MAIN::*stq
  ((?src register
         SYMBOL
         (is-valid-quad-register ?current-argument))
   $?rest)
  (*stq ?src
        (mem-format-arg (expand$ ?rest))))

; -------------------
; st
; -------------------
(defmethod MAIN::*st
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst mem-format-argument))
  (definstruction st
                  (convert-register ?src)
                  ?dst))
(defmethod MAIN::*st
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst MULTIFIELD))
  (*st ?src
       (mem-format-arg (expand$ ?dst))))
(defmethod MAIN::*st
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   $?dst)
  (*st ?src
       ?dst))
; -------------------
; stob
; -------------------
(defmethod MAIN::*stob
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst mem-format-argument))
  (definstruction stob
                  (convert-register ?src)
                  ?dst))
(defmethod MAIN::*stob
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst MULTIFIELD))
  (*stob ?src
       (mem-format-arg (expand$ ?dst))))
(defmethod MAIN::*stob
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   $?dst)
  (*stob ?src
       ?dst))
; -------------------
; stib
; -------------------
(defmethod MAIN::*stib
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst mem-format-argument))
  (definstruction stib
                  (convert-register ?src)
                  ?dst))
(defmethod MAIN::*stib
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst MULTIFIELD))
  (*stib ?src
       (mem-format-arg (expand$ ?dst))))
(defmethod MAIN::*stib
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   $?dst)
  (*stib ?src
       ?dst))
; -------------------
; stos
; -------------------
(defmethod MAIN::*stos
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst mem-format-argument))
  (definstruction stos
                  (convert-register ?src)
                  ?dst))
(defmethod MAIN::*stos
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst MULTIFIELD))
  (*stos ?src
       (mem-format-arg (expand$ ?dst))))
(defmethod MAIN::*stos
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   $?dst)
  (*stos ?src
       ?dst))
; -------------------
; stis
; -------------------
(defmethod MAIN::*stis
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst mem-format-argument))
  (definstruction stis
                  (convert-register ?src)
                  ?dst))
(defmethod MAIN::*stis
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   (?dst MULTIFIELD))
  (*stis ?src
       (mem-format-arg (expand$ ?dst))))
(defmethod MAIN::*stis
  ((?src register
         SYMBOL
         (is-valid-register ?current-argument))
   $?dst)
  (*stis ?src
       ?dst))
