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

(defleaf foo
         (args (?a ORDINAL)
               (?b ORDINAL))
         (returns ORDINAL)
         (body (+ ?a ?b)))
(defleaf foo
         (args (?a INTEGER)
               (?b INTEGER))
         (returns INTEGER)
         (body (+ ?a ?b)))

(defleaf foo
         (args (?a INTEGER)
               (?b ORDINAL))
         (returns ORDINAL)
         (body (if (condition (< ?a 0))
                 (body (- ?b 
                          ?a))
                 (body (+ (cast ?a 
                                ORDINAL)
                          ?b)))))

(defleaf foo
         (args (?a LONG-ORDINAL)
               (?b LONG-ORDINAL))
         (returns LONG-ORDINAL)
         (body (+ ?a 
                  ?b)))

(defleaf address-plus-offset
         (args (?address ADDRESS)
               (?b ORDINAL))
         (returns ORDINAL)
         (body (bind ?base-address 
                     ?a)
               (+ ?base-address
                  ?b)))

(defleaf goo
         (args (?a BOOLEAN))
         (returns BOOLEAN)
         (body (not ?a)))


(defwindow address-plus-offset
           (args (?address ADDRESS)
                 (?b ORDINAL))
           (returns ADDRESS)
           (body (bind ?base-address 
                       (cast ?a ORDINAL))
                 (cast (call (lambda (args (?first ORDINAL)
                                           (?second ORDINAL))
                               (returns ORDINAL)
                               (body (+ ?first
                                        ?second)))
                             ?base-address
                             ?b) 
                       ADDRESS)))

(defleaf foo
         (args (?a ORDINAL)
               (?b ORDINAL)
               (?c ORDINAL)) 
         (returns ORDINAL)
         (body (+ ?a
                  ?b
                  ?c)))

(defleaf foo
         (args (?a ORDINAL)
               (?b ORDINAL)
               (?c ORDINAL)
               (?d ORDINAL))
         (returns ORDINAL)
         (+ ?a
            ?b
            ?c
            ?d))

(defleaf goo
         (args (?a ORDINAL)
               (?b ORDINAL)
               (?c ORDINAL)
               (?d ORDINAL))
         (returns ORDINAL)
         (and ?a
              ?b
              ?c
              ?d))
(defleaf zoo
         (args (?a ORDINAL))
         (while (condition (> ?a 1000))
                (body (bind ?a 
                            (+ ?a 1)))))
