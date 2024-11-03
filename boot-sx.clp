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
(include boot-macros.clp)
(deffunction MAIN::def-table-entry
             (?name)
             (.word (format nil 
                            "(%s + 0x2)"
                            (str-cat ?name))))
(deffunction MAIN::reserved-table-entry
             ()
             (.word 0))
(deffunction MAIN::print-text
             (?name)
             (mkblock (*ldconst ?name
                              g0)
                    (*bal boot_print)))
(deffunction MAIN::transfer-data
             (?size ?src ?dest ?offset)
             (mkblock (*ldconst ?size 
                              g0)
                    (*ldconst ?src 
                              g1)
                    (*ldconst ?dest
                              g2)
                    (*ldconst ?offset
                              g3)
                    (*bal move_data)))


;(deffunction MAIN::code-body
;             ()
;             (block (.global system_address_table)
;                    (.global prcb_ptr)
;                    (.global _prcb_ram)
;                    (.global start_ip)
;                    (.global cs1)
;                    (.global STACK_SIZE)
;                    (.global _user_stack)
;                    (.global _sup_stack) ; supervisor stack
;                    (.global _intr_stack)
;
;                    ; core initialization block (located at address 0)
;                    ; 8 words
;                    (block (.text)
;                           (.word system_address_table ; SAT pointer
;                                  prcb_ptr 
;                                  0
;                                  start_ip ; pointer to first ip
;                                  cs1 ; calculated at link time (bind ?cs (- (+ ?SAT ?PRCB ?startIP)))
;                                  0
;                                  0
;                                  -1))
;                    (block (deflabel start_ip)
;                           (clear-g14)
;                           (print-text msg_boot_checksum_passed)
;                           (transfer-data 1028
;                                          intr_table
;                                          intr_ram
;                                          0)
;                           (print-text msg_transfer_complete)
;                           (*ldconst intr_ram 
;                                     g0)
;                           ; processor starts execution at this spot upon power-up after self-test
;
