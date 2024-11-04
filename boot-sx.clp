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


(deffunction MAIN::code-body
             ()
             (mkblock (.global system_address_table)
                      (.global prcb_ptr)
                      (.global _prcb_ram)
                      (.global start_ip)
                      (.global cs1)
                      (.global STACK_SIZE)
                      (.global _user_stack)
                      (.global _sup_stack) ; supervisor stack
                      (.global _intr_stack)

                      ; core initialization block (located at address 0)
                      ; 8 words
                      (mkblock (.text)
                               (.word system_address_table ; SAT pointer
                                      prcb_ptr 
                                      0
                                      start_ip ; pointer to first ip
                                      cs1 ; calculated at link time (bind ?cs (- (+ ?SAT ?PRCB ?startIP)))
                                      0
                                      0
                                      -1))
                      ; processor starts execution at this spot upon power-up after self-test
                      (mkblock (deflabel start_ip)
                               (clear-g14)
                               (print-text msg_boot_checksum_passed)
                               (transfer-data 1028
                                              intr_table
                                              intr_ram
                                              0)
                               (print-text msg_transfer_complete)
                               (*ldconst intr_ram 
                                         g0)
                               (*ldconst _prcb_ram
                                         g1)
                               (*st g0 
                                    abase: g1
                                    offset: 20)
                               (*ldconst 0xff000010
                                         g5)
                               (*ldconst reinitialize_iac 
                                         g6)
                               (*synmovq g5
                                         g6))
                      (mkblock (.align 4)
                               (deflabel reinitialize_iac)
                               (.word 0x93000000 ; reinitialize iac message
                                      system_address_table 
                                      _prcb_ram ; use newly copied PRCB
                                      start_again_ip ; start here
                                      ))
                      ; the process will begin execution here after being reinitialized
                      ; We will now setup the stacks and continue
                      (mkblock (deflabel start_again_ip)
                               ; this would be a good place to disable board level interrupts if an interrupt controller is being used
                               ;
                               ; before calling main, we need to take the processor out of the "interrupted" state.
                               ; In order to do this, we will execute a call statement, then "fix up" the stack frame
                               ; to cause an interrupt return to be executed.
                               (*ldconst 64 g0) ; bump up stack to make
                               (*addo sp g0 sp) ; room for simulated interrupt frame
                               (*call fix_stack) ; routine to turn off interrupted state
                               (*lda dest: fp 
                                     displacement: _user_stack) ; setup user stack space
                               (*lda dest: pfp
                                     displacement: -0x40
                                     abase: fp) ; load pfp (just in case)
                               (*lda dest: sp
                                     displacement: 0x40
                                     abase: fp) ; set up current stack pointer
                               (*ldconst msg_stack_fixed g0)
                               (*callx displacement: boot_print2)
                               ; this is the point where your main code is called
                               ; If any IO needs to be set up, you should do it here before your
                               ; call to main. No opens have been done for STDIN, STDOUT, or STDERR
                               ;(*callx displacement: _init_fp)
                               (*callx displacement: setupInterruptHandler)
                               (*ldconst msg_invoking_main g0)
                               (*callx displacement: boot_print2)
                               (clear-callx _main)
                               (deflabel exec_fallthrough)
                               (*b exec-fallthrough)
                               )
                      )
             )
