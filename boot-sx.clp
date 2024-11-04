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
(include utils.clp)
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
(deffunction MAIN::print-char
             (?in)
             (*st ?in
                  displacement: 0xFE000008))
(deffunction MAIN::emit-char
             (?value)
             (mkblock (*ldconst ?value 
                                r3)
                      (print-char r3)))
(deffunction MAIN::emit-newline () (emit-char '\n'))
(deffunction MAIN::emit-tab () (emit-char '\t'))
(deffunction MAIN::emit-space () (emit-char 0x20))
(deffunction MAIN::emit-colon () (emit-char ':'))
(deffunction MAIN::pnum 
             (?in)
             (mkblock (*mov ?in g0)
                      (*bal print_number_hex)))
(deffunction MAIN::print-comparison
             (?prefix ?a ?b)
             (mkblock (print-text ?prefix)
                      (pnum ?a)
                      (emit-space)
                      (pnum ?b)
                      (emit-newline)))
(deffunction MAIN::print-single-register
             (?prefix ?value)
             (mkblock (print-text ?prefix)
                      (pnum ?value)
                      (emit-newline)))
(deffunction MAIN::slice-digit
             (?shift)
             (mkblock (*shro ?shift g0 g1)
                      (*and 15 g1 g1) ; @todo implement 0xf for literals
                      (*ldob dest: g2
                             displacement: ascii_hex_table
                             index: g1
                             scale: 1)
                      (print-char g2)))
(deffunction MAIN::reserved-table-entry
             ()
             (.word 0))
(deffunction MAIN::fault-table-entry
             (?name)
             (def-table-entry (sym-cat _user_ ?name _core)))
(deffunction MAIN::def-fault-dispatcher
             (?name)
             (defroutine:window (sym-cat _user_ ?name _core)
                                (*lda dest: g0
                                      displacement: -48
                                      abase: fp)
                                (*callx (sym-cat _user_ ?name))
                                (*flushreg)))


(deffunction MAIN::make-ibr
             ()
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
                             -1)))
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
                      (make-ibr)
                      ; processor starts execution at this spot upon power-up after self-test
                      (defsite start_ip
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
                      (.align 4)
                      (defsite reinitialize_iac
                               (.word 0x93000000 ; reinitialize iac message
                                      system_address_table 
                                      _prcb_ram ; use newly copied PRCB
                                      start_again_ip)) ; start here
                      ; the process will begin execution here after being reinitialized
                      ; We will now setup the stacks and continue
                      (defsite start_again_ip
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
                               ; this is the point where your main code is called
                               ; If any IO needs to be set up, you should do it here before your
                               ; call to main. No opens have been done for STDIN, STDOUT, or STDERR
                               (*callx _init_fp)
                               (*callx setupInterruptHandler)
                               (clear-callx _main)
                               (deflabel exec_fallthrough)
                               (*b exec_fallthrough))
                      (defroutine:window _init_fp
                                         (*cvtir 0 [fp0])
                                         (*movre [fp0] [fp1])
                                         (*movre [fp1] [fp2])
                                         (*movre [fp2] [fp3]))
                      (defroutine:window setupInterruptHandler
                                         (*ldconst 0xff000004 
                                                   g5)
                                         (*ldconst 0xFCFDFEFF
                                                   g6)
                                         (*synmov g5 
                                                  g6))
                      (defroutine:leaf move_data
                                       (*ldconst 256 g12)
                                       (*ldconst 0 g13)
                                       (defsite move_data_loop
                                                (*ldq dest: g4
                                                      abase: g1
                                                      index: g3
                                                      scale: 1)
                                                (*stq g4
                                                      abase: g2
                                                      index: g3
                                                      scale: 1)
                                                (*ldq dest: g8
                                                      abase: g2
                                                      index: g3
                                                      scale: 1)
                                                (*cmpoqbne g4 g8 problem_checksum_failure)
                                                (*addi g3 16 g3) ; increment index
                                                (*modi g12 g3 g13) ; check and see if it is a multiple of 256
                                                (*cmpobne 0 g13 move_data_no_print)
                                                (emit-char '.'))
                                       (defsite move_data_no_print
                                                (*cmpibg g0 g3 move_data_loop) ; loop until done
                                                (emit-newline)))
                      (defsite problem_checksum_failure
                               (*movq g0 r12)
                               (print-text msg_checksum_failures)
                               (print-single-register msg_g0 r12)
                               (print-single-register msg_g1 r13)
                               (print-single-register msg_g2 r14)
                               (print-single-register msg_g3 r15)
                               (emit-newline)
                               (print-comparison msg_g4_g8 g4 g8)
                               (print-comparison msg_g5_g9 g5 g9)
                               (print-comparison msg_g6_g10 g6 g10)
                               (print-comparison msg_g7_g11 g7 g11)
                               (*b exec_fallthrough))
                      (defroutine:leaf print_number_hex
                                       (slice-digit 28)
                                       (slice-digit 24)
                                       (slice-digit 20)
                                       (slice-digit 16)
                                       (slice-digit 12)
                                       (slice-digit 8)
                                       (slice-digit 4)
                                       (slice-digit 0))
                      (defroutine:leaf boot_print
                                       (*cmpobe 0 g1 boot_print_done)
                                       (*addi g0 1 g0) ; increment the counter
                                       (print-char g1)
                                       (*b boot_print)
                                       (deflabel boot_print_done))
                      (defroutine:window boot_print2
                                         (*ldob dest: g1
                                                abase: g0) ; load the current byte to potentially print out
                                         (*cmpobe 0 g1 boot_print_done2)
                                         (*addi g0 1 g0) ; increment the counter
                                         (print-char g1)
                                         (*b boot_print2)
                                         (deflabel boot_print_done2))
                      (deflabeled-string-newline msg_checksum_failures
                                                 "Copy Verification Failed")
                      (deflabeled-string-newline msg_transfer_complete
                                                 "Done Copying Data/BSS to SRAM from IO Memory")
                      (deflabeled-string-newline msg_boot_checksum_passed
                                                 "i960 Boot Checksum Passed!")
                      (deflabeled-string msg_g4_g8 
                                         "g4&g8: ")
                      (deflabeled-string msg_g5_g9 
                                         "g5&g9: ")
                      (deflabeled-string msg_g6_g10 
                                         "g6&g10: ")
                      (deflabeled-string msg_g7_g11 
                                         "g7&g11: ")
                      (deflabeled-string msg_g0
                                         "g0: ")
                      (deflabeled-string msg_g1
                                         "g1: ")
                      (deflabeled-string msg_g2
                                         "g2: ")
                      (deflabeled-string msg_g3
                                         "g3: ")
                      (deflabeled-string ascii_hex_table
                                         "0123456789ABCDEF")
                      ; setup the bss section, so do giant block of writes
                      ; the routine below fixes up the stack for a false interrupt return
                      ; We have reserved area on the stack before the call to this routine.
                      ; We need to build a phony interrupt record here to force the processor
                      ; to pick up on the return.
                      ;
                      ; Also, we will take advantage of the fact that the processor will
                      ; restore the PC and AC to its registers
                      (.align 6)
                      (defroutine:window fix_stack
                                         (*flushreg)
                                         (*or pfp 7 pfp) ; put interrupt return code into pfp
                                         (*ldconst 0x1f0002 
                                                   g0)
                                         (*st g0 
                                              displacement: -16
                                              abase: fp) ; store contrived pc
                                         (*ldconst 0x3b001000
                                                   g0) ; setup arithmetic controls
                                         (*st g0 
                                              displacement: -12
                                              abase: fp)) ; store contrived AC
                      (.align 6)
                      (defsite system_address_table
                               (null-segment) ; 0
                               (null-segment) ; 1
                               (null-segment) ; 2
                               (null-segment) ; 3
                               (null-segment) ; 4
                               (null-segment) ; 5
                               (null-segment) ; 6
                               (declare-segment 0 0 sys_proc_table 0x304000fb) ; 7
                               (small-segment-table system_address_table) ; 8
                               (declare-segment 0 0 sys_proc_table 0x304000fb) ; 9
                               (declare-segment 0 0 fault_proc_table 0x304000fb) ; 10
                               )
                      ; initial prcb
                      ; this is our startup prcb. After initialization
                      ; this will be copied to RAM
                      (.align 6)
                      (defsite prcb_ptr
                               (.word 0x0  ; 0 - reserved
                                      0xc  ; 4 - process state = executing (no virtual address translation)
                                      0x0  ; 8 - reserved
                                      0x0  ; 12 - current process
                                      0x0  ; 16 - dispatch port
                                      intr_table ; 20 - interrupt table physical address
                                      _intr_stack ; 24 - interrupt stack pointer
                                      0x0)  ; 28 -reserved
                               (segment-selector 7) ; 32 - pointer to offset zero (region 3 segment selector)
                               (segment-selector 9) ; 36 - system procedure table pointer
                               (.word fault_table ; 40 - fault table
                                      0x0)        ; 44 - reserved
                               (.space 12)        ; 48 - reserved
                               (.word 0)          ; 60 - reserved
                               (.space 8)         ; 64 - idle time
                               (.word 0)          ; 72 - system error fault
                               (.word 0)          ; 76 - reserved
                               (.space 48)        ; 80 - resumption record
                               (.space 44))        ; 128 - system error fault record
                      (.align 6)
                      (.global sys_proc_table)
                      (defsite sys_proc_table
                               (.word 0 0 0) ; Reserved
                               (.word "(_sup_stack + 0x1)") ; Supervisor stack pointer
                               (.word 0 0 0 0
                                      0 0 0 0) ; Preserved
                               ; up to 260 entries!
                               (.word 0 0 0 0) ; 0 - 3
                               (.word 0 0 0  ) ; 4-6
                               (def-table-entry _hitagi_unlink)
                               (reserved-table-entry) ; _hitagi_getpid
                               (reserved-table-entry) ; _hitagi_kill
                               (reserved-table-entry) ; _hitagi_fstat
                               (reserved-table-entry) ; sbrk
                               (reserved-table-entry) ; _hitagi_argvlen
                               (reserved-table-entry) ; _hitagi_argv
                               (reserved-table-entry) ; _hitagi_chdir
                               (reserved-table-entry) ; _hitagi_stat
                               (reserved-table-entry) ; _hitagi_chmod
                               (reserved-table-entry) ; _hitagi_utime
                               (reserved-table-entry) ; _hitagi_time
                               (def-table-entry _hitagi_gettimeofday)

                               (def-table-entry _hitagi_setitimer)
                               (def-table-entry _hitagi_getrusage)
                               (.word 0 0) ; 22, 23
                               (.word 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                               (.word 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                               (.word 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                               (.word 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                               (.word 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                               (.word 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                               (.word 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                               (.word 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                               (.word 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                               (.word 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                               (.word 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                               (.word 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                               (.word 0 0 0 0 0 0 0 0 0 0 0 0)
                               ; mon960 registrations
                               (.word 0 0)
                               (def-table-entry _hitagi_open)
                               (def-table-entry _hitagi_read)

                               (def-table-entry _hitagi_write)
                               (def-table-entry _hitagi_lseek)
                               (def-table-entry _hitagi_close)
                               (.word 0)

                               (.word 0 0 0 0 0 0 0 0)
                               (.word 0 0 0 0 0 0 0 0)
                               (.word 0 0 0 0)
                               (.word 0)
                               (def-table-entry _hitagi_exit)
                               (.word 0 0))
                      (.align 6)
                      (defsite fault_proc_table
                               (.word 0 ; Reserved
                                      0 ; Reserved
                                      0 ; Reserved
                                      _sup_stack) ; Supervisor stack pointer
                               (.word 0 0 0 0
                                      0 0 0 0) ; preserved
                               (apply-to-each$ fault-table-entry 
                                               override
                                               trace
                                               operation 
                                               arithmetic
                                               floating_point
                                               constraint
                                               virtual_memory
                                               protection
                                               machine
                                               structural
                                               type
                                               process
                                               descriptor
                                               event
                                               reserved))
                      (apply-to-each$ def-fault-dispatcher
                                      override
                                      trace
                                      operation
                                      arithmetic
                                      floating_point
                                      constraint
                                      virtual_memory
                                      protection
                                      machine
                                      structural
                                      type
                                      process
                                      descriptor
                                      event
                                      reserved)
                      (def-system-call 7 _sys_unlink)
                      ;(def-system-call 8 _sys_getpid)
                      ;(def-system-call 9 _sys_kill)
                      ;(def-system-call 10 _sys_fstat)
                      ;(def-system-call 11 _sys_sbrk)
                      ;(def-system-call 12 _sys_argvlen)
                      ;(def-system-call 13 _sys_argv)
                      ;(def-system-call 14 _sys_chdir)
                      ;(def-system-call 15 _sys_stat)
                      ;(def-system-call 16 _sys_chmod)
                      ;(def-system-call 17 _sys_utime)
                      ;(def-system-call 18 _sys_time)
                      (def-system-call 19 _sys_gettimeofday)
                      (def-system-call 20 _sys_setitimer)
                      (def-system-call 21 _sys_getrusage)

                      ; define RAM area to copy the PRCB and interrupt table to after initial bootup from EPROM/FLASH
                      (.bss _user_stack 0x8000 6)
                      (.bss _intr_stack 0x8000 6)
                      (.bss _sup_stack 0x8000 6)
                      (.bss intr_ram 1028 6)
                      (.bss _prcb_ram 176 6)
                      )
             )

