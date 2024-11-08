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
; code to define all of the different aspects of parsing source files
(defrule parser:generate-files::generate-file-container
         ?f <- (parse file ?path)
         =>
         (retract ?f)
         (make-instance of parser
                        (path ?path)))

(defrule parser:process-file::read-token
         ?f <- (object (is-a parser)
                       (current-token)
                       (state PARSING)
                       (valid TRUE)
                       (id ?id))
         =>
         (modify-instance ?f
                          (current-token (next-token ?id))))

(defrule parser:process-file::stop-parsing-file
         "If we see a stop token then just terminate immediately!"
         ?f <- (object (is-a parser)
                       (current-token STOP ?)
                       (state PARSING)
                       (valid TRUE)
                       (id ?id))
         =>
         (close ?id)
         (modify-instance ?f
                          (current-token)
                          (state PARSED)))
; @todo continue here
;(defrule parser:generate-files::display-information
;         =>
;         (printout stdout 
;                   (get-current-module) 
;                   crlf))
;
;(defrule parser:process-file::display-information
;         =>
;         (printout stdout 
;                   (get-current-module) 
;                   crlf))
;
;(defrule parser:sanity-check::display-information
;         =>
;         (printout stdout 
;                   (get-current-module) 
;                   crlf))
;(defrule parser:hoisting::display-information
;         =>
;         (printout stdout 
;                   (get-current-module) 
;                   crlf))
;
;(defrule parser:identify-structures::display-information
;         =>
;         (printout stdout 
;                   (get-current-module) 
;                   crlf))
