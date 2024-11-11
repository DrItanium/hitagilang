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
(defrule MAIN::convert-parser-fact-to-template
         (declare (salience 10000))
         ?f <- (parse file ?path)
         =>
         (retract ?f)
         (assert (parser-open-request (path ?path))))
(defrule parser:generate-files::generate-file-container
         ?f <- (parser-open-request (path ?path))
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
(defrule parser:process-file::make-new-list
         ?f <- (object (is-a parser)
                       (current-token LEFT_PARENTHESIS ?)
                       (state PARSING)
                       (valid TRUE)
                       (id ?id)
                       (current-element ?target))
         ?k <- (object (is-a list)
                       (name ?target)
                       (contents $?prior))
         =>
         (bind ?ncurr
               (make-instance of list
                              (parent ?target)))
         (modify-instance ?f
                          (current-token)
                          (current-element ?ncurr))
         (modify-instance ?k
                          (contents ?prior
                                    ?ncurr)))
(defrule parser:process-file::leave-current-element:valid
         ?f <- (object (is-a parser)
                       (current-token RIGHT_PARENTHESIS ?)
                       (state PARSING)
                       (valid TRUE)
                       (id ?id)
                       (current-element ?target))
         (object (is-a list)
                 (name ?target)
                 (parent ?parent&~FALSE))
         =>
         (modify-instance ?f
                          (current-token)
                          (current-element ?parent)))


(defrule parser:process-file::leave-current-element:invalid-noparent
         ?f <- (object (is-a parser)
                       (current-token RIGHT_PARENTHESIS ?)
                       (state PARSING)
                       (valid TRUE)
                       (id ?id)
                       (path ?path)
                       (current-element ?target)
                       (name ?name))
         (object (is-a list)
                 (name ?target)
                 (parent FALSE))
         =>
         (printout stderr
                   "ERROR: mismatched parens, found a right paren without a matching left paren" crlf
                   "Target file: " ?path crlf
                   "Target parser: " ?name crlf)
         (halt))

(defrule parser:process-file::make-atomic-value
         ?f <- (object (is-a parser)
                       (current-token ?kind&~LEFT_PARENTHESIS&~RIGHT_PARENTHESIS&~STOP ?value)
                       (state PARSING)
                       (valid TRUE)
                       (id ?id)
                       (current-element ?target))
         ?k <- (object (is-a list)
                       (name ?target)
                       (contents $?prior))
         =>
         (modify-instance ?f
                          (current-token))
         (modify-instance ?k
                          (contents ?prior
                                    (make-instance of atom
                                                   (parent ?target)
                                                   (kind ?kind)
                                                   (value ?value)))))
(defrule parser:sanity-check::found-lparen-mismatch-at-end
         ?f <- (object (is-a parser)
                       (state PARSED)
                       (valid TRUE)
                       (path ?path)
                       (top-element ?top)
                       (current-element ?v&~?top)
                       (name ?name))
         =>
         (printout stderr
                   "ERROR: mismatched parens, found a left paren without a matching right paren after finishing parsing" crlf
                   "Target file: " ?path crlf
                   "Target parser: " ?name crlf
                   "Mismatched container: " ?v crlf)
         (halt))
(defrule parser:hoisting::raise-elements-out-of-atoms
          (annotation (kind types-to-raise-out-of-atoms)
                      (target atoms-to-raise)
                      (args $? ?target $?))
         ?k <- (object (is-a atom)
                       (parent ?p)
                       (name ?sym-ref)
                       (kind ?target)
                       (value ?value))
         ?f <- (object (is-a list)
                       (name ?p)
                       (contents $?a ?sym-ref $?b))
         =>
         (unmake-instance ?k)
         (modify-instance ?f
                          (contents ?a 
                                    ?value
                                    ?b)))

(defrule parser:hoisting::do-atom-to-object-conversion
          (annotation (kind atom-to-variable-conversion)
                      (reversible FALSE)
                      (target ?target)
                      (args ?type))
         ?f <- (object (is-a atom)
                       (kind ?target)
                       (parent ?p)
                       (name ?name)
                       (value ?value))
         =>
         (unmake-instance ?f)
         (make-instance ?name of ?type
                        (parent ?p)
                        (value ?value)))
