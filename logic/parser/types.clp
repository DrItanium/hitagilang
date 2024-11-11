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
(include logic/common/types.clp)
; reconstruction is hard
(defclass MAIN::atom
  (is-a has-parent)
  (slot kind
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot value
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler to-string primary))
(defmessage-handler MAIN::atom to-string primary
                    ()
                    (send ?self:value
                          to-string))
(defclass MAIN::parser-variable
  (is-a has-parent)
  (slot kind
        (type SYMBOL)
        (storage shared)
        (visibility public)
        (access read-only)
        (default UNDEFINED))
  (slot value
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass MAIN::parser-single-field-variable
  (is-a parser-variable)
  (slot kind
        (source composite)
        (default single-field-variable)))

(defclass MAIN::parser-multifield-variable
  (is-a parser-variable)
  (slot kind
        (source composite)
        (default multifield-variable)))

(defclass MAIN::parser-global-variable
  (is-a parser-variable)
  (slot kind
        (source composite)
        (default global-variable)))

(defclass MAIN::parser-multifield-global-variable
  (is-a parser-variable)
  (slot kind
        (source composite)
        (default multifield-global-variable)))


(defclass MAIN::list
  (is-a has-parent
        has-contents))

(defclass MAIN::file-container
  (is-a list)
  (slot file-name
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE)))

(defclass MAIN::parser
  (is-a USER)
  (slot top-element
        (type INSTANCE)
        (storage local)
        (visibility public))
  (slot current-element
        (type INSTANCE)
        (storage local)
        (visibility public))
  (slot path
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot id 
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default-dynamic (gensym*)))
  (slot valid
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public))
  (slot state
        (type SYMBOL)
        (allowed-symbols UNPROCESSED
                         PARSED
                         PARSING
                         INVALID)
        (storage local)
        (visibility public))
  (multislot current-token
             (storage local)
             (visibility public))
  (message-handler init after)
  (message-handler to-string primary))

(defmessage-handler MAIN::parser init after
                    ()
                    (bind ?self:valid
                          (open ?self:path
                                ?self:id
                                "r"))
                    (bind ?self:state 
                          (if ?self:valid then PARSING else INVALID))
                    (bind ?self:top-element
                          (make-instance of file-container
                                         (parent FALSE)
                                         (file-name ?self:path)))
                    (bind ?self:current-element
                          ?self:top-element))



(deftemplate MAIN::parser-open-request
             (slot path
                   (type LEXEME)
                   (default ?NONE)))

(deffacts MAIN::parser-focus-files
          (annotation (kind focus-on-stage)
                      (target parse-files)
                      (reversible FALSE)
                      (args parser:generate-files
                            parser:process-file
                            parser:sanity-check
                            parser:hoisting
                            parser:resolve-parent-child-relationships
                            parser:identify-structures
                            )))

(deffacts MAIN::atom-to-variable-conversions
          (annotation (kind atom-to-variable-conversion)
                      (reversible FALSE)
                      (target SF_VARIABLE)
                      (args parser-single-field-variable))
          (annotation (kind atom-to-variable-conversion)
                      (reversible FALSE)
                      (target MF_VARIABLE)
                      (args parser-multifield-variable))
          (annotation (kind atom-to-variable-conversion)
                      (reversible FALSE)
                      (target GBL_VARIABLE)
                      (args parser-global-variable))
          (annotation (kind atom-to-variable-conversion)
                      (reversible FALSE)
                      (target MF_GBL_VARIABLE)
                      (args parser-multifield-global-variable))
          (annotation (kind types-to-raise-out-of-atoms)
                      (reversible FALSE)
                      (target atoms-to-raise)
                      (args SYMBOL
                            STRING
                            INSTANCE_NAME
                            INTEGER
                            FLOAT))
          )
