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
        (type INSTANCE
              SYMBOL)
        (allowed-symbols FALSE)
        (storage local)
        (visibility public)
        (default-dynamic FALSE)))

(defclass MAIN::has-title
  (is-a USER)
  (slot title
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE)))

(defclass MAIN::has-description
  (is-a USER)
  (slot description
        (type LEXEME)
        (storage local)
        (visibility public)))

(defclass MAIN::has-contents
  (is-a USER)
  (multislot contents
             (storage local)
             (visibility public)))

(deftemplate MAIN::stage
             (slot current
                   (type SYMBOL)
                   (default ?NONE))
             (multislot rest
                        (type SYMBOL)))

(deffunction MAIN::apply-to-each$ 
             (?function $?contents)
             (bind ?results
                   (create$))
             (progn$ (?item ?contents)
                     (bind ?results
                           ?results
                           (funcall ?function
                                    ?item)))
             ?results)

(deffunction MAIN::apply-to-each-with-index$ 
             (?function $?contents)
             (bind ?results
                   (create$))
             (progn$ (?item ?contents)
                     (bind ?results
                           ?results
                           (funcall ?function
                                    ?item-index
                                    ?item)))
             ?results)

(defmessage-handler STRING to-string primary
                    ()
                    ?self)
(defmessage-handler NUMBER to-string primary
                    ()
                    (str-cat ?self))
(defmessage-handler SYMBOL to-string primary
                    ()
                    (str-cat ?self))
(defmessage-handler MULTIFIELD to-string primary
                    ()
                    (bind ?contents 
                          (create$))
                    (progn$ (?a ?self)
                            (bind ?contents
                                  ?contents
                                  (send ?a to-string)))
                    ?contents)

(deftemplate MAIN::annotation
             "A template fact one attaches to instances indirectly to describe more information about them"
             (slot target
                   (default ?NONE))
             (slot kind
                   (type SYMBOL)
                   (default ?NONE))
             (slot reversible
                   (type SYMBOL)
                   (allowed-symbols TRUE
                                    FALSE))
             (slot treat-as-set
                   (type SYMBOL)
                   (allowed-symbols TRUE
                                    FALSE))
             (multislot args
                        (default ?NONE)))

(deftemplate MAIN::annotation-close-request
             "When generating reverse kind annotations, we can hook into doing name replacements"
             (slot target-kind
                   (type SYMBOL)
                   (default ?NONE))
             (slot new-name
                   (type SYMBOL)
                   (default ?NONE)))
