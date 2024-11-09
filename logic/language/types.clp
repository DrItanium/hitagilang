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
(include logic/parser/types.clp)

(defclass MAIN::individual-argument
  (is-a has-parent
        has-contents
        has-title))
(defclass MAIN::argument-block
  (is-a has-parent
        has-contents))
(defclass MAIN::execution-block-declaration
  (is-a has-parent
        has-contents
        has-title)
  ;(slot mangled-title
  ;      (type SYMBOL)
  ;      (storage local)
  ;      (visibility public)
  ;      (default-dynamic FALSE))
  (slot kind
        (type SYMBOL)
        (storage shared)
        (visibility public)
        (access read-only)
        (default FALSE)))
(defclass MAIN::function-declaration
  (is-a execution-block-declaration)
  (slot arguments
        (type INSTANCE)
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass MAIN::leaf-function-declaration
  (is-a function-declaration)
  (slot kind
        (source composite)
        (default leaf)))

(defclass MAIN::window-function-declaration
  (is-a function-declaration)
  (slot kind
        (source composite)
        (default window)))

(defclass MAIN::site-declaration
  (is-a function-declaration)
  (slot kind
        (source composite)
        (default site)))
(deftemplate MAIN::execution-block-translation
             (slot keyword
                   (type SYMBOL)
                   (default ?NONE))
             (slot class-kind
                   (type SYMBOL)
                   (default ?NONE)))
(deffacts MAIN::important-declaration-setups
          (execution-block-translation (keyword defleaf)
                                       (class-kind leaf-function-declaration))
          (execution-block-translation (keyword defwindow)
                                       (class-kind windows-function-declaration))
          (execution-block-translation (keyword defsite)
                                       (class-kind site-declaration)))

(defclass MAIN::expression
  (is-a has-parent
        has-contents
        has-title))
