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

(defgeneric MAIN::comment
            "Attach a i960 comment to a given expression")


(defmethod MAIN::comment
  ((?message LEXEME))
  (format nil
          "# %s"
          ?message))

(defmethod MAIN::comment
  ((?message MULTIFIELD))
  (comment (implode$ ?message)))
(defmethod MAIN::comment
  ($?message)
  (comment ?message))

(defmethod MAIN::label
  ((?title SYMBOL))
  (format nil
          "%s:"
          ?title))


(defmethod MAIN::op-0arg
  ((?keyword SYMBOL))
  ?keyword)

(defmethod MAIN::op-1arg
  ((?keyword SYMBOL)
   (?arg0 LEXEME
          NUMBER))
  (format nil
          "%s %s"
          ?keyword
          (str-cat ?arg0)))

(defmethod MAIN::op-2arg
  ((?keyword SYMBOL)
   (?arg0 LEXEME
          NUMBER)
   (?arg1 LEXEME
          NUMBER))
  (format nil
          "%s %s, %s"
          ?keyword
          (str-cat ?arg0)
          (str-cat ?arg1)))

(defmethod MAIN::op-3arg
  ((?keyword SYMBOL)
   (?arg0 LEXEME
          NUMBER)
   (?arg1 LEXEME
          NUMBER)
   (?arg2 LEXEME
          NUMBER))
  (format nil
          "%s %s, %s, %s"
          ?keyword
          (str-cat ?arg0)
          (str-cat ?arg1)
          (str-cat ?arg2)))

