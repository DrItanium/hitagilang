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
(include asmgen.clp)
(defmethod MAIN::*b
  ((?targ SYMBOL))
  (definstruction b
                  ?targ))
(defmethod MAIN::*call
  ((?targ SYMBOL))
  (definstruction call
                  ?targ))
(defmethod MAIN::*ret () (definstruction ret))
(defmethod MAIN::*bal
  ((?targ SYMBOL))
  (definstruction bal
                  ?targ))
(defmethod MAIN::*bno
  ((?targ SYMBOL))
  (definstruction bno
                  ?targ))
(defmethod MAIN::*bg
  ((?targ SYMBOL))
  (definstruction bg
                  ?targ))
(defmethod MAIN::*be
  ((?targ SYMBOL))
  (definstruction be
                  ?targ))
(defmethod MAIN::*bge
  ((?targ SYMBOL))
  (definstruction bge
                  ?targ))
(defmethod MAIN::*bl
  ((?targ SYMBOL))
  (definstruction bl
                  ?targ))
(defmethod MAIN::*bne
  ((?targ SYMBOL))
  (definstruction bne
                  ?targ))
(defmethod MAIN::*ble
  ((?targ SYMBOL))
  (definstruction ble
                  ?targ))
(defmethod MAIN::*bo
  ((?targ SYMBOL))
  (definstruction bo
                  ?targ))

(defmethod MAIN::*faultno () (definstruction faultno))
(defmethod MAIN::*faultg () (definstruction faultg))
(defmethod MAIN::*faulte () (definstruction faulte))
(defmethod MAIN::*faultge () (definstruction faultge))
(defmethod MAIN::*faultl () (definstruction faultl))
(defmethod MAIN::*faultne () (definstruction faultne))
(defmethod MAIN::*faultle () (definstruction faultle))
(defmethod MAIN::*faulto () (definstruction faulto))
