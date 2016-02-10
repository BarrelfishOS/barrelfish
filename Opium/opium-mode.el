;/*
; * BEGIN LICENSE BLOCK
; * Version: CMPL 1.1
; *
; * The contents of this file are subject to the Cisco-style Mozilla Public
; * License Version 1.1 (the "License"); you may not use this file except
; * in compliance with the License.  You may obtain a copy of the License
; * at www.eclipse-clp.org/license.
; * 
; * Software distributed under the License is distributed on an "AS IS"
; * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
; * the License for the specific language governing rights and limitations
; * under the License. 
; * 
; * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
; * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
; * Portions created by the Initial Developer are
; * Copyright (C) 1990,2006 Cisco Systems, Inc.  All Rights Reserved.
; * 
; * Contributor(s): Mireille Ducasse, ECRC.
; * 
; * END LICENSE BLOCK
; *
; *	$Id: opium-mode.el,v 1.1 2006/09/23 01:54:34 snovello Exp $
; */

; 
; opium mode to help to declare opium's objects
; 

(defun opium-mode ()
  (interactive)
      (setq mode-name "opium-mode")
)


(defun scenario ()
  (interactive)
           (end-of-line)
           (insert-string "\nopium_scenario(\n")
           (insert-string 
               (concat "\tname\t\t: "
                       (read-string "Name ? ")
                       ",\n"))
           (insert-string 
               (concat "\tfiles\t\t: ["
                       (read-string "Source Files, included the current file ? (separate with ',') ")
                       "],\n"))
           (insert-string 
               (concat "\tscenarios\t: ["
                       (read-string "Further scenarios needed to run the actual one ? (separate with ',') ")
                       "],\n"))
           (insert-string 
               (concat "\tmessage\t\t:\n\""
                       (read-string " Help Message ? ")
                       "\"\n"))
           (insert-string "\t).\n")
           (message "opium_scenario defined"))


(defun command ()
  (interactive)
               (insert-string "\nopium_command(\n")
               (insert-string 
                   (concat "\tname\t\t: "
                           (read-string "Name ? ")
                           ",\n"))
               (insert-string 
                   (concat "\targ_list\t: ["
                           (read-string "Arguments ? (separate with ',') ")
                           "],\n"))
               (insert-string 
                   (concat "\targ_type_list\t: ["
                           (read-string "Argument Types ? (separate with ',') ")
                           "],\n"))
               (insert-string 
                   (concat "\tabbrev\t\t: "
                           (read-string "Abbrev ? (no arguments, '_' if no abbrev) ")
                           ",\n"))
               (insert-string 
                   (concat "\tinterface\t: "
                           (read-string "Interface ? (button/menu/hidden) ")
                           ",\n"))
               (insert-string 
                   (concat "\tcommand_type\t: "
                           (read-string "Command type ? (trace/opium/tool) ")
                           ",\n"))
              (insert-string 
                   (concat "\timplementation\t: "
                           (read-string "Name of the implementation ? ")
                           ",\n"))
               (insert-string 
                   (concat "\tparameters\t: ["
                           (read-string "Related Parameters ? (separate with ',') ")
                           "],\n"))
               (insert-string 
                   (concat "\tmessage\t\t:\n\""
                           (read-string "Help Message ? ")
                           "\"\n"))
               (insert-string "\t).\n")
               (message "opium_command defined"))


(defun primitive ()
  (interactive)
               (insert-string "\nopium_primitive(\n")
               (insert-string 
                   (concat "\tname\t\t: "
                           (read-string "Name ? ")
                           ",\n"))
               (insert-string 
                   (concat "\targ_list\t: ["
                           (read-string "Arguments ? (separate with ',') ")
                           "],\n"))
               (insert-string 
                   (concat "\targ_type_list\t: ["
                           (read-string "Argument Types ? (separate with ',') ")
                           "],\n"))
               (insert-string 
                   (concat "\tabbrev\t\t: "
                           (read-string "Abbrev ? (no arguments) ")
                           ",\n"))
              (insert-string 
                   (concat "\timplementation\t: "
                           (read-string "Name of the implementation ? ")
                           ",\n"))
               (insert-string 
                   (concat "\tmessage\t\t:\n\""
                           (read-string "Help Message ? ")
                           "\"\n"))
               (insert-string "\t).\n")
               (message "opium_primitive defined"))


(defun procedure ()
  (interactive)
           (insert-string "\nopium_procedure(\n")
               (insert-string 
                   (concat "\tname\t\t: "
                           (read-string "Name ? ")
                           ",\n"))
               (insert-string 
                   (concat "\targ_list\t: ["
                           (read-string "Arguments ? (separate with ',') ")
                           "],\n"))
               (insert-string 
                   (concat "\timplementation\t: "
                           (read-string " Name of the implementation ? ")
                           ",\n"))
               (insert-string 
                   (concat "\tparameters\t: ["
                           (read-string "Related Parameters ? (separate with ',') ")
                           "],\n"))
               (insert-string 
                   (concat "\tmessage\t\t:\n\""
                           (read-string "Help Message ? ")
                           "\"\n"))
               (insert-string "\t).\n")
               (message "opium_procedure defined"))


(defun parameter ()
  (interactive)
               (insert-string "\nopium_parameter(\n")
               (insert-string 
                   (concat "\tname\t\t: "
                           (read-string "Name ? ")
                           ",\n"))
               (insert-string 
                   (concat "\targ_list\t: ["
                           (read-string "Arguments ? (separate with ',') ")
                           "],\n"))
               (insert-string 
                   (concat "\targ_type_list\t: ["
                           (read-string "Argument Types ? (separate with ',') ")
                           "],\n"))
               (insert-string 
                   (concat "\tparameter_type\t: "
                           (read-string "Parameter type ? (single/multiple) ")
                           ",\n"))
               (insert-string 
                   (concat "\tdefault\t\t: ["
                           (read-string "Default Value of the Arguments ? (separate with ',') ")
                           "],\n"))
               (insert-string 
                   (concat "\tcommands\t: ["
                           (read-string "Related Commands ? (separate with ',') ")
                           "],\n"))
               (insert-string 
                   (concat "\tmessage\t\t: \n\""
                           (read-string "Help Message ? ")
                           "\"\n"))
               (insert-string "\t).\n")
               (message "opium_parameter defined"))


(defun type ()
  (interactive)
               (insert-string "\nopium_type(\n")
               (insert-string 
                   (concat "\tname\t\t: "
                           (read-string "Name ? ")
                           ",\n"))
               (insert-string 
                   (concat "\timplementation\t: "
                           (read-string " Name of the implementation ? ")
                           ",\n"))
               (insert-string 
                   (concat "\tmessage\t\t: \n\""
                           (read-string "Help Message ? ")
                           "\"\n"))
               (insert-string "\t).\n")
               (message "opium_type defined"))


(defun demo ()
  (interactive)
               (insert-string "\nopium_demo(\n")
               (insert-string 
                   (concat "\tname\t\t: "
                           (read-string "Name ? ")
                           ",\n"))
               (insert-string 
                   (concat "\tdemo_goal\t: ("
                           (read-string " Goals to be used for the demo? (separate with ',') ")
                           "),\n"))
	       (insert-string 
                   (concat "\tcondition\t: ("
                           (read-string " Conditions/initialisation for the demo goal? (separate with ',') ")
                           "),\n"))
               (insert-string 
                   (concat "\tmessage\t\t: \n\""
                           (read-string "Help Message ? ")
                           "\"\n"))
               (insert-string "\t).\n")
               (message "opium_demo defined"))
