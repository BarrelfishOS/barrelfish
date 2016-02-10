% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 1995 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, Stefano Novello, Vassilis Liatsos,
%                 Mark Wallace, Andrew Sadler, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: fd_global.ecl,v 1.6 2015/01/14 01:31:10 jschimpf Exp $
%
%
% IDENTIFICATION:	fd_global.ecl
%
% AUTHORS:		Joachim Schimpf, IC-Parc, Imperial College, London
%			Stefano Novello, IC-Parc, Imperial College, London
%			Vassilis Liatsos, IC-Parc, Imperial College, London
%			Mark Wallace, IC-Parc, Imperial College, London
%                       Andrew Sadler, IC-Parc, Imperial College, London
%
% Specialise the generic code of generic_global_constraints.ecl to
% create the FD global constraints library.
% ----------------------------------------------------------------------

:- module(fd_global).

:- comment(categories, ["Constraints"]).
:- comment(summary, "Various global constraints over lists of FD variables").
:- comment(author, "J.Schimpf, V.Liatsos, S.Novello, M.Wallace, A.Sadler, IC-Parc").
:- comment(copyright, "Cisco Systems, Inc.").
:- comment(date, "$Date: 2015/01/14 01:31:10 $").

:- lib(fd).
:- use_module(fd_generic_interface).

:- include(generic_global_constraints).

:- export alldifferent_matrix/1.

alldifferent_matrix(Matrix) :-
        alldifferent(Matrix,fd_global).

:-comment(alldifferent_matrix/1,[
      summary:"Constrain the rows and columns of Matrix to be different values",
      amode:alldifferent_matrix(+),
      args:["Matrix":"A two dimensional square matrix of Variables or integer"],
      see_also:[fd_global:alldifferent/1,_:alldifferent_matrix/1],
      desc:html("\
<P>
    This constraint is a matrix version of alldifferent. Matrix is a two
    dimensional square (NxN) matrix, and the constraint ensures that the 
    elements in each row and column of the matrix are different. The same
    value can occur in different rows and columns. It is logically 
    equivalent to imposing 2N alldifferent constraints, on each row and column,
    but it allows more reasoning because it considers the rows and columns 
    together. The alldifferent constraint used is the lib(fd_global) version.
    The maximum propagation occurs when the variables' domains also
    have N values
</P><P>
    This is currently a prototype -- the constraint has not been tested
    very extensively and little effort has been spent to optimise performance.
    We welcome any feedback on using this constraint.
</P><P>
    This constraint is described in J.-C. Regin and C. Gomes,
    'The Cardinality Matrix Constraint', CP 2004.
") ]).

:- use_module(fd_sequence).
:- reexport sequence_total/7,
            sequence_total/6
   from fd_sequence.

:- comment(sequence_total/7, [
        amode: sequence_total(+,+,+,+,+,+,++),
        args: ["Min":"Non-negative integer",
               "Max":"Positive integer",
               "Low":"Non-negative integer",
               "High":"Positive integer",
               "K": "Postive integer",
               "Vars": "A list of variables or integers",
               "Values": "A list of (different) integers"
              ],
        summary: "The number of values taken from Values is between Low and "
                 "High for all sequences of K variables in Vars, and the "
                 "total occurrence of each value in Vars is between Min and Max",
        see_also: [fd_global:sequence_total/6, fd:element/3, fd_global_gac:sequence/4, fd_global_gac:sequence/5], 

        desc: html("\
<P>
    This constraint ensures that the number of values taken from the set
    specified in Values is at least Low and at most High for all sequences 
    of K consecutive variables/values in Vars, and at least Min and at most
    Max in total for all Vars.
</P><P>
    This is currently a prototype -- the constraint has not been tested
    very extensively and little effort has been spent to optimise performance.
    We welcome any feedback on using this constraint.
</P>
") 
         ]
).

:- comment(sequence_total/6, [
        amode: sequence_total(+,+,+,+,+,+),
        args: ["Min":"Non-negative integer",
               "Max":"Positive integer",
               "Low":"Non-negative integer",
               "High":"Positive integer",
               "K": "Postive integer",
               "ZeroOnes": "A list of 0/1 variables or integers"
              ],
        summary: "The number of occurrences of the value 1 is between Low and "
                 "High for all sequences of K variables in ZeroOnes, and the "
                 "total occurrences of 1 in ZeroOnes is between Min and Max",

        see_also: [fd_global:sequence_total/7, fd:element/3, fd_global_gac:sequence/4, fd_global_gac:sequence/5], 
        desc: html("\
<P>
    This constraint ensures that the number of occurrences of the value 1 is 
    at least Low and at most High for all sequences of K consecutive 
    variables/values in ZeroOnes, and at least Min and at most Max in total 
    for all ZeroOnes. ZeroOnes are 0/1 variables (or integers), i.e. they 
    have the domain [0,1].
</P><P>
    The ZeroOnes can be interpreted as the fulfillment of various
    conditions if the variables are linked to these conditions. For example,
    sequence_total/7 is implemented by linking the N ZeroOnes variables to  
    a matching collection of N finite domain `original' variables using 
    element/3 constraints to constrain the ZeroOnes to be 1 if the 
    corresponding original value takes one of the specified values. The
    ZeroOnes can then be used in further constraint reasoning.
</P><P>
    This is currently a prototype -- the constraint has not been tested
    very extensively and little effort has been spent to optimise performance.
    We welcome any feedback on using this constraint.
") 
         ]
).
