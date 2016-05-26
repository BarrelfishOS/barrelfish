%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
% The Original Code is The Generalized Arc Consistent all-different global 
% constraint.    
% The Initial Developer of the Original Code is  Helmut Simonis
% Portions created by the Initial Developer are  Copyright (C)2008.
% All Rights Reserved.
% 
% Contributor(s): Helmut Simonis, 4C, University College Cork
% 
% END LICENSE BLOCK
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-module(fd_global_gac).
:- comment(categories, ["Constraints","Algorithms"]).
:-comment(summary,"Library of global constraints which achieve"
                        " generalized arc consistency").
:-comment(desc,"This library is intended for global constraints for"
               " which GAC (generalized arc consistency, also called hyper arc"
               " consistency, or domain consistency) is maintained."
               " The first example is a version of the alldifferent"
               " constraint which performs more pruning than the bound"
               " consistent version in the fd_global library.").
:-comment(author,"H. Simonis, 4C, University College Cork").
:-comment(copyright,"2008, H. Simonis, 4C, University College Cork").
:-comment(status,prototype).
:-comment(date,"2008").

:-lib(fd).
:-lib(graph_algorithms).
:- lib(max_flow).
:-lib(hash).
:-lib(lists).
:- lib(fd_generic_interface).

:- import get_bounds/3 from fd_generic_interface.

:- include(generic_global_gac).

:- export alldifferent_matrix/1,
          gcc_matrix/3.

alldifferent_matrix(Matrix) :-
        fd_global:alldifferent_matrix_internal(Matrix,fd_global_gac).

gcc_matrix(Row,Col,Matrix) :-
        fd_global:gcc_matrix_internal(Row,Col,Matrix,fd_global_gac).

:- lib(fd_sequence).
:- reexport sequence/5,
            sequence/4
   from fd_sequence.

:-comment(gcc_matrix/3,[
      summary:"Constrain the cardinality of values taken in the rows and"
              " columns of Matrix as specified by RowBounds and ColBounds,"
              " respectively", 
      amode:gcc_matrix(++,++,+),

      args:["RowBounds":"A list of M sublists with elements of the form "
                        "gcc(Low,High,Value), where Low, High and Value are "
                        "integers, and High and Low are non-negative "
                        "(High >= Low), and Value must be different from "
                        "other Values in RowBounds",
            "ColBounds":"A list of N sublists with elements of the form "
                        "gcc(Low,High,Value), where Low, High and Value are "
                        "integers, and High and Low are non-negative "
                        "(High >= Low), and Value must be different from "
                        "other Values in ColBounds",
             "Matrix":"A two dimensional MxN matrix of Variables or integer"],
      see_also: [fd_global_gac:gcc/2],
      kind:[constraint:[root:fd]],
      desc:html("\
    This constraint ensures that the cardinality (the number of occurrences)
    of values in each row and column of Matrix conforms to the specifications
    in RowBounds and ColBounds, respectively. RowBounds and ColBounds are 
    lists of triples in the form gcc(Low,High,Value) where Value is an integer,
    a value that Vars is to be assigned to, and must occur only once as a
    Value in the row/column, and whose cardinality |Value| is specified by 
    Low =< |Value| =< High, where Low and High are non-negative integers. 
    Vars cannot take values not specified in a gcc triplet.
    This constraint is logically equivalent to imposing M+N individual gcc
    constraints, on each row and column of Matrix, but allows more reasoning
    because of the interaction of the values between the rows and columns.
    The gcc used is from lib(fd_global_gac), but the extra inferences 
    performed between the rows and columns themselves may be not fully 
    domain consistent. 
</P><P>
    This is currently a prototype -- the constraint has not been tested
    very extensively and little effort has been spent to optimise performance.
    We welcome any feedback on using this constraint.
</P><P>
    This constraint is described in J.-C. Regin and C. Gomes,
    'The Cardinality Matrix Constraint', CP 2004.
")]).

:-comment(alldifferent_matrix/1,[
      summary:"Constrain the rows and columns of Matrix to be different values",
      amode:alldifferent_matrix(+),
      args:["Matrix":"A two dimensional square matrix of Variables or integer"],
      see_also:[fd_global_gac:alldifferent/1,_:alldifferent_matrix/1],
      kind:[constraint:[root:fd]],
      desc:html("\
<P>
    This constraint is a matrix version of alldifferent. Matrix is a two
    dimensional square (NxN) matrix, and the constraint ensures that the 
    elements in each row and column of the matrix are different. The same
    value can occur in different rows and columns. It is logically 
    equivalent to imposing 2N alldifferent constraints on each row and column,
    but it allows more reasoning because it consider the rows and columns 
    together. The version uses alldifferent from lib(fd_global_gac), but the 
    extra inferences performed between the rows and columns themselves not be
    fully domain consistent. The maximum propagation occurs when the 
    variables' domains also have N values. 
</P><P>
    This is currently a prototype -- the constraint has not been tested
    very extensively and little effort has been spent to optimise performance.
    We welcome any feedback on using this constraint.
</P><P>
    This constraint is described in J.-C. Regin and C. Gomes,
    'The Cardinality Matrix Constraint', CP 2004.
") ]).

:- comment(sequence/5, [
        amode: sequence(+,+,+,+,++),
        args: ["Low":"Non-negative integer",
               "High":"Positive integer",
               "K": "Postive integer",
               "Vars": "A list of variables or integers",
               "Values": "A list of (different) integers"
              ],
        summary: "The number of values taken from Values is between Low and"
                 " High for all sequences of K variables in Vars.", 
        see_also: [fd_global_gac:sequence/5,fd:element/3,fd_global:sequence_total/6,fd_global:sequence_total/7],
        kind:[constraint:[root:fd]],
        desc: html("\
<P>
    This constraint ensures that the number of values taken from the set
    specified in Values is at least Low and at most High for all sequences 
    of K consecutive variables/values in Vars. 
</P><P>
    This is currently a prototype -- the constraint has not been tested
    very extensively and little effort has been spent to optimise performance.
    We welcome any feedback on using this constraint.
</P><P>
    This constraint is known as among_seq in the global constraint catalog.
    The algorithm implemented is described in M. Maher et al.'s paper 
    'Flow-Based Propagators for the SEQUENCE and Related Global Constraints' 
    in CP'2008.
") 
         ]
).

:- comment(sequence/4, [
        amode: sequence(+,+,+,+),
        args: ["Low":"Non-negative integer",
               "High":"Positive integer",
               "K": "Postive integer",
               "ZeroOnes": "A collection of 0/1 variables or integers"
              ],
        summary: "The number of occurrences of the value 1 is between Low and"
                 " High for all sequences of K variables in ZeroOnes", 
        see_also: [fd_global_gac:sequence/5,fd:element/3,fd_global:sequence_total/6,fd_global:sequence_total/7],
        kind:[constraint:[root:fd]],
        desc: html("\
<P>
    This constraint ensures that the number of occurrences of the value 1
    is at least Low and at most High for all sequences of K consecutive 
    variables/values in ZeroOnes. ZeroOnes are 0/1 variables (or itnegers), 
    i.e. they have the domain [0,1]. 
</P><P>
    The ZeroOnes can be interpreted as the fulfillment of various
    conditions if the variables are linked to these conditions. For example,
    sequence/5 is implemented by linking the N ZeroOnes variables to a 
    matching collection of N finite domain `original' variables using 
    element/3 constraints to constrain the ZeroOnes to be 1 if the 
    corresponding original value takes one of the specified values. The
    ZeroOnes can then be used in further constraint reasoning.
</P><P>
    Note: this constraint is different from sequence/4 in lib(fd).
</P><P>
    This is currently a prototype -- the constraint has not been tested
    very extensively and little effort has been spent to optimise performance.
    We welcome any feedback on using this constraint.
") 
         ]
).

:-pragma(debug).
