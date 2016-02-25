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
% Copyright (C) 2003 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Andrew Cheadle, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
%
% Description:  Instrumentation and compile-time code weaving library
%
% System:       ECLiPSe Constraint Logic Programming System
% Author/s:     Andrew Cheadle, IC-Parc
% Version:      $Id: instrument.ecl,v 1.3 2013/02/14 01:31:05 jschimpf Exp $
%
%
% NOTES
%	- (read) macro expansion should be on during preprocessing
%	  because the result is compile_term'd subsequently.
%	  During printing, the settings should be the same, but normally
%	  leaving macro expansion off will not cause a problem and lead
%	  to nicer looking output.
%
%	- DCGs are preprocessed after expansion. They could be done
%	  before when the preprocessor knows about dcg format.
%	  The printing must print whatever is being preprocessed.
%
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
% Module definition, library dependencies and predicate import / export
% ----------------------------------------------------------------------

:- module(instrument).

:- comment(categories, ["Development Tools"]).
:- comment(summary, "Generic tool for code instrumentation").
:- comment(author,"Andrew Cheadle, based on ideas by Joachim Schimpf").
:- comment(date, "$Date: 2013/02/14 01:31:05 $").
:- comment(status, prototype).

:- comment(desc, html("<P>
   The instrument library is a tool that enables predicate definitions or all
   calls to a specific predicate to be annotated with user-defined predicates.
   These instrumentation predicates are free to perform such actions as collect
   program statistics or write debugging data to a stream during program
   execution.  Additionally, the instrumentation can be inserted and removed
   dynamically as the program executes.
</P><P>
The usage is as follows:
   <OL>
   <LI>Load the instrument library
   <PRE>
   ?- lib(instrument).
   </PRE>
   <LI>Compile your program with the instrument compiler
   <PRE>
   ?- instrument:instrument(my_program, Templates).
   </PRE>
   <LI>Run the query for which you wish to generate instrumentation data
   <PRE>
   ?- my_query(X,Y,Z).
   </PRE>
   <LI>Generate an html file containing the results. E.g. the following
   will create the result file instrument/my_program.html:
   <PRE>
   ?- instrument:results(my_program).
   </PRE>
   <LI>View the result file using any browser. The result file
   contains a pretty-printed form of the source, annotated with
   the instrumentation results.
   </OL>
   <P>
   The following is an example of very basic use of the tool.
   Consider instrumenting a simple path finding program saved in a
   file called 'instrument_example.ecl.
   <PRE>
   list_member(X, [X | _Tail]).
   list_member(X, [_ | Tail]):-
   list_member(X, Tail).
 
   all_edges(Graph, Source, Outgoing):-
       findall((Source-Next),list_member((Source-Next), Graph), Outgoing).
 
   path(_Graph, Source, Source, []).
   path(Graph, Source, Sink, [(Source-Next) | Path]):-
       all_edges(Graph, Source, Outgoing),
       list_member((Source-Next), Outgoing),
       path(Graph, Next, Sink, Path).
 
   test(Path):-
       path([1-2, 1-3, 2-4, 3-4, 4-5, 4-6, 5-7, 5-8, 6-7,
             1-8, 8-9, 8-10, 9-10, 9-11, 10-11],
             1,
             7,
             Path).
   </PRE>
   The simplest way to instrument your code is to insert predicates around
   every call in the source. The following code demonstrates how to
   print the CPU time before and after every predicate call:
   <PRE>
   get_time(Cpu):-
       statistics(times, [Cpu, _Sys, _Real]).
 
   time_point:-
       get_time(T),
       writeln(error, T).
 
   go:-
       File = instrument_example,
       GlobalTemplate = itemplate{subgoal_start:time_point/0,
                                         subgoal_end:time_point/0},
       instrument(File, GlobalTemplate),
       test(Path).
   </PRE>
   Note the same predicate <TT>time_point/0</TT> is specified before and
   after goal calls. If we wished to instrument all calls except those
   to <TT>list_member/2</TT>, the following call to <TT>instrument/2</TT> is
   made:
   <PRE>
        instrument(File, GlobalTemplate - [list_member/2])
   </PRE>
   In general any number of goals may be specified in this 'exclusion list'.
   <P> 
   The following code demonstrates alternative instrumentation
   for this excluded predicate:
   <PRE>
   :-lib(instrument).

   :-export time_point/0, special_point/0.

   get_time(Cpu):-
       statistics(times,[Cpu, _Sys, _Real]).        

   time_point:-
       get_time(T),
       writeln(error, T).

   special_point:-
       writeln(error, 'start, end, redo or fail').

   go:-
       File = instrument_example,
       GlobalTemplate = itemplate{subgoal_start:time_point/0,
                                        subgoal_end:time_point/0},
       SpecialTemplate = itemplate{call_start:special_point/0,
                                         call_end:special_point/0,
                                         call_fail:special_point/0
                                         call_redo:special_point/0},
       instrument(File,[GlobalTemplate - [list_member/2],
                        (list_member/2) = SpecialTemplate]),
       test(Path).
   </PRE>
   Notice how the <TT>special_point/0</TT> predicate is assigned to the
   <TT>call_start</TT>, <TT>call_end</TT>, <TT>call_fail</TT> and 
   <TT>call_redo</TT> points in this example. This ensures that the 
   <TT>special_point</TT> predicate is called if <TT>list_member/2</TT> 
   fails, or if resatisfiable, executed at the redo.
   <P>
   Using arity-1 predicates (i.e. one argument predicates)
   unique identification of the callsite can be obtained:
   <PRE>
   :-lib(instrument).

   get_time(Cpu):-
       statistics(times, [Cpu, _Sys, _Real]).        

   time_point(CallSite):-
       get_time(T),
       writeln(error,['Time', T, 'at callsite', CallSite]).

   go:-
       File = instrument_example,
       GlobalTemplate = itemplate{subgoal_start:time_point/1,
                                        subgoal_end:time_point/1},
       instrument(File, GlobalTemplate),
       test(Path).
   </PRE>
   <P>
   By supplying a predicate to the <TT>result</TT> field of a template 
   one can specify terms to be printed within a copy of the source code.
   Using this feature along with the utility predicates 
   <TT>get_callsite_data/2</TT> and <TT>set_callsite_data/2</TT> 
   one can create quite varied and useful output.
   <PRE>
   :-lib(instrument).

   get_time(Cpu):-
       statistics(times, [Cpu, _Sys, _Real]).        

   time_point(CallSite):-
       get_time(T),
       set_callsite_data(CallSite, T).

   result(CallSite, _Type, _Module, Goal, NewGoal):-
       get_callsite_data(CallSite, Time),
       NewGoal='instrument:point_before'(Time, Goal).
   go:-
       File = instrument_example,
       GlobalTemplate = itemplate{subgoal_start:time_point/1,
                                        result:result/5},
       instrument(File, GlobalTemplate),
       test(Path),
       file_result(File).
   </PRE>
   No data is printed during the running of the <TT>test(Path)</TT> call, 
   but the <TT>file_result(File)</TT> call causes the source code to be 
   emitted (color coded) with the given callsite data embedded.
</P>")).

:- lib(source_processor).
:- lib(xref).
:- lib(graph_algorithms).
:- lib(pretty_printer).

:- export instrument/2, instrument/3, file_result/1, file_result/2,
   module_result/0, module_result/1, instrument_control/2,
   erase_all_templates/0, erase_module_templates/0, 
   erase_file_templates/1, file_callsites/3, module_callsites/2, 
   set_callsite_data/2, get_callsite_data/2, defined_modules/2.


% ----------------------------------------------------------------------
% Instrumentation template and book-keeping stores
% ----------------------------------------------------------------------

:- comment(struct(itemplate),
        [ summary:" The template used to guide predicate instrumentation",
          fields:[clause_start : "PredSpec of maximum arity two, the atom "
                                 "'inherit', or variable", 
                  clause_end : "PredSpec of maximum arity two, the atom "
                               "'inherit', or variable",
                  clause_fail : "PredSpec of maximum arity two, the atom "
                                "'inherit', or variable",
                  clause_redo : "PredSpec of maximum arity two, the atom "
                                "'inherit', or variable",
                  block_start : "PredSpec of maximum arity two, the atom "
                                "'inherit', or variable",
                  block_end : "PredSpec of maximum arity two, the atom "
                              "'inherit', or variable",
                  block_fail : "PredSpec of maximum arity two, the atom "
                               "'inherit', or variable",
                  block_redo : "PredSpec of maximum arity two, the atom "
                               "'inherit', or variable",
                  subgoal_start : "PredSpec of maximum arity two, the atom "
                                  "'inherit', or variable",
                  subgoal_end : "PredSpec of maximum arity two, the atom "
                                "'inherit', or variable",
                  subgoal_fail : "PredSpec of maximum arity two, the atom "
                                 "'inherit', or variable",
                  subgoal_redo : "PredSpec of maximum arity two, the atom "
                                 "'inherit', or variable",
                  call_start : "PredSpec of maximum arity two, the atom "
                               "'inherit', or variable", 
                  call_end : "PredSpec of maximum arity two, the atom "
                             "'inherit', or variable",
                  call_fail : "PredSpec of maximum arity two, the atom "
                              "'inherit', or variable",
                  call_redo : "PredSpec of maximum arity two, the atom "
                              "'inherit', or variable",
                  fact : "PredSpec of maximum arity one, the atom "
                         "'inherit', or variable",
                  inbetween : "PredSpec of maximum arity two, the atom "
                              "'inherit', or variable",
                  result : "PredSpec of maximum arity five, the atom "
                           "'inherit', or variable",
                  meta_args : "List of itemplate or variable",
                  exclude : "List of PredSpec, the atom 'inherit', or"
                            " variable",
                  code_weaver : "PredSpec of maximum arity six, the atom "
                              "'inherit', or variable",
                  asserted : "Specific atom, the atom 'inherit' or variable",
                  module_scope : "Atomic module name, the atom 'inherit' or variable",
                  file_local : "Specific atom, the atom 'inherit' or variable",
                  goal_expansion : "Specific atom, the atom 'inherit' or variable"
                 ],
          see_also:[erase_all_templates/0, get_callsite_data/2, instrument/2, 
                    instrument/3, instrument_control/2, library(instrument), 
                    module_callsites/2, module_result/0, 
                    set_callsite_data/2, defined_modules/2],
          desc:html("<P>
   The <TT><B>itemplate</B></TT> structure serves as a specification that 
   determines how predicate definitions and calls to predicates are instrumented.
   </P><P>
   An <TT>itemplate</TT> is associated with a predicate whose definition or 
   invocations are to be instrumented. This association is specified as an 
   argument to <TT>instrument:instrument/2</TT> or <TT>instrument:instrument/3</TT> 
   and is of the form: <TT>PredSpec = itemplate{...}</TT>.
   </P><P>
   <TT>PredSpec</TT> is of the form <TT>Module:Functor/Arity</TT>. 
   If the module qualifier is omitted, then <TT>PredSpec</TT> is assumed to 
   be a predicate defined within the calling module context in which 
   <TT>instrument/2</TT> was invoked. The definition context for a module 
   can be wildcarded by specifying the module qualifier as the atom 
   <TT>every_module</TT> or by setting the <TT>itemplate module_scope</TT> 
   field to <TT>every_module</TT>.
   </P><P>
   This has two effects:
   <OL>
   <LI> When instrumenting predicate definitions a template can be applied 
        to a predicate regardless of the module it is defined in.
   <LI> When instrumenting predicate calls a template can be applied to a 
        predicate regardless of whether it is module qualified or not.
   </OL>
   However, as a general rule of thumb, predicate specifications for 
   <TT>PredSpec</TT> and instrumentation predicates within the templates 
   should be correctly module qualified. 
   </P><P>
   When determining whether a predicate is to be instrumented, a template is 
   sought which matches the module qualified <TT>PredSpec</TT>, first using 
   the named module (or current module if unqualified) and then using 
   <TT>every_module</TT> as the module qualifier.
   </P><P>
   Within the template, predicate specifications supplied to its fields 
   determine how predicate instrumentation and invocation proceeds. 
   The template specifies the following instrumentation points:
   <DL><DT>
   <PRE>clause_start</PRE>
   <DD>
   A call to the specified predicate is placed at the beginning of each 
   clause definition of the predicate the template is being applied to.
   <DT>
   <PRE>clause_end</PRE>
   <DD>
   A call to the specified predicate is placed at the end of each clause 
   definition of the predicate the template is being applied to.
   <DT>
   <PRE>clause_fail</PRE>
   <DD>
   A call to the specified predicate is placed such that failure within 
   the clause definition of the predicate the template is being applied 
   to results in its invocation.
   <DT>
   <PRE>clause_redo</PRE>
   <DD>
   A call to the specified predicate is placed such that failure and 
   re-execution (i.e. resatisfiable execution) within the clause 
   definition of the predicate the template is being applied to results 
   in its invocation.
   <DT>
   <PRE>block_start</PRE>
   <DD>
   A call to the specified predicate is placed at the beginning of each 
   conjunction (comma-sequences of subgoals) within the definition of 
   the predicate the template is being applied to.
   <DT>
   <PRE>block_end</PRE>
   <DD>
   A call to the specified predicate is placed at the end of each 
   conjunction within the definition of the predicate the template is 
   being applied to.
   <DT>
   <PRE>block_fail</PRE>
   <DD>
   A call to the specified predicate is placed such that failure of each 
   conjunction within the definition of the predicate the template is being 
   applied to results in its invocation.
   <DT>
   <PRE>block_redo</PRE>
   <DD>
   A call to the specified predicate is placed such that failure and 
   re-execution (i.e. resatisfiable execution) of each conjunction within 
   the definition of the predicate the template is being applied to results 
   in its invocation.
   <DT>
   <PRE>subgoal_start</PRE>
   <DD>
   A call to the specified predicate is placed at the beginning of each 
   subgoal within the definition of the predicate the template is being 
   applied to.
   <DT>
   <PRE>subgoal_end</PRE>
   <DD>
   A call to the specified predicate is placed at the end of each subgoal 
   within the definition of the predicate the template is being applied to.
   <DT>
   <PRE>subgoal_fail</PRE>
   <DD>
   A call to the specified predicate is placed such that failure of each 
   subgoal within the definition of the predicate the template is being 
   applied to results in its invocation.
   <DT>
   <PRE>subgoal_redo</PRE>
   <DD>
   A call to the specified predicate is placed such that failure and 
   re-execution (i.e. resatisfiable execution) of each subgoal within 
   the definition of the predicate the template is being applied to results 
   in its invocation.
   <DT>
   <PRE>call_start</PRE>
   <DD>
   A call to the specified predicate is placed at the beginning of each 
   call invocation of the predicate the template is being applied to.
   <DT>
   <PRE>call_end</PRE>
   <DD>
   A call to the specified predicate is placed at the end of each call 
   invocation of the predicate the template is being applied to.
   <DT>
   <PRE>call_fail</PRE>
   <DD>
   A call to the specified predicate is placed such that failure of a call 
   invocation of the predicate the template is being applied to results in 
   its invocation.
   <DT>
   <PRE>call_redo</PRE>
   <DD>
   A call to the specified predicate is placed such that failure and 
   re-execution (i.e. resatisfiable execution) of a call invocation of the 
   predicate the template is being applied to results in its invocation.
   <DT>
   <PRE>fact</PRE>
   <DD>
   A call to the specified predicate is placed as the clause body of the 
   fact predicate the template is being applied to. A fact is a predicate 
   definition with no defined clause body.
   <DT>
   <PRE>inbetween</PRE>
   <DD>
   A call to the specified predicate is placed at the end of each subgoal 
   of a conjunction within the definition of the predicate the template is 
   being applied to.
   </DL>
   </P><P>
   The default value for the instrumentation predicates is that none are 
   defined and so no instrumentation is performed. This is equivalent to 
   setting the field values explicitly to free variables.
   </P><P>
   The instrumentation predicates must be defined with one of the following 
   signatures:
   <DL><DT>
   Arity 0
   <DD>
   When an arity zero instrumentation predicate is specified, it is invoked 
   with no arguments passed.
   <DT>
   Arity 1
   <DD>
   Each code instrumentation point within a module is uniquely identified 
   by its callsite identifier. The callsite identifier is a monotonically 
   increasing integer incrementing from the initial value of 0. It is the 
   callsite identifier value that is passed to an arity one instrumentation 
   predicate.
   <DT>
   Arity 2
   <DD>
   An arity two instrumentation predicate is passed the callsite identifier 
   in argument position one and an auxiliary variable in argument position 
   two. The same auxiliary variable is passed as argument two to the start, 
   end and fail instrumentation points (i.e. it is common to <TT>clause_start</TT>, 
   <TT>clause_end </TT>, <TT>clause_fail</TT> and <TT>clause_redo</TT> while a 
   different auxiliary variable is common to <TT>block_start</TT>, 
   <TT>block_end</TT>, <TT>block_fail</TT> and <TT>block_redo</TT>, etc).
   </DL>
   </P><P>
   It is anticipated that the callsite identifier be used for executing 
   callsite specific code or storing data pertinent to the callsite in a 
   non-logical store keyed by callsite identifier - 
   <TT>set_callsite_data/2</TT> and <TT>get_callsite_data/2</TT> are provided 
   for exactly this purpose.
   </P><P>
   The auxiliary variable passed as argument two to instrumentation 
   predicates is provided for convenience for capturing 'delta' measurements 
   between the start, end and fail instrumentation points. The variable is a 
   logical variable and while the value passed to the end or fail predicate 
   is guaranteed to be the value bound by the start predicate, backtracking 
   past the start predicate results in the unbinding of the variable. If the 
   captured delta should be retained beyond backtracking then it should be 
   placed in the callsite's non-logical store using <TT>set_callsite_data/2</TT>.
   </P><P>
   The maximum arity of the fact and inbetween point predicates is one - 
   only the callsite identifier is passed, there is no benefit in passing an 
   auxiliary variable.
   </P><P>
   The result instrumentation predicate provides a mechanism for 
   pretty-printing the annotated source code with the instrumentation results 
   gathered during execution. By executing <TT>instrument:module_result/0</TT>
   or <TT>instrument:file_result/1</TT> the predicate specified for result 
   instrumentation within the template is invoked as each of the 
   instrumentation points are encountered for pretty-printing.
   </P><P>
   The result instrumentation predicate must be defined with one of the 
   following signatures:
   <DL> <DT>
   Arity 0
   <DD>
   When an arity zero predicate is specified, it is invoked with no 
   arguments passed.
   <DT>
   Arity 1
   <DD>
   The callsite identifier representing the instrumentation point is 
   passed to an arity one result predicate.
   <DT>
   Arity 2
   <DD>
   The instrumentation point type is passed as argument two of an arity 
   two result predicate. The point type is the atom associated with the 
   point, for example <TT>call_start, call_end, call_fail or call_redo</TT>, 
   etc. 
   <DT>
   Arity 3
   <DD>
   The module into which the file being pretty-printed was instrumented 
   and compiled is passed as argument three of an arity three result predicate.
   <DT>
   Arity 4
   <DD>
   The goal appearing in the source code around which instrumentation was 
   originally placed is passed as argument four of an arity four result 
   predicate.
   <DT>
   Arity 5
   <DD>
   The fifth argument of an arity five result predicate is a result goal 
   that can be returned to the pretty-printer to be placed in the 
   pretty-printed output in the place of the fourth argument goal. This 
   allows the goal to be annotated with commentary or instrumentation results.
   </DL>
   </P><P>
   The <TT>meta_args</TT> field of the <TT>itemplate</TT> structure is 
   applicable only to templates associated with predicates that are 
   meta-predicates. When applicable, <TT>meta_args</TT> is a list of 
   <TT>itemplate</TT>. Each element in the list is a template for the 
   corresponding argument of the meta-predicate. The template defined 
   instrumentation points are applied to the code found inside the 
   meta-predicate at this argument position. For example: 
   <PRE>findall/3 = itemplate{..., meta_args:[_, ITemplateArg2, _]...}</PRE> 
   is an <TT>itemplate</TT> specification describing the instrumentation 
   of the meta-predicate <TT>findall/3</TT>. Argument one and three undergo 
   no instrumentation (denoted by free variables) and argument two is 
   instrumented according to <TT>ITemplateArg2</TT>.
   </P><P>
   Within the meta-predicate argument <TT>itemplate</TT> fields may be 
   specified as being <TT>inherit</TT>-ed. When such a field is specified 
   as inherited it is set to the corresponding value of the template used in 
   instrumenting the definition of the predicate in which the call to the 
   meta-predicate resides. 
   </P><P>
   The <TT>exclude</TT> field of the <TT>itemplate</TT> contains a list of 
   predicate specifications. Any occurrence of such a predicate as a call 
   or subgoal is excluded from application of the instrumentation specified 
   by the enclosing template. The main use of exclusion is in preventing 
   instrumentation application to recursive predicates or built-ins.
   </P><P>
   The <TT>code_weaver</TT> field of the <TT>itemplate</TT> specifies a 
   predicate of arity six that is a compile-time callback allowing arbitrary 
   user code to be woven into the code currently undergoing compilation. The 
   weaving of user code is performed before the insertion of instrumentation 
   predicates. The arguments of the predicate are as follows:
   <DL> <DT>
   File
   <DD>
   The name of the file currently undergoing compilation.
   <DT>
   Code
   <DD>
   The block of code available for manipulation by the user specified code 
   weaving predicate.
   <DT>
   Type
   <DD>
   The ECLiPSe construct type of the 'Code' block, one of: 
   <TT>clause, head, body, fact, variable, conjunction, disjunction, 
   conditional, do, goal</TT>. The decomposition of code blocks into these 
   various constructs is for convenience to save the weaver predicate from 
   having to match out the constructs itself. It is however free to do so 
   by operating solely on the <TT>clause</TT> construct.
   <DT>
   WeavedCode
   <DD>
   The block of code that results from the weaving of the Code' block with 
   the arbitrary user code. 
   <DT>
   Mode
   <DD>
   The mode that code weaving is proceeding in, either <TT>compile</TT>, 
   during compilation, or <TT>print</TT> during pretty-printing.
   <DT>
   Module
   <DD>
   The module the code is being compiled into.
   </DL>
   </P><P>
   The remaining fields of the <TT>itemplate</TT> structure specify the 
   options:
   <DL><DT>
   <TT>asserted</TT> (default:free variable)
   <DD><P>
   The value of asserted may be the atoms: <TT>on</TT>, <TT>off</TT> and 
   <TT>post_compile</TT> or a free variable. When a free variable, the 
   instrumentation predicates are compiled into the code like any other 
   predicates. However, when set to one of the atomic values, the predicates 
   are compiled such that they can be inserted and removed at runtime. This 
   is done efficiently such that there is negligible overhead on execution.
   </P><P>
   The value <TT>on</TT> specifies that the predicates are initially inserted.
   The value <TT>off</TT> that they are removed and <TT> post_compile</TT> 
   that they are not inserted until compilation of the whole file has 
   completed. The <TT>post_compile</TT> option is provided so that 
   instrumentation predicates inserted into predicate definitions that get 
   executed at compile-time do not get executed.
   </P><P>
   During execution, the instrumentation predicates can be inserted and 
   removed using <TT>instrument:instrument_control/2</TT>. 
   <P><DT>
   <TT>module_scope</TT> (default:free variable)
   <DD><P>
   The possible values are an atom representing a module name, the atom 
   <TT>every_module</TT> or a free variable indicating the current module. 
   The value is used to determine the module definition context of 
   unqualified instrumentation predicates or the predicate associated with 
   the template for definition and call instrumentation:
   <DL><DT>
   Named module
   <DD><P>
   The unqualified predicate is qualified with the named module.
   <P><DT>
   Free variable
   <DD><P>
   The unqualified predicate is qualified with the calling module context 
   in which <TT>instrument/2</TT> was first invoked.
   <P><DT>
   <TT>every_module</TT>
   <DD><P>
   The unqualified predicate is qualified with the name of the current 
   compilation module.
   <P></DL><P><DT>
   <TT>file_local</TT> (default:<TT>off</TT>)
   <DD><P>
   Templates persist in a global store between successive calls to 
   <TT>instrument/2</TT> and <TT>instrument/3</TT>. If it is undesirable 
   for a template to be added to the global store (thus making it available 
   for the instrumentation of other files and modules) an <TT>itemplate</TT> 
   may be declared as being applicable to only the file currently being 
   instrumented by setting this option to <TT>on</TT>.
   </P><P>
   The search order for an instrumentation template is first in the file 
   local store and then in the global store.
   <P><DT>
   <TT>goal_expansion</TT> (default:<TT>on</TT>)
   <DD><P>
   Setting this to <TT>off</TT> will suppress goal expansion inlining) 
   during compilation. This may be necessary when the processed code 
   contains predicates that get executed at compile time. 
   </P></DL><P>")]).

% Instrumentation template
:- export struct( itemplate(   clause_start, 
                               clause_end,
                               clause_fail,
                               clause_redo,
                               block_start,
                               block_end,
                               block_fail,
                               block_redo,
                               subgoal_start,
                               subgoal_end,
                               subgoal_fail,
                               subgoal_redo,
                               call_start, 
                               call_end,
                               call_fail,
                               call_redo,
                               fact,
                               inbetween,
                               result,
                               meta_args,
                               exclude,
                               code_weaver,
                               asserted,
                               module_scope,
                               file_local,
                               goal_expansion
                           )).

% Hash table mapping Module:PredSpec -> itemplate
:- local store(itemplates).

% Hash table mapping Module -> Module:File to file local template hashes
:- local store(file_local_templates).

% Hash table mapping Module or Module:File to the next 
% available call site / insertion point value for that module.
% For a file:   Module:File is key StartPoint:EndPoint is value
%       module: Module is the key  EndPoint is the value
:- local store(insertion_point_values).

% Hash table mapping Module to store handle for user data
% associated with a particular callsite.
:- local store(module_callsite_store).

:- local variable(verboseLevel).

% ----------------------------------------------------------------------
% Library initialisation / finalisation
% ----------------------------------------------------------------------

% The basic itemplates are a global template that does nothing -
% Before 'call' instrumentation, there was no need to process a clause
% for which a template doesn't exist. However, with the ability to
% instrument calls, the clause must be processed regardless and the 
% neatest way is just to have an empty default template returned.
%
% Define default meta templates for ;/2 and ->/2 that have their
% arguments treated as blocks and the instrumentation inherited
% from the definition context.
add_default_itemplates(Store, FileLocal) :-
        % Add a default global if it doesn't exist
        ( store_get(Store, every_module:iglobal, _) ->
            true
        ;
            store_set(Store, every_module:iglobal, 
                      itemplate with [exclude:[],
                                      module_scope:every_module,
                                      file_local:FileLocal,
                                      goal_expansion:on])
        ),
        DefaultMetaArgTemplate = itemplate with [clause_start:inherit,
                                                 clause_end:inherit,
                                                 clause_fail:inherit,
                                                 clause_redo:inherit,
                                                 block_start:inherit,
                                                 block_end:inherit,
                                                 block_fail:inherit,
                                                 block_redo:inherit,
                                                 subgoal_start:inherit,
                                                 subgoal_end:inherit,
                                                 subgoal_fail:inherit,
                                                 subgoal_redo:inherit,
                                                 call_start:inherit,
                                                 call_end:inherit,
                                                 call_fail:inherit, 
                                                 call_redo:inherit, 
                                                 fact:inherit, 
                                                 inbetween:inherit, 
                                                 result:inherit, 
                                                 exclude:inherit,
                                                 asserted:inherit, 
                                                 module_scope:every_module,
                                                 file_local:FileLocal,
                                                 goal_expansion:inherit],
        MetaArgTemplates = [DefaultMetaArgTemplate, DefaultMetaArgTemplate],
        % Add a default -> if it doesn't exist
        ( store_get(Store, every_module:(->)/2, _) ->
            true
        ;
            % Note: call instrumentation may not be inserted around '->'
            store_set(Store, every_module:(->)/2, 
                             itemplate with [result:inherit, exclude:inherit,
                                             asserted:inherit,
                                             meta_args:MetaArgTemplates,
                                             module_scope:every_module,
                                             file_local:FileLocal, 
                                             goal_expansion:inherit])
        ),
        % Add a default ; if it doesn't exist
        ( store_get(Store, every_module:(;)/2, _) ->
            true
        ;
            store_set(Store, every_module:(;)/2, 
                             itemplate with [call_start:inherit, call_end:inherit,
                                             call_fail:inherit, call_redo:inherit,
                                             result:inherit, exclude:inherit,
                                             asserted:inherit, meta_args:MetaArgTemplates,
                                             module_scope:every_module,
                                             file_local:FileLocal, goal_expansion:inherit])
        ).


% Initialise module for first use and install the basic templates
:- local initialization(add_default_itemplates(itemplates, off)).

% ToDo Handle erasing of modules?


% ----------------------------------------------------------------------
% Predicates for removal of templates
% ----------------------------------------------------------------------


:- comment(erase_all_templates/0, 
        [ summary: "Erase all instrument template stores.",
          args:[],
          fail_if:no,
          resat:no,
          desc: html("<P>
   This predicate erases all <TT>itemplate</TT> that are persisted in 
   the global and file local template stores between successive calls 
   to <TT>instrument/2</TT> and <TT>instrument/3</TT>."),
          see_also:[erase_module_templates/0, erase_file_templates/1, 
                    instrument/2, instrument/3, library(instrument), 
                    struct(itemplate), defined_modules/2]
        ]).

erase_all_templates :-
        store_erase(itemplates),
        store_erase(file_local_templates),
        add_default_itemplates(itemplates, off).


:- comment(erase_module_templates/0, 
        [ summary: "Erase all instrument template stores for a specific module.",
          args:[],
          fail_if: "No instrumentation templates exist for the calling module.",
          resat:no,
          desc: html("<P>
   This predicate erases all templates of the calling module context, 
   that are persisted in the global and file local template stores 
   between successive calls to <TT>instrument/2</TT> and <TT>instrument/3</TT>. 
   The predicate is a tool and the templates of a module other than the 
   current calling module context can be erased by invoking 
   <TT>erase_module_templates@Module</TT>. To erase templates 
   applicable to all modules, the atom <TT>every_module</TT> may be 
   specified."),
          see_also:[erase_all_templates/0, erase_file_templates/1, 
                    instrument/2, instrument/3, library(instrument), 
                    struct(itemplate), defined_modules/2]
        ]).

:- tool(erase_module_templates/0, erase_module_templates/1).

erase_module_templates(Module) :-
        stored_keys(itemplates, Keys),
        ( foreach( Key, Keys), 
          param(Module, Succeed) 
        do
            ( Key = Module ->
                store_delete(itemplates, Key),
                Succeed = true
            ;
                ( Key = Module:_PredSpec ->
                    store_delete(itemplates, Key),
                    Succeed = true
                ;
                    true
                )
            )
        ),
        ( var(Succeed) ->
            fail
        ;
            ( Module == every_module ->
                add_default_itemplates(itemplates, off)
            ;
                true
            )
        ).


:- comment(erase_file_templates/1, 
        [ summary: "Erase the file local instrument template store for a
   specific file.",
          amode:erase_file_templates(+),
          args:["File" : "Atom or string"],
          fail_if: "No instrumentation templates exist for the file in 
   the calling module context.",
          resat:no,
          desc: html("<P>
   This predicate erases all templates associated with the named file 
   that was compiled into the the calling module context and persisted 
   by (successive) calls to <TT>instrument/2</TT> and <TT>instrument/3</TT>. 
   The predicate is a tool and the file local templates of a module other 
   than the current calling module context can be erased by invoking 
   <TT>erase_file_templates(File)@Module</TT>."),
          see_also:[erase_all_templates/0, erase_module_templates/0, 
                    instrument/2, instrument/3, library(instrument), 
                    struct(itemplate), defined_modules/2]
        ]).

:- tool(erase_file_templates/1, erase_file_templates/2).

erase_file_templates(File, Module) :-
        suffixed_canonical_path_name(File, CanonicalFile),
        store_delete(file_local_templates, Module:CanonicalFile).


% ----------------------------------------------------------------------
% Top-level 'instrument' predicate
% ----------------------------------------------------------------------

:- comment(instrument/2,
        [ summary:"Compile a file, inserting predicate instrumentation",
          args:["File":"Atom or string", 
                "ITemplates" : "itemplate, List of itemplate or PredSpec"],
          amode:instrument(+, +),
          exceptions:[4 : "ITemplates is not instantiated.", 
                      5 : "ITemplates is not of the appropriate type."],
          see_also:[erase_all_templates/0, file_result/1, file_result/2, 
                    file_callsites/3, get_callsite_data/2, instrument/3, 
                    instrument_control/2, library(instrument), 
                    module_callsites/2, module_result/0, 
                    module_result/1, set_callsite_data/2, defined_modules/2],
          desc:html("<P>
   This is a variant of the ECLiPSe compiler that inserts user specified 
   instrumentation predicates into the compiled code. This code can then 
   be run, and the results analysed by printing them using 
   <TT>instrument:module_result/0</TT> or <TT>instrument:file_result/1</TT>.
   <P>
   ITemplates can be specified as:
   <DL><DT>
   PredSpec of arity two
   <DD>
   If an arity two predicate specification is supplied, then this predicate 
   is invoked in order to retrieve the template for the predicate being 
   instrumented. Argument one is the predicate specification of the 
   predicate undergoing instrumentation. Argument two is an output variable
   that is to be returned by this predicate as the template to use. The 
   returned <TT>itemplate</TT> will be validated before instrumentation 
   proceeds.
   <DT>
   <TT>itemplate</TT>
   <DD>
   A single template passed to <TT>instrument/2</TT> or <TT>instrument/3</TT> 
   is adopted as the <B>global</B> template to be used for predicate 
   instrumentation. If a template for the predicate currently being 
   instrumented is not found in the template stores then if a global template 
   exists, it is used.
   <DT>
   <TT>PredSpec = itemplate</TT>
   <DD>
   A template specification for instrumentation of a single predicate. If 
   however, PredSpec is not actually a valid predicate specification but 
   the atom <TT>iglobal</TT>, the template is adopted as the global template 
   (see above).
   <DT>
   List of <TT>itemplate</TT>
   <DD>
   The list is of the form: 
   <PRE>[itemplate{...} | PredSpec = itemplate{...} ... 
   PredSpec = itemplate{...}]</PRE>
   The <TT>itemplate</TT> at the head of the list is specifies the global 
   template and is optional. The remaining elements of the list are 
   <TT>itemplate</TT> specifying instrumentation for the PredSpec with which 
   they are associated.
   </DL><P>
   In addition to supplying the global template, an exclude list of 
   PredSpec for predicates that should not be instrumented by the global 
   template can be specified:
   <PRE>itemplate{...} - 
   predicate/1, my_module:my_predicate/8,...]</PRE>")
        ]).

:- tool(instrument/2, instrument_body/3).

instrument_body(File, Templates, Module) :-
        set_verbosity_level(off),
        instrument_body(File, Templates, _, [], Module).


:- comment(instrument/3,
        [ summary:"Compile a file, inserting predicate instrumentation",
          args:["File":"Atom or string", 
                "ITemplates" : "itemplate, List of itemplate or PredSpec",
                "OptionList" : "List of Name:Value pairs"],
          amode:instrument(+, +, +),
          exceptions:[4 : "ITemplates is not instantiated.", 
                      5 : "ITemplates is not of the appropriate type."],
          see_also:[erase_all_templates/0, file_result/1, file_result/2, 
                    file_callsites/3, get_callsite_data/2, instrument/3, 
                    instrument_control/2, library(instrument), 
                    module_callsites/2, module_result/0, 
                    module_result/1, set_callsite_data/2, defined_modules/2],
          desc:html("<P>
   This is a variant of the ECLiPSe compiler that inserts user specified 
   instrumentation predicates into the compiled code. 
   This code can then be run, and the results analysed by printing them using 
   <TT>instrument:module_result/0</TT> or <TT>instrument:file_result/1</TT>.
   <P>
   ITemplates can be specified as:
   <DL><DT>
   PredSpec of arity two
   <DD>
   If an arity two predicate specification is supplied, then this predicate 
   is invoked in order to retrieve the template for the predicate being 
   instrumented. Argument one is the predicate specification of the 
   predicate undergoing instrumentation. Argument two is an output variable th
   at is to be returned by this predicate as the template to use. The 
   returned <TT>itemplate</TT> will be validated before instrumentation 
   proceeds.
   <DT>
   <TT>itemplate</TT>
   <DD>
   A single template passed to <TT>instrument/2</TT> or <TT>instrument/3</TT> 
   is adopted as the <B>global</B> template to be used for predicate 
   instrumentation. If a template for the predicate currently being 
   instrumented is not found in the template stores then if a global template 
   exists, it is used.
   <DT>
   <TT>PredSpec = itemplate</TT>
   <DD>
   A template specification for instrumentation of a single predicate. If 
   however, PredSpec is not actually a valid predicate specification but the 
   atom <TT>iglobal</TT>, the template is adopted as the global template 
   (see above).
   <DT>
   List of <TT>itemplate</TT>
   <DD>
   The list is of the form: 
   <PRE>[itemplate{...} | PredSpec = itemplate{...} ... 
   PredSpec = itemplate{...}]</PRE>
   The <TT>itemplate</TT> at the head of the list is specifies the global 
   template and is optional. The remaining elements of the list are 
   <TT>itemplate</TT> specifying instrumentation for the PredSpec with which 
   they are associated.
   </DL><P>
   In addition to supplying the global template, an exclude list of PredSpec 
   for predicates that should not be instrumented by the global template can 
   be specified: <PRE>itemplate{...} - predicate/1, 
   my_module:my_predicate/8,...]</PRE>
   <P>
   OptionList may contain the following options:
   <DL><DT>
   <TT>erase_templates</TT>
   <DD>
   Specifying this option results in the removal of instrumentation templates 
   in file local and/or global template stores. The 'Value' portion of the 
   option can be specified as an atom representing a specific module, the 
   atom <TT>all</TT> or the atom <TT>every_module</TT>.
   <DT>
   <TT>instrument_recursive</TT> (default:<TT>off</TT>)
   <DD>
   Specifying this option as <TT>on</TT> indicates that instrumentation 
   should be applied within recursive predicates. With it <TT>off</TT> the 
   net effect is that instrumentation is placed around the recursive 
   call-graph (unless no applicable template exists). This prevents 
   incorrect aggregation of accounting by instrumentation predicates. 
   <DT>
   <TT>file_local_templates</TT> (default:<TT>off</TT>)
   <DD>
   Specifying this option as <TT>on</TT> indicates that all templates in 
   ITemplates should be stored locally to the file - they are not available 
   for use in instrumenting other files (in other modules). The 
   <TT>file_local</TT> option of the <TT>itemplate</TT> in ITemplates are 
   overridden by this option.
   <DT>
   <TT>ignore_module_templates</TT> (default:<TT>off</TT>)
   <DD>
   Specifying this option as <TT>on</TT> results in the use of file local 
   templates only during instrumentation. Templates are not sought from 
   the global store.
   <DT>
   <TT>verbose</TT> (default:<TT>off</TT>)
   <DD>
   If set to <TT>on</TT>, the instrument preprocessor will print predicate 
   names as they are processed (to log_output). If set to <TT>debug</TT>, 
   the instrument preprocessor prints each instrumented and/or woven 
   predicate definition (to log_output).
   </DL>
   <P>")
        ]).

:- tool(instrument/3, instrument_body_check_options/4).

instrument_body_check_options(File, Templates, OptionList0, Module) :-
        ( var(OptionList0) -> OptionList0 = [] ; true),
        ( delete(erase_templates:ETopt, OptionList0, OptionList1) ->
            (ETopt == all ->
                erase_all_templates
            ;
                erase_module_templates(ETopt)
            )
        ;
            OptionList1 = OptionList0
        ),
        ( delete(file_local_templates:FLTopt, OptionList1, OptionList2) ->
            valid_on_off_option(FLTopt),
            (FLTopt == on ->
                suffixed_canonical_path_name(File, FLT),
                store_create(FileLocalItemplates),
                add_default_itemplates(FileLocalItemplates, on),
                store_set(file_local_templates, Module:FLT, FileLocalItemplates)
            ;
                true
            )
        ;
            OptionList2 = OptionList1
        ),
        ( delete(verbose:VerboseLevel, OptionList2, OptionList) ->
            valid_verbosity_option(VerboseLevel),
            set_verbosity_level(VerboseLevel)
        ;    
            set_verbosity_level(off),
            OptionList = OptionList2
        ),
        instrument_body(File, Templates, FLT, OptionList, Module).

instrument_body(File, TemplateVar, _, _, Module) :-
        var(TemplateVar),
        !,
        % instantiation fault
        error(4, instrument(File, TemplateVar, Module)).
instrument_body(File, DefModule:PredName/2, FileLocalTemplate, OptionList, Module) :-
        !,
        % Predicate to call to return template
        % Note Defmodule could be 'every_module'
        store_itemplate(File, DefModule, 
                        FileLocalTemplate, DefModule:idynamic, PredName),
        instrument_file(File, Module, compile, OptionList).
instrument_body(File, PredName/2, FileLocalTemplate, OptionList, Module) :-
        !,
        % Predicate to call to return template
        store_itemplate(File, Module, 
                        FileLocalTemplate, Module:idynamic, PredName),
        instrument_file(File, Module, compile, OptionList).
instrument_body(File, Templates, FileLocalTemplate, OptionList0, Module) :-
        (Templates = []; Templates = [_,_]),
        !,
        parse_templates(File, Templates, FileLocalTemplate, Module),
        store_scc_excluded_templates(File, FileLocalTemplate, 
                                     OptionList0, OptionList, Module),
        instrument_file(File, Module, compile, OptionList).
instrument_body(File, TemplateSpec, FileLocalTemplate, OptionList0, Module) :-
        % All we can do is try and match as 'F/A = Template'
        % parse_template will throw the type error!
        parse_template(File, TemplateSpec, FileLocalTemplate, Module),
        store_scc_excluded_templates(File, FileLocalTemplate, 
                                     OptionList0, OptionList, Module),
        instrument_file(File, Module, compile, OptionList).


% ----------------------------------------------------------------------
% Top-level result predicates and friends
% ----------------------------------------------------------------------

:- comment(module_result/0, [
        summary:"Pretty-print all files in a module, including any 
    instrumentation results",
        args:[],
        see_also:[instrument/2, instrument/3, file_result/1, file_result/2, 
                  library(instrument), library(pretty_printer), module_result/1,
                  struct(itemplate), defined_modules/2],
        desc:html("<P>
   This will pretty-print all source files in the calling module context, 
   annotated with the result of instrumentation.  The resulting .html 
   files are placed in a sub-directory called instrument', relative to 
   the files that were compiled into the file.
   </P><P>
   See <TT>module_result/1</TT> for options to modify the output.
   </P><P>
   The use of this predicate is only meaningful if the module has 
   previously been instrumented and compiled 
   using <TT>instrument:instrument/1,2</TT>, and the code has been 
   run in order to obtain instrumentation results.
   </P><P>
   See <TT>struct(itemplate)</TT> for documentation of how the 
   result instrumentation is used in the annotation and pretty-printing 
   of the code.
   </P><P>
   The predicate is a tool and the instrumentation results of a 
   module other than the current calling module context can be obtained 
   by invoking <TT>module_result@Module</TT>.
   <P>")
         ]).

:- tool(module_result/0, module_result_body/1).

module_result_body(Module) :-
        module_result_body([], Module).


:- comment(module_result/1, [
        summary:"Pretty-print all files in a module, including any 
   instrumentation results",
        amode:module_result(+),
        args:["OptionList" : "List of options"],
        see_also:[instrument/2, instrument/3, file_result/1, file_result/2, 
                  library(instrument), library(pretty_printer), module_result/0,
                  struct(itemplate), defined_modules/2],
        desc:html("<P>
   This will pretty-print all source files in the calling
   module context, annotated with the result of instrumentation. 
   The resulting .html files are placed in a sub-directory called 
   instrument', relative to the files that were compiled into the file.
   </P><P>
   OptionList is a list of options identical to the one accepted by 
   <TT>pretty_print/2</TT> in the library(pretty_printer), and can be 
   used to modify the output style and the location of the output file.
   </P><P>
   Additionally, OptionList may contain the following options:
   <DL><DT>
   <TT>ignore_module_templates</TT> (default:<TT>off</TT>)
   <DD>
   Specifying this option as <TT>on</TT> results in the 
   use of file local templates only during instrumentation. 
   Templates are not sought from the global store.
   <P>
   This option should only be specified if it was also specified 
   during instrumentation (i.e. when using 
   <TT>instrument:instrument/3</TT>).
   <DT>
   <TT>macro_expansion</TT> (default:<TT>off</TT>)
   <DD>
   This option affects only the printing of the result (html) file. 
   By default, read-macros are not expanded in this output. 
   In rare cases, where macro expansion would affect the placement of 
   instrumentation positions, it may be necessary to set 
   this option to <TT>on</TT> in order to display the instrumentation 
   at the correct positions in the code.
   <P>
   <DT>
   <TT>verbose</TT> (default:<TT>off</TT>)
   <DD>
   If set to <TT>on</TT>, the instrument preprocessor 
   will print predicate names as they are processed (to 
   log_output). 
   If set to <TT>debug</TT>, the instrument preprocessor 
   prints each instrumented and/or woven predicate definition (to 
   log_output).
   </DL>
   </P><P>
   The use of this predicate is only meaningful if the module has 
   previously been instrumented and compiled 
   using <TT>instrument:instrument/1,2</TT>, and the code has been 
   run in order to obtain instrumentation results.
   </P><P>
   See <TT>struct(itemplate)</TT> for documentation of how the 
   result instrumentation is used in the annotation and pretty-printing 
   of the code.
   </P><P>
   The predicate is a tool and the instrumentation results of a 
   module other than the current calling module context can be obtained 
   by invoking <TT>module_result(OptionList)@Module</TT>.
   <P>")
         ]).

:- tool(module_result/1, module_result_body/2).

module_result_body(OptionList, Module) :-
        ( var(OptionList) -> OptionList = [] ; true),
        stored_keys(insertion_point_values, ModuleFileKeys),
        % All files in Module
        ( foreach(ModuleFileKey, ModuleFileKeys) ,
          param(OptionList, Module)
        do
            ( ModuleFileKey = Module:CanonicalFile ->
                instrument_file(CanonicalFile, Module, print, OptionList)
            ;
                true
            )
        ).


:- comment(file_result/1, [
        summary:"Pretty-print a file, including any instrumentation results",
        amode:file_result(+),
        args:["File" : "Atom or string"],
        see_also:[instrument/2, instrument/3, file_result/2, library(instrument), 
                  library(pretty_printer), module_result/0, module_result/1,
                  struct(itemplate), defined_modules/2],
        desc:html("<P>
   This will pretty-print the specified source file in the calling 
   module context, annotated with the result of instrumentation. 
   The resulting .html file is placed in a sub-directory called 
   instrument', relative to File.
   </P><P>
   See <TT>file_result/2</TT> for options to modify the output.
   </P><P>
   The use of this predicate is only meaningful if the module has 
   previously been instrumented and compiled 
   using <TT>instrument:instrument/1,2</TT>, and the code has been 
   run in order to obtain instrumentation results.
   </P><P>
   See <TT>struct(itemplate)</TT> for documentation of how the 
   result instrumentation is used in the annotation and pretty-printing 
   of the code.
   </P><P>
   The predicate is a tool and the instrumentation results of a 
   file in an alternate module to the current calling module context 
   can be obtained invoking <TT>file_result(File)@Module</TT>.
   <P>")
         ]).

:- tool(file_result/1, file_result_body/2).

file_result_body(File, Module) :-
        file_result_body(File, [], Module).


:- comment(file_result/2, [
        summary:"Pretty-print a file, including any instrumentation results",
        amode:file_result(+, +),
        args:["File" : "Atom or string", "OptionList" : "List of options"],
        see_also:[instrument/2, instrument/3, file_result/1, library(instrument), 
                  library(pretty_printer), module_result/0, module_result/1,
                  struct(itemplate), defined_modules/2],
        desc:html("<P>
   This will pretty-print the specified source file in the calling 
   module context, annotated with the result of instrumentation. 
   The resulting .html file is placed in a sub-directory called 
   instrument', relative to File.
   </P><P>
   The use of this predicate is only meaningful if the module has 
   previously been instrumented and compiled 
   using <TT>instrument:instrument/1,2</TT>, and the code has been 
   run in order to obtain instrumentation results.
   </P><P>
   OptionList is a list of options identical to the one accepted by 
   <TT>pretty_print/2</TT> in the library(pretty_printer), and can be 
   used to modify the output style and the location of the output file.
   </P><P>
   Additionally, OptionList may contain the following options:
   <DL><DT>
   <TT>ignore_module_templates</TT> (default:<TT>off</TT>)
   <DD>
   Specifying this option as <TT>on</TT> results in the 
   use of file local templates only during instrumentation. 
   Templates are not sought from the global store.
   <P>
   This option should only be specified if it was also specified 
   during instrumentation (i.e. when using 
   <TT>instrument:instrument/3</TT>).
   </P><DT>
   <TT>macro_expansion</TT> (default:<TT>off</TT>)
   <DD>
   This option affects only the printing of the result (html) file. 
   By default, read-macros are not expanded in this output. 
   In rare cases, where macro expansion would affect the placement of 
   instrumentation positions, it may be necessary to set 
   this option to <TT>on</TT> in order to display the instrumentation 
   at the correct positions in the code.
   <P><DT>
   <TT>verbose</TT> (default:<TT>off</TT>)
   <DD>
   If set to <TT>on</TT>, the instrument preprocessor 
   will print predicate names as they are processed (to 
   log_output). 
   If set to <TT>debug</TT>, the instrument preprocessor 
   prints each instrumented and/or woven predicate definition (to 
   log_output).
   </DL><P>
   See <TT>struct(itemplate)</TT> for documentation of how the 
   result instrumentation is used in the annotation and pretty-printing 
   of the code.
   </P><P>
   The predicate is a tool and the instrumentation results of a 
   file in an alternate module to the current calling module context 
   can be obtained invoking <TT>module_result(File, OptionList)@Module</TT>.
   </P>")
         ]).

:- tool(file_result/2, file_result_body/3).

file_result_body(File, OptionList, Module) :-
        ( var(OptionList) -> OptionList = [] ; true),
        suffixed_canonical_path_name(File, CanonicalFile),
        instrument_file(CanonicalFile, Module, print, OptionList).


% ----------------------------------------------------------------------
% Instrumentation template validation, type checking and storage
% ----------------------------------------------------------------------

parse_templates(_, [], _, _) :- !.
parse_templates(File, [TemplateSpec | TemplateSpecs], FileLocalTemplate, Module) :-
        parse_template(File, TemplateSpec, FileLocalTemplate, Module),
        parse_templates(File, TemplateSpecs, FileLocalTemplate, Module).

parse_template(File, TemplateSpec, _FileLocalTemplate, Module) :-
        var(TemplateSpec),
        !,
        % instantiation fault
        error(4, parse_template(File, TemplateSpec, _, Module)).
parse_template(File, Global - ExcludeList, FileLocalTemplate, Module) :-
        !,
        % Parsing a global template specified as 'Template - [<excluded preds>]'
        % not 'F/A = Template'
        ( var(Global) ->
            % instantiation fault
            error(4, parse_template(File, Global - ExcludeList, _, Module))
        ;
            ( functor(Global, itemplate, _) ->
                true
            ;
                % type error
                error(5, parse_template(File, Global - ExcludeList, _, Module))
            )
        ),
        arg(exclude of itemplate, Global, TemplateExcludeList),
        append(TemplateExcludeList, ExcludeList, UnqualExcludeList),
        setarg(exclude of itemplate, Global, UnqualExcludeList),
        valid_template_format(Global, FileLocalTemplate, Module, MQualGlobal),
        MQualGlobal = itemplate with [module_scope:ModuleScope],
        store_itemplate(File, ModuleScope, 
                        FileLocalTemplate, ModuleScope:iglobal,
                        MQualGlobal).
parse_template(File, Global, FileLocalTemplate, Module) :-
        functor(Global, itemplate, _),
        !,
        % Parsing a global template specified as lone 'Template' not
        % 'F/A = Template'
        valid_template_format(Global, FileLocalTemplate, Module, MQualGlobal),
        MQualGlobal = itemplate with [module_scope:ModuleScope],
        store_itemplate(File, ModuleScope, 
                        FileLocalTemplate, ModuleScope:iglobal,
                        MQualGlobal).
parse_template(File, PredSpec = Template, FileLocalTemplate, Module) :-
        !,
        valid_template_format(Template, 
                              FileLocalTemplate, Module, MQualTemplate),
        MQualTemplate = itemplate with [module_scope:ModuleScope],
        % Allow 'iglobal = Template' to specify a global template
        ( PredSpec == iglobal ->
            MQualPredSpec = ModuleScope:iglobal
        ;
            valid_predicate_format(PredSpec, ModuleScope, MQualPredSpec)
        ),
        store_itemplate(File, ModuleScope, 
                        FileLocalTemplate, MQualPredSpec, MQualTemplate).
parse_template(File, TemplateSpec, _FileLocalTemplate, Module) :-
        % type error
        error(5, parse_template(File, TemplateSpec, _, Module)).


valid_predicate_format(PredSpec, Module, _MQualPredSpec) :-
        var(PredSpec),
        !,
        % instantiation fault
        error(4, valid_predicate_format(PredSpec, Module, _)).
valid_predicate_format(PredSpec, Module, MQualPredSpec) :-
        valid_predicate_or_var(PredSpec, 1.0Inf, Module, MQualPredSpec).


% Don't need a var check and error(4,...) see valid_template_format/4
parse_meta_argument_templates([], _Module, []) :- !.
parse_meta_argument_templates([Template | Templates], 
                              Module, [MQualTemplate | MQualTemplates]) :-
        !,
        % A free variable indicates that no template is applicable 
        % to the argument position
        ( compound(Template) ->
            valid_template_format(Template, _, Module, MQualTemplate)
        ;
            true
        ),
        parse_meta_argument_templates(Templates, Module, MQualTemplates).
parse_meta_argument_templates(TemplateList, Module, _) :-
        % type error
        error(5, parse_meta_argument_templates(TemplateList, Module, _)).


valid_predicate_or_inherit_or_var(inherit, _, _, Inherit) ?- 
        !,
        Inherit = inherit.
valid_predicate_or_inherit_or_var(PredSpec, MaxArity, Module, MQualPredSpec) :-
        valid_predicate_or_var(PredSpec, MaxArity, Module, MQualPredSpec).


valid_predicate_or_var(Var, _, _, _) :-
        var(Var), 
        !.
valid_predicate_or_var(PredName/Arity, MaxArity, Module, Module:PredName/Arity) :-
        !,
        % Module is validated since came from calling context tool
        ( (var(PredName); var(Arity)) ->
            % instantiation fault 
            error(4, valid_predicate_or_var(PredName/Arity, _, Module, _))
        ;
            ( (not (atom(PredName), integer(Arity))) ->
                error(5, valid_predicate_or_var(PredName/Arity, _, Module, _))
            ;
                ( Arity > MaxArity ->
                    error(5, valid_predicate_or_var(Module:PredName/ Arity, 
                                                    MaxArity, Module, _))
                ;
                    true
                )
            )
        ).
valid_predicate_or_var(DefModule:PredName/Arity, MaxArity,
                       Module, DefModule:PredName/Arity) :-
        !,
        ( (var(PredName); var(Arity); var(DefModule)) ->
            % instantiation fault 
            error(4, valid_predicate_or_var(DefModule:PredName/Arity, _, Module, _)) 
        ;
            ( (not (atom(DefModule), atom(PredName), integer(Arity))) ->
                error(5, valid_predicate_or_var(DefModule:PredName/Arity, _, Module, _))
            ;
                ( Arity > MaxArity ->
                    error(5, valid_predicate_or_var(DefModule:PredName/ Arity, 
                                                    MaxArity, Module, _))
                ;
                    true
                )
            )
        ).
valid_predicate_or_var(Error, _, Module, _) :-
        % type error
        error(5, valid_predicate_or_var(Error, _, Module, _)).


valid_predicate_list(PredList, Module, _) :-
        var(PredList),
        !,
        % instantiation fault
        error(4, valid_predicate_list(PredList, Module, _)).
valid_predicate_list([], _, []) :- !.
valid_predicate_list([Predicate | Predicates], 
                     Module, [MQualPredicate | MQualPredicates]) :-
        !,
        valid_predicate_format(Predicate, Module, MQualPredicate),
        valid_predicate_list(Predicates, Module, MQualPredicates).
valid_predicate_list(Error, Module, _) :-
        % type error
        error(5, valid_predicate_list(Error, Module, _)).


valid_template_format(Template, _FileLocalTemplate, Module, _MTemplate) :-
        var(Template),
        !,
        % instantiation fault
        error(4, valid_template_format(Template, _, Module, _)).
valid_template_format(itemplate(CLsp, CLep, CLfp, CLrp, Bsp, Bep, Bfp, Brp, 
                                SGsp, SGep, SGfp, SGrp, CAsp, CAep, CAfp,
                                CArp, Fp, Ip, Rp, MAs, EL, CWp, Aopt, 
                                MSopt, FLopt, GEopt), 
                      FileLocalTemplate, Module,
                      itemplate(MCLsp, MCLep, MCLfp, MCLrp, MBsp, MBep,
                                MBfp, MBrp, MSGsp, MSGep, MSGfp, MSGrp, 
                                MCAsp, MCAep, MCAfp, MCArp, MFp, MIp, MRp, 
                                MMAs, MEL, MCWp, Aopt, MMSopt, MFLopt, GEopt)) :-
        ( (var(MSopt); MSopt == current_module) ->
            % Default is current module scope
            MMSopt = Module,
            ScopeModule = Module
        ;
            atom(MSopt),
            % non-atom will fail and produce type error
            % Specific module or 'every_module'
            MMSopt = MSopt,
            ScopeModule = MSopt
        ),
        !,
        valid_predicate_or_inherit_or_var(CLsp, 2, ScopeModule, MCLsp),
        valid_predicate_or_inherit_or_var(CLep, 2, ScopeModule, MCLep),
        valid_predicate_or_inherit_or_var(CLfp, 2, ScopeModule, MCLfp),
        valid_predicate_or_inherit_or_var(CLrp, 2, ScopeModule, MCLrp),
        valid_predicate_or_inherit_or_var(Bsp, 2, ScopeModule, MBsp),
        valid_predicate_or_inherit_or_var(Bep, 2, ScopeModule, MBep),
        valid_predicate_or_inherit_or_var(Bfp, 2, ScopeModule, MBfp),
        valid_predicate_or_inherit_or_var(Brp, 2, ScopeModule, MBrp),
        valid_predicate_or_inherit_or_var(SGsp, 2, ScopeModule, MSGsp),
        valid_predicate_or_inherit_or_var(SGep, 2, ScopeModule, MSGep),
        valid_predicate_or_inherit_or_var(SGfp, 2, ScopeModule, MSGfp),
        valid_predicate_or_inherit_or_var(SGrp, 2, ScopeModule, MSGrp),
        valid_predicate_or_inherit_or_var(CAsp, 2, ScopeModule, MCAsp),
        valid_predicate_or_inherit_or_var(CAep, 2, ScopeModule, MCAep),
        valid_predicate_or_inherit_or_var(CAfp, 2, ScopeModule, MCAfp),
        valid_predicate_or_inherit_or_var(CArp, 2, ScopeModule, MCArp),
        valid_predicate_or_inherit_or_var(Fp, 1, ScopeModule, MFp),
        valid_predicate_or_inherit_or_var(Ip, 1, ScopeModule, MIp),
        valid_predicate_or_inherit_or_var(Rp, 5, ScopeModule, MRp),
        valid_predicate_or_inherit_or_var(CWp, 6, ScopeModule, MCWp),
        ( var(EL) -> 
            MEL = []
        ;
            valid_predicate_list(EL, ScopeModule, MEL)
        ),
        valid_assert_option(Aopt),
        ( var(FileLocalTemplate) ->
            valid_on_off_option(FLopt),
            MFLopt = FLopt
        ;
            MFLopt = on
        ),
        valid_on_off_inherit_option(GEopt),
        ( (var(MAs); MAs = []) ->
            % MMAs is to be a var
            true
        ;
            % ToDo Should meta module_scope be inherited?
            % It is not currently
            parse_meta_argument_templates(MAs, Module, MMAs)
        ).
valid_template_format(Error, _, Module, _) :-
        % type error
        error(5, valid_template_format(Error, _, Module, _)).


valid_assert_option(post_compile) ?- !.
valid_assert_option(site) ?- !.
valid_assert_option(global) ?- !.
valid_assert_option(Option) :-
        valid_on_off_inherit_option(Option).


valid_verbosity_option(debug) ?- !.
valid_verbosity_option(Option) :-
        valid_on_off_option(Option).

set_verbosity_level(Level) :-
        setval(verboseLevel, Level).

get_verbosity_level(Level) :-
        getval(verboseLevel, Level).


valid_on_off_inherit_option(inherit)  ?- !.
valid_on_off_inherit_option(Option) :-
        valid_on_off_option(Option).


valid_on_off_option(Option) :-
        var(Option),
        !.
valid_on_off_option(on)  :- !.
valid_on_off_option(off) :- !.
valid_on_off_option(Error) :-
        % type error
        error(5, valid_on_off_option(Error)).


% ----------------------------------------------------------------------
% Instrumentation template storage and retrieval
% ----------------------------------------------------------------------

store_itemplate(File, Module, 
                FileLocalTemplate, MQualPredSpec, Template) :-
        Template = itemplate with [file_local:FL],
        ( FL == on ->
            ( var(FileLocalTemplate) -> 
                suffixed_canonical_path_name(File, FileLocalTemplate)
            ;
                true
            ),
            ( store_get(file_local_templates, 
                        Module:FileLocalTemplate, Store) ->
                store_set(Store, MQualPredSpec, Template)
            ;
                store_create(Store),
                add_default_itemplates(Store, on),
                store_set(Store, MQualPredSpec, Template),
                store_set(file_local_templates, 
                          Module:FileLocalTemplate, Store)
            )
        ;
            % var(FileLocalTemplate) holds from validation
            % by valid_template_format/4
            store_set(itemplates, MQualPredSpec, Template)
        ).


% The PredSpec is assumed to be module qualified from this point on,
retrieve_predicate_itemplate(File, Module, DefModule:PredSpec, 
                             IgnoreModuleTemplates, Template) :-
        % Is there a predicate that returns the appropriate template for
        % such a goal within *this* module not (necessarily) the defining module
        retrieve_predicate_itemplate_aux(File, Module, Module:idynamic, 
                                         IgnoreModuleTemplates, PredName),
        !,
        DynamicPredicate =.. [PredName, DefModule:PredSpec, RetrievedTemplate],
        Module:DynamicPredicate,
        valid_template_format(RetrievedTemplate, File, Module, ValidatedTemplate),
        resolve_template_fields(ValidatedTemplate, File, Module, Module,
                                IgnoreModuleTemplates, Template).
retrieve_predicate_itemplate(File, Module, DefModule:PredSpec, 
                             IgnoreModuleTemplates, Template) :-
        % Is there a specific template for the predicate? 
        retrieve_predicate_itemplate_aux(File, Module, DefModule:PredSpec, 
                                         IgnoreModuleTemplates, RetrievedTemplate),
        !,
        resolve_template_fields(RetrievedTemplate, File, Module, DefModule,
                                IgnoreModuleTemplates, Template).
retrieve_predicate_itemplate(File, Module, DefModule:PredSpec, 
                             IgnoreModuleTemplates, Template) :-
        % Is there a global template from which the predicate is not
        % excluded
        retrieve_predicate_itemplate_aux(File, Module, DefModule:iglobal, 
                                         IgnoreModuleTemplates, 
                                         RetrievedTemplate),
        RetrievedTemplate = itemplate with [exclude:ExcludeList],
        not memberchk(every_module:PredSpec, ExcludeList),
        not memberchk(DefModule:PredSpec, ExcludeList),
        resolve_template_module_scope(RetrievedTemplate, Module, Template).


retrieve_predicate_itemplate_aux(File, Module, DefModule:PredSpec,
                                 IgnoreModuleTemplates, Template) :-
        ( retrieve_itemplate(File, Module, DefModule:PredSpec, 
                             IgnoreModuleTemplates, Template)
        ;
          retrieve_itemplate(File, Module, every_module:PredSpec, 
                             IgnoreModuleTemplates, Template)
        ),
        !.


% When looking for the store from which to retrieve a template:
%      If IgnoreModuleTemplates is a free variable then
%      look for the template first in the file store
%      and then in the module store.
%      Otherwise look for the template in just the file store.
%      When looking in each store, look first for a match in the 
%      specified module scope and then in the generic 'every_module'
%      scope.
retrieve_itemplate(File, Module, Key, IgnoreModuleTemplates, Template) :-
        IgnoreModuleTemplates == on,
        !,
        ( store_get(file_local_templates, Module:File, Store) ->
            retrieve_itemplate_from_store(Store, Module, Key, Template)
        ;
            store_get(file_local_templates, every_module:File, Store),
            retrieve_itemplate_from_store(Store, Module, Key, Template)
        ).
retrieve_itemplate(File, Module, Key, _IgnoreModuleTemplates, Template) :-
        ( store_get(file_local_templates, Module:File, Store) ->
            ( retrieve_itemplate_from_store(Store, Module, Key, Template) ->
                true
            ;
                retrieve_itemplate_aux(File, Module, Key, Template)
            )
        ;
            retrieve_itemplate_aux(File, Module, Key, Template)
        ).


retrieve_itemplate_aux(File, Module, Key, Template) :-
        ( store_get(file_local_templates, every_module:File, Store) ->
            ( retrieve_itemplate_from_store(Store, Module, Key, Template) ->
                true
            ;
                retrieve_itemplate_from_store(itemplates, Module, Key, Template)
            )
        ;
            retrieve_itemplate_from_store(itemplates, Module, Key, Template)
        ).


retrieve_itemplate_from_store(Store, Module, DefModule:PredSpec, Template) :-
        (PredSpec == iglobal ; PredSpec == idynamic; DefModule == every_module),
        !,
        store_get(Store, DefModule:PredSpec, Template),
        validate_module_scope(Template, Module).
retrieve_itemplate_from_store(Store, Module, DefModule:PredSpec, Template) :-
        store_get(Store, DefModule:PredSpec, Template),
        validate_module_scope(Template, Module),
        !.
retrieve_itemplate_from_store(Store, Module, DefModule:PredSpec, Template) :-
        get_flag(PredSpec, definition_module, BaseModule)@DefModule,
        findall(CurrentModule, current_module(CurrentModule), CurrentModules),
        stored_keys(Store, Keys),
        findall(M, (memberchk(M:PredSpec, Keys), 
                    memberchk(M,CurrentModules)), PosModules),
        ( fromto(PosModules, [PosModule|PosModulesTail], NextLoopValue, []),
          param(PredSpec, BaseModule, Store, Template, Module)
        do
            ( get_flag(PredSpec, definition_module, BaseModule)@PosModule,
              store_get(Store, PosModule:PredSpec, Template),
              validate_module_scope(Template, Module) ->
                NextLoopValue = []
            ;
                NextLoopValue = PosModulesTail
            )
        ),
        compound(Template).


validate_module_scope(itemplate with [module_scope:Module], Module) :- !.
validate_module_scope(itemplate with [module_scope:every_module], _Module) :- !.


resolve_template_fields(RetrievedTemplate, File, Module, 
                        DefModule, IgnoreModuleTemplates, Template) :-
        functor(RetrievedTemplate, Functor, Arity),
        functor(Template, Functor, Arity),
        ( for(I, 1, Arity), 
          % 'GlobalTemplate is anonymised to supress singleton warning!
          param(RetrievedTemplate, File, Module, DefModule, 
                IgnoreModuleTemplates, Template, _GlobalTemplate) do
            arg(I, RetrievedTemplate, Arg),
            ( nonvar(Arg) ->
                ( Arg = every_module:PredSpec ->
                    arg(I, Template, Module:PredSpec)
                ;
                    ( Arg = inherit ->
                        % First identification of an inherited
                        % field forces the global template, from
                        % which inheritance is assumed.
                        ( var(_GlobalTemplate) ->
                            retrieve_predicate_itemplate_aux(File, Module, 
                                                             DefModule:iglobal, 
                                                             IgnoreModuleTemplates, 
                                                             RGT),
                            resolve_template_module_scope(RGT, Module, _GlobalTemplate)
                        ;
                            true
                        ),
                        arg(I, _GlobalTemplate, GlobalTemplateArg),
                        arg(I, Template, GlobalTemplateArg)
                    ;
                        arg(I, Template, Arg)
                    )
                )
            ;
                true
            )
        ).


resolve_template_module_scope(RetrievedTemplate, Module, Template) :-
        functor(RetrievedTemplate, Functor, Arity),
        functor(Template, Functor, Arity),
        ( for(I, 1, Arity), param(RetrievedTemplate, Template, Module) do
            arg(I, RetrievedTemplate, Arg),
            ( (nonvar(Arg), Arg = every_module:PredSpec) ->
                arg(I, Template, Module:PredSpec)
            ;
                arg(I, Template, Arg)
            )            
        ).


% ----------------------------------------------------------------------
% Template creation for exclusion of strongly connected components
% ----------------------------------------------------------------------

store_scc_excluded_templates(_, _, OptionList0, OptionList, _) :-
        ( delete(instrument_recursive:on, OptionList0, OptionList) ->
            !
        ;
            fail
        ).
store_scc_excluded_templates(File, FileLocalTemplate, 
                             OptionList0, OptionList, Module) :-
        ( delete(instrument_recursive:_, OptionList0, OptionList) ->
            true
        ;
            OptionList = OptionList0
        ),
        call_graph(File, CallGraph, [builtins:off, module_qualified:on])@Module,
        strong_components(CallGraph, StrongComponents),
        findall(CurrentModule, current_module(CurrentModule), CurrentModules),
        % Unfortunately we have to compile the module and then compile over
        % the top of it or erase as much as we can! This is so that
        % predicate template retrieval can resolve the base module a
        % predicate is defined in... and funnily enough, to do this, the
        % module must be loaded.
        compile(File)@Module,
        ( foreach([SCH|SCT], StrongComponents), 
          param(File, FileLocalTemplate, CallGraph) 
        do
            ( (length(SCT, 0), not graph_edge(CallGraph, e(SCH, SCH, _)))  ->
                true
            ;
                ( foreach(SCE, [SCH|SCT]), 
                  param(File, FileLocalTemplate, CallGraph, SCH, SCT) 
                do
                    node_to_nodename(CallGraph, SCE, Module:PredSpec),
                    ( var(Module) -> VarModule = every_module ; VarModule = Module ),
                    ( retrieve_predicate_itemplate_aux(File, VarModule, VarModule:PredSpec, _, _) ->
                        % Explicit template added by user overrides
                        % automatically generated exclusion template 
                        true
                    ;
                        % Create an exclusion template for Module:PredSpec  
                        % First, retrieve global template and merge exclusions
                        retrieve_predicate_itemplate(File, VarModule, VarModule:iglobal, _, 
                                                     itemplate with exclude:ExcludeList),
                        ( memberchk(VarModule:PredSpec, ExcludeList) ->
                            true
                        ;
                            ( foreach(ExcludedSC, [SCH|SCT]), 
                              foreach(ExcludedPredSpec, ExcludedPredSpecs),
                              param(CallGraph) do
                                node_to_nodename(CallGraph, ExcludedSC, ExcludedPredSpec)
                            ),
                            ( get_verbosity_level(debug) ->
                                printf(log_output, 
                                       "Exlcuding %w from instrumentation%n",
                                       [ExcludedPredSpecs])
                            ;
                                true
                            ),
                            append(ExcludeList, ExcludedPredSpecs, NewExcludeList),
                            construct_inherited_exclude_template(VarModule, FileLocalTemplate, 
                                                                 NewExcludeList, Template),
                            store_itemplate(File, Module, FileLocalTemplate, VarModule:PredSpec, Template)
                        )
                    )
                )
            )
        ),
        findall(NewCurrentModule, current_module(NewCurrentModule), NewCurrentModules),
        subtract(NewCurrentModules, CurrentModules, NewModules),
        ( foreach(NewModule, NewModules)
        do
            erase_module(NewModule)
        ).


construct_inherited_exclude_template(ModuleScope, FileLocalTemplate,
                                     ExcludeList, Template) :-
        ( var(FileLocalTemplate) ->
            FLopt = off
        ;
            FLopt = on
        ),
        % Clause and block instrumentation are not meaningful in recursive
        % predicates!
        Template = itemplate with [subgoal_start:inherit, subgoal_end:inherit,
                                   subgoal_fail:inherit, subgoal_redo:inherit,
                                   call_start:inherit, call_end:inherit,
                                   call_fail:inherit, call_redo:inherit, 
                                   fact:inherit, inbetween:inherit, 
                                   result:inherit, exclude:ExcludeList,
                                   asserted:inherit, module_scope:ModuleScope,
                                   file_local:FLopt, goal_expansion:inherit].


% ----------------------------------------------------------------------
% File-to-memory compiler with instrumentation preprocessor
% ----------------------------------------------------------------------
% ----------------------------------------------------------------------
% Common code, runs in 'compile' or 'print' mode
% In compile mode, OptionList0 is instrument options.
% In print mode, OptionList0 is pretty_printer options.
% ----------------------------------------------------------------------

instrument_file(File, Module, Mode, OptionList) :-
        suffixed_canonical_path_name(File, FileKey),
        ( delete(ignore_module_templates:IMT, OptionList, OptionList1) ->
            valid_on_off_option(IMT)
        ;
            IMT = off,
            OptionList1 = OptionList
        ),
        ( Mode = compile ->
            ReadOptions = [recreate_modules]
        ;
            % 'macro_expansion:off' is default
            ( delete(macro_expansion:MEopt, OptionList1, OptionList2) ->
                valid_on_off_option(MEopt),
                (MEopt == on ->
                    ReadOptions = [keep_comments]
                ;
                    ReadOptions = [keep_comments , no_macro_expansion]
                )
            ;
                OptionList2 = OptionList1,
                ReadOptions = [keep_comments]
            )
        ),
        cputime(StartTime),
        statistics(shared_heap_used, StartMem),
        error(146, File, Module),	% start of compilation event
        source_open(File, ReadOptions, SourcePos0)@Module,
        ( Mode = compile ->
            % Get the current instrumentation site count for this module
            % This will be the definition context for the file unless a
            % module directive is encountered.
            ( store_get(insertion_point_values, Module, LastUsedCallSite) ->
                StartingCallSite is LastUsedCallSite + 1
            ;
                StartingCallSite = 0
            ),
            % Set the value to the starting value, sorry, bit of a hack,
            % - we're going to retrieve it at the end when we tidy up
            % and report stats etc.
            store_set(insertion_point_values, Module, StartingCallSite)
        ;
            % Get the current instrumentation site count for this module
            % This will be the definition context for the file unless a
            % module directive is encountered.
            ( store_get(insertion_point_values, 
                        Module:FileKey, StartingCallSite:_) ->
                true
            ;
                % Warning file hasn't been instrumented
                printf(warning_output, "File '%w' has not been"
                       " instrumented in module '%w'!%n", [FileKey, Module])
            ),
            % interpret OptionList as PrettyPrintOptions
            ( memberchk(outdir:_, OptionList2) ->
                PrettyPrintOptionList = OptionList2
            ;
                PrettyPrintOptionList = [outdir:instrument | OptionList2]
            ),
            pretty_print_open(File, PrettyPrintOptionList, Listing0)
        ),
        (
            fromto(begin, _, Class, end),
            fromto(SourcePos0, SourcePos1, SourcePos2, SourcePosEnd),
            fromto(Listing0, Listing1, Listing2, Listing),
            fromto(ClauseTail, Clauses0, Clauses1, []),
            fromto(ClauseTail, ClauseTail0, ClauseTail1, []),
            fromto(StartingCallSite, CallSite0, CallSite, _),
            fromto(none, Pred0, Pred1, none),
            fromto(none, GoalExpand0, GoalExpand1, none),
            param(Mode, FileKey, IMT)
        do
            source_read(SourcePos1, SourcePos2, Class, SourceTerm),
            arg(module of source_position, SourcePos1, PosModule),
            arg(term of source_term, SourceTerm, Term),
            ( Class = clause ->
                
                extract_pred(Term, N, A),
                Pred1 = PosModule:N/A,
                % Weave the code before instrumenting. We retrieve
                % the instrumentation template in each case and don't
                % just reuse it for uber-genericity... the case where the
                % clause is completely mangled...
                weave_clause(Term, WeavedTerm, FileKey, 
                             Pred1, IMT, Mode, PosModule),
                instrument_clause(WeavedTerm, TermInst, FileKey, Pred1, 
                                  IMT, CallSite0, CallSite, Mode, 
                                  PosModule, GoalExpand1),
                ( get_verbosity_level(debug) ->
                    writeln(log_output, 
                            '\nInstrumentation of:\n'),
                    writeclause(log_output, WeavedTerm),
                    writeln(log_output, '\nYields:\n'),
                    writeclause(log_output, TermInst)
                ;
                    true
                ),
                ( Mode = compile ->
                    ( Pred1 = Pred0 ->
                        % new clause for same pred
                        ClauseTail0 = [TermInst|ClauseTail1],
                        Clauses1 = Clauses0
                    ;
                        ClauseTail0 = [],		% new pred, compile previous
                        compile_predicate(Pred0, Clauses0, GoalExpand0),
                        Clauses1 = [TermInst|ClauseTail1]
                    )
                ; % Mode = print ->
                    update_struct(source_term, [term:TermInst], SourceTerm, SourceTermInst),
                    pretty_print_term(Class, SourceTermInst, SourcePos1, Listing1, Listing2)
                )

            ; Class = comment ->		% comment, ignore
                ( Mode = compile ->
                    true
                ;
                    pretty_print_term(Class, SourceTerm, SourcePos1, Listing1, Listing2)
                ),
                Pred1 = Pred0,
                ClauseTail1 = ClauseTail0,
                CallSite = CallSite0,
                Clauses1 = Clauses0
            
            ; % other classes are taken as predicate separator
                ClauseTail0 = [],		% compile previous predicate
                Clauses1 = ClauseTail1,
                Pred1 = none,
                
                ( Mode = compile ->
                    compile_predicate(Pred0, Clauses0, GoalExpand0)
                ;
                    pretty_print_term(Class, SourceTerm, SourcePos1, Listing1, Listing2)
                ),
                
                ( Class = directive ->
                    CallSite = CallSite0,
                    call_directive(Mode, SourcePos1, Term, PosModule)
                ; Class = query ->
                    CallSite = CallSite0,
                    call_directive(Mode, SourcePos1, Term, PosModule)
                ; Class = handled_directive ->
                    arg(module of source_position, SourcePos2, PosModule2),
                    ( PosModule2 == PosModule ->
                        CallSite = CallSite0
                    ;
                              /* module-changing directive encountered */
                        end_of_module(Mode, FileKey, PosModule, CallSite0),
                        ( Mode == compile ->
                            CallSite = 0,
                            store_set(insertion_point_values, PosModule2, 0)
                        ;
                            ( store_get(insertion_point_values, 
                                        PosModule2:FileKey, CallSite:_) ->
                                true
                            ;
                                % Warning file hasn't been instrumented
                                printf(warning_output, "File '%w' has not been"
                                       " instrumented in module '%w'!%n", [FileKey, PosModule2])
                            )
                        )
                    )
                ; Class = var ->
                    CallSite = CallSite0,
                    compiler_error(4, SourcePos1, SourceTerm)
                ; Class = end ->
                    end_of_module(Mode, FileKey, PosModule, CallSite0),
                    CallSite = 0
                ;
                    CallSite = CallSite0
                )
            )
        ),
        % Enable instrument goals, disabled at compile-time
        recorded_list(post_compile_goals, Goals),
        erase_all(post_compile_goals),
        ( foreach( Goal, Goals) do
            Goal
        ),
        % Tidy up, print out statistics
        ( Mode = compile ->
            Time is cputime-StartTime,
            Size is statistics(shared_heap_used) - StartMem,
            arg(file of source_position, SourcePos0, CanonicalFile),
            concat_atom([CanonicalFile], CanonicalFileA),
            error(166, CanonicalFileA-(instrument:instrument(CanonicalFileA)), Module),
            error(139, (CanonicalFileA,Size,Time), Module)
        ;
            pretty_print_close(Listing)
        ),
        source_close(SourcePosEnd, [keep_modules]).


compile_predicate(_, [], _) :- !.
compile_predicate(Pred, Clauses, GoalExpand) :-
        % Default for goal expansion is on. If there
        % was no template, then var(GoalExpand)
        % Unifying GoalExpand = on does the right thing!
        % Verbose could be 'on' or 'debug'. Default is off
        get_verbosity_level(Verbose),
        Pred = Module:_,
        ( GoalExpand = on, get_flag(goal_expansion, on) ->
            (
                foreach(Clause,Clauses),
                foreach(ExpandedClause, ExpandedClauses),
                param(Module, Verbose)
            do
                ( Clause = (Head :- Body) ->
                    expand_goal(Body, ExpandedBody)@Module,
                    ExpandedClause = (Head :- ExpandedBody)
                ;
                    ExpandedClause = Clause
                ),
                ( Verbose == debug ->
                    writeln(log_output, 
                            '\nGoal expansion yields:'),
                    writeclause(log_output, ExpandedClause)
                ;
                    true
                )
            )
        ;
            ExpandedClauses = Clauses
        ),
    	compile_term(ExpandedClauses)@Module,
        ( Verbose == off ->
            true
        ;
            printf(log_output, "Instrumentation of '%w' complete%n", [Pred])
        ).


extract_pred((Head :- _), N, A) :-
        !,
    	functor(Head, N, A).
extract_pred((Head ?- _), N, A) :- 
        !,	% shouldn't occur
        functor(Head, N, A).
extract_pred((Head if _), N, A) :- 
        !,	% shouldn't occur
        functor(Head, N, A).
extract_pred(Fact, N, A) :-
    	functor(Fact, N, A).


call_directive(compile, source_position with [file:F,line:L], Dir, Module) :-
        arg(1, Dir, Goal),
    	block(
                 ( call(Goal)@Module ->
                     true
                 ;
                     printf(error, "Compiler: query failed in file %w, line %d.%n", [F,L])
                 ),
                 Tag,
                 printf(error, "Compiler: query exited (%w) in file %w, line %d.%n", [Tag, F,L])
             ).
call_directive(print, _, _, _).


compiler_error(N, source_position with [file:F, line:L], source_term with [term:Term]) :-
        error_id(N, Message),
        printf(error, "Compiler: %w in file %w, line %d:%n%Qw%n",
               [Message, F, L, Term]).


end_of_module(compile, CanonicalFile, Module, EndingCallSite) :- 
        !,
        % Store the  current instrumentation site count way
        store_get(insertion_point_values, Module, StartingCallSite),
        store_set(insertion_point_values, Module, EndingCallSite),
        store_set(insertion_point_values,
                  Module:CanonicalFile, StartingCallSite:EndingCallSite),
        FileCallSites is EndingCallSite - StartingCallSite,
        ( FileCallSites > 0 ->
            error(149, end_of_file, CanonicalFile), % end-of-compilation-unit event
            printf(log_output, "instrument: %d call sites instrumented in file %w%n",
                   [FileCallSites, CanonicalFile]),
            printf(log_output, 
                   "instrument: %d call sites in total instrumented in module %w%n",
                   [EndingCallSite, Module])
        ;
            true
        ).
end_of_module(print, CanonicalFile, Module, EndingCallSite) :-
        ( store_get(insertion_point_values, 
                    Module:CanonicalFile, _:PrintEndingCallSite) ->
            ( EndingCallSite =:= PrintEndingCallSite ->
                true
            ;
                printf(warning_output, 
                       "Instrumentation points out of sync - "
                       "recompile File '%w' and module '%w'!%n", 
                       [CanonicalFile, Module])
            )
        ;
            % Already reported an error
            true
        ).


% Add suffix to pathname
suffixed_canonical_path_name(File, CanonicalFile) :-
        canonical_path_name(File, File1),
        get_flag(prolog_suffix, Suffixes),
        once existing_file(File1, Suffixes, [readable], File2),
        concat_atom([File2], CanonicalFile).


% ----------------------------------------------------------------------
% Instrumentation insertion
% ----------------------------------------------------------------------

% -?-> is treated specially because nothing must be inserted before the -?->

instrument_clause((Head :- -?-> Body), NewClause, FileKey, MQualPredSpec, 
        IMT, CallSite0, CallSite, Mode, Module, GoalExpand) ?-
        !,
        ( (retrieve_predicate_itemplate(FileKey, Module, 
                                        MQualPredSpec, IMT, Template),
           Template = itemplate with [goal_expansion:GoalExpand]) ->
            insert_clause_instrumentation(BlockBody, ClauseBody, CallSite0, 
                                          CallSite1, Mode, Module, Template)
        ;
            ClauseBody = BlockBody,
            CallSite1 = CallSite0
        ),
        instrument_clause_block_body(Body, BlockBody, FileKey, IMT, CallSite1,
                                     CallSite, Mode, Module, Template),
        NewClause = (Head :- -?-> ClauseBody).
instrument_clause((Head :- Body), NewClause, FileKey, MQualPredSpec, 
        IMT, CallSite0, CallSite, Mode, Module, GoalExpand) ?-
        !,
        ( (retrieve_predicate_itemplate(FileKey, Module, 
                                        MQualPredSpec, IMT, Template),
           Template = itemplate with [goal_expansion:GoalExpand]) ->
            insert_clause_instrumentation(BlockBody, ClauseBody, CallSite0, 
                                          CallSite1, Mode, Module, Template)
        ;
            ClauseBody = BlockBody,
            CallSite1 = CallSite0
        ),
        instrument_clause_block_body(Body, BlockBody, FileKey, IMT,
                                     CallSite1, CallSite, Mode, Module, Template),
        NewClause = (Head :- ClauseBody).
instrument_clause(Fact, NewClause, FileKey, MQualPredSpec, IMT, 
                  CallSite0, CallSite, Mode, Module, GoalExpand) :-
        ( (retrieve_predicate_itemplate(FileKey, Module,
                                        MQualPredSpec, IMT, Template),
           Template = itemplate with [fact:FactPred, 
                                      asserted:Assert, result:ResultPred, 
                                      goal_expansion:GoalExpand]),
          compound(FactPred) ->
            ( Mode = compile ->
                insert_instrumentation(true, (true, FactBody), CallSite0, 
                                       CallSite, _, FactPred, _, _, Assert),
                NewClause = (Fact :- FactBody)
            ;
                ( compound(ResultPred) ->
                    insert_instrumentation_result(Fact, CallSite0, ResultPred, 
                                                  fact, Module, FactBody),
                    NewClause = FactBody,
                    CallSite is CallSite0 + 1
                ;
                    CallSite = CallSite0,
                    NewClause = Fact
                )
            )
        ;
            CallSite = CallSite0,
            NewClause = Fact
        ).


%:- mode instrument_clause_block_body(?,-,+,-,+,+, ?).
%:- mode instrument_clause_body(?,-,+,-,+,+, ?).
instrument_clause_block_body(Goal, FinalGoal, File, IMT,
                             CallSite0, CallSite, Mode, Module, Template) :- 
        insert_block_instrumentation(NewGoal, FinalGoal, CallSite0, 
                                     CallSite1, Mode, Module, Template),
        instrument_clause_body(Goal, NewGoal, File, IMT, 
                               CallSite1, CallSite, Mode, Module, Template).


instrument_clause_body(Goal, NewGoal, _File, _IMT, 
                       CallSite0, CallSite, Mode, Module, Template) :-
        var(Goal),
        !,
        % Can't exclude a var!
        insert_subgoal_instrumentation(Goal, NewGoal, CallSite0, 
                                       CallSite, Mode, Module, Template).
instrument_clause_body((G1,G2), (NewG1, NewG2), File, IMT,
                       CallSite0, CallSite, Mode, Module, Template) :- 
        !,
        instrument_clause_body(G1, NewG1, File, IMT, 
                               CallSite0, CallSite1, Mode, Module, Template),
        % Inbetween point instrumentation
        insert_inbetween_instrumentation(NextG2, NewG2, CallSite1,
                                         CallSite2, Mode, Module, Template),
        instrument_clause_body(G2, NextG2, File, IMT, 
                               CallSite2, CallSite, Mode, Module,
                               Template).
instrument_clause_body((G1->G2;G3), NewGoal, File, IMT,
                       CallSite0, CallSite, Mode, Module, Template) :- 
        !,
        insert_subgoal_instrumentation((NewG1->NewG2;NewG3), 
                                       NewGoal, CallSite0, CallSite1, 
                                       Mode, Module, Template),
        % Must not wrap '->' with subgoal instrumentation
        instrument_simple_goal((G1->G2), (NewG1->NewG2), File, IMT, CallSite1, 
                               CallSite2, Mode, Module, Template, not_subgoal),
        instrument_clause_block_body(G3, NewG3, File, IMT, 
                                     CallSite2, CallSite, Mode, Module, Template).
instrument_clause_body(Goal, NewGoal, File, IMT, 
                       CallSite0, CallSite, Mode, Module, Template) :-
        instrument_simple_goal(Goal, NewGoal, File, IMT, CallSite0, 
                               CallSite, Mode, Module, Template, subgoal).


instrument_simple_goal(DefModule:PredSig, NewGoal, _, _, 
                       CallSite0, CallSite, Mode, Module, Template, IsSubgoal) :- 
        var(PredSig),
        !,
        ( IsSubgoal = subgoal ->
            insert_subgoal_instrumentation(DefModule:PredSig, NewGoal, CallSite0, 
                                           CallSite, Mode, Module, Template)
        ;
            NewGoal = DefModule:PredSig,
            CallSite = CallSite0
        ).
instrument_simple_goal(Goal, NewGoal, File, IMT, 
                       CallSite0, CallSite, Mode, Module, Template, IsSubgoal) :-
        % Handle module qualified and unqualified predicates
        ( Goal = DefModule:PredSig ->
            ( var(DefModule) -> VarModule = every_module ; VarModule = DefModule ),
            functor(PredSig, F, N),
            MQualPredSpec = VarModule:F/N,
            UnQualGoal = PredSig,
            ProcessedGoal = DefModule:ProcessedGoal0
        ;
            functor(Goal, F, N),
            MQualPredSpec = Module:F/N,
            UnQualGoal = Goal,
            ProcessedGoal = ProcessedGoal0
        ),
        % Is the goal excluded from call or subgoal instrumentation?
        Template = itemplate with exclude:ExcludeList,
        ( memberchk(MQualPredSpec, ExcludeList) -> 
            Excluded = 1
        ;
            Excluded = 0
        ),
        % Should we insert subgoal instrumentation?
        ( (Excluded = 0, IsSubgoal = subgoal) ->
            insert_subgoal_instrumentation(Subgoal, NewGoal, CallSite0,
                                           CallSite1, Mode, Module, Template)
        ;
            NewGoal = Subgoal,
            CallSite1 = CallSite0
        ),
        % Is there a 'call' template and should call instrumentation be inserted
        ( (Excluded = 0, 
           retrieve_predicate_itemplate(File, Module, 
                                        MQualPredSpec, IMT, CallTemplate)) ->
            insert_call_instrumentation(ProcessedGoal, Subgoal, CallSite1, 
                                        CallSite2, Mode, Module, CallTemplate),
            CallTemplate = itemplate with [meta_args:MetaArguments],
            % nonvar(MetaArguments) indicates list, since CallTemplate
            % has been validated
            ( nonvar(MetaArguments) ->
                % Some of Goal's arguments may need to be preprocessed themselves
                functor(ProcessedGoal0, F, N),
                (
                    for(I,1,N),
                    foreach(MetaTemplate, MetaArguments),
                    fromto(CallSite2, CallSite3, CallSite4, CallSite),
                    param(Template, UnQualGoal, ProcessedGoal0, File, IMT, Mode, Module)
                do
                    arg(I, UnQualGoal, Arg),
                    arg(I, ProcessedGoal0, NewArg),
                    ( compound(MetaTemplate) ->
                        resolve_inherited_fields(MetaTemplate,
                                                 Template, InheritedMetaTemplate),
                        instrument_clause_block_body(Arg, NewArg, File, IMT, 
                                                     CallSite3, CallSite4, 
                                                     Mode, Module,
                                                     InheritedMetaTemplate)
                    ;
                        NewArg = Arg,
                        CallSite4 = CallSite3
                    )
                )
            ;
                % simple goal, just inserting instrumentation in front is enough
                ProcessedGoal0 = UnQualGoal,
                CallSite = CallSite2
            )
        ;
            Subgoal = Goal,
            CallSite = CallSite1
        ).


:- mode resolve_inherited_fields(+,+,-).
resolve_inherited_fields(MetaTemplate0, ParentTemplate, MetaTemplate) :-
        functor(MetaTemplate0, Functor, Arity),
        functor(MetaTemplate, Functor, Arity),
        ( for(I, 1, Arity), 
          param(MetaTemplate0, ParentTemplate, MetaTemplate)
        do
            arg(I, MetaTemplate0, Arg),
            ( Arg == inherit ->
                arg(I, ParentTemplate, InheritedArg),
                arg(I, MetaTemplate, InheritedArg)
            ;
                arg(I, MetaTemplate, Arg)
            )
        ).


insert_clause_instrumentation(Goal, NewGoal, CallSite0, 
                              CallSite, compile, _Module, Template) :-
        !,
        Template = itemplate with [clause_start:ClauseStart, clause_end:ClauseEnd, 
                                   clause_fail:ClauseFail, clause_redo:ClauseRedo, 
                                   asserted:Assert],
        insert_instrumentation(Goal, NewGoal, CallSite0, CallSite,
                               ClauseStart, ClauseEnd, ClauseFail,
                               ClauseRedo, Assert).
insert_clause_instrumentation(Goal, NewGoal, CallSite0, 
                              CallSite, print, Module, Template) :-
        Template = itemplate with [clause_start:ClauseStart, clause_end:ClauseEnd, 
                                   clause_fail:ClauseFail,  clause_redo:ClauseRedo,
                                   result:ResultPred],
        insert_scoped_instrumentation_result(Goal, NewGoal, CallSite0,
                                             CallSite, ClauseStart,
                                             ClauseEnd, ClauseFail, ClauseRedo,
                                             ResultPred, clause, Module).


insert_block_instrumentation(Goal, NewGoal, CallSite0, 
                             CallSite, compile, _Module, Template) :-
        !,
        Template = itemplate with [block_start:BlockStart, block_end:BlockEnd, 
                                   block_fail:BlockFail, block_redo:BlockRedo, 
                                   asserted:Assert],
        insert_instrumentation(Goal, NewGoal, CallSite0, CallSite,
                               BlockStart, BlockEnd, BlockFail, BlockRedo, Assert).
insert_block_instrumentation(Goal, NewGoal, CallSite0, 
                             CallSite, print, Module, Template) :-
        Template = itemplate with [block_start:BlockStart, block_end:BlockEnd, 
                                   block_fail:BlockFail, block_redo:BlockRedo, 
                                   result:ResultPred],
        insert_scoped_instrumentation_result(Goal, NewGoal, CallSite0,
                                             CallSite, BlockStart,
                                             BlockEnd, BlockFail, BlockRedo,
                                             ResultPred, block, Module).


insert_subgoal_instrumentation(Goal, NewGoal, CallSite0, 
                               CallSite, compile, _Module, Template) :-
        !,
        Template = itemplate with [subgoal_start:SubgoalStart, 
                                   subgoal_end:SubgoalEnd, 
                                   subgoal_fail:SubgoalFail, 
                                   subgoal_redo:SubgoalRedo, 
                                   asserted:Assert],
        insert_instrumentation(Goal, NewGoal, CallSite0, 
                               CallSite, SubgoalStart, SubgoalEnd, 
                               SubgoalFail, SubgoalRedo, Assert).
insert_subgoal_instrumentation(Goal, NewGoal, CallSite0, 
                               CallSite, print, Module, Template) :-
        Template = itemplate with [subgoal_start:SubgoalStart, 
                                   subgoal_end:SubgoalEnd, 
                                   subgoal_fail:SubgoalFail,
                                   subgoal_redo:SubgoalRedo,
                                   result:ResultPred],
        insert_scoped_instrumentation_result(Goal, NewGoal, CallSite0,
                                             CallSite, SubgoalStart,
                                             SubgoalEnd, SubgoalFail,
                                             SubgoalRedo, ResultPred, 
                                             subgoal, Module).


insert_call_instrumentation(Goal, NewGoal, CallSite0, 
                            CallSite, compile, _Module, Template) :-
        !,
        Template = itemplate with [call_start:CallStart, call_end:CallEnd, 
                                   call_fail:CallFail, call_redo:CallRedo,
                                   asserted:Assert],
        insert_instrumentation(Goal, NewGoal, CallSite0, CallSite,
                               CallStart, CallEnd, CallFail, CallRedo,
                               Assert).
insert_call_instrumentation(Goal, NewGoal, CallSite0, 
                            CallSite, print, Module, Template) :-
        Template = itemplate with [call_start:CallStart, call_end:CallEnd, 
                                   call_fail:CallFail, call_redo:CallRedo,
                                   result:ResultPred],
        insert_scoped_instrumentation_result(Goal, NewGoal, CallSite0,
                                             CallSite, CallStart,
                                             CallEnd, CallFail, CallRedo,
                                             ResultPred, call, Module).


insert_inbetween_instrumentation(Goal, NewGoal, CallSite0, 
                                 CallSite, compile, _Module, Template) :-
        !,
        Template = itemplate with [inbetween:BetweenPred, asserted:Assert],
        ( compound(BetweenPred) ->
            insert_instrumentation(Goal, NewGoal, CallSite0, 
                                   CallSite, BetweenPred, _, _, _, Assert)
        ;
            NewGoal = Goal,
            CallSite = CallSite0
        ).
insert_inbetween_instrumentation(Goal, NewGoal, CallSite0, 
                                 CallSite, print, Module, Template) :-
        Template = itemplate with [inbetween:BetweenPred, result:ResultPred],
        ( (compound(BetweenPred), compound(ResultPred)) ->
            insert_instrumentation_result(Goal, CallSite0, ResultPred, 
                                          inbetween, Module, NewGoal),
            CallSite is CallSite0 + 1
        ;
            NewGoal = Goal,
            CallSite = CallSite0
        ).


insert_scoped_instrumentation_result(Goal, NewGoal, CallSite0, 
                                     CallSite, StartPred, EndPred, FailPred, 
                                     RedoPred, ResultPred, Position, Module) :-
        ( compound(ResultPred) ->
            ( compound(StartPred) -> 
                concat_atoms(Position, '_start', PositionStart),
                insert_instrumentation_result(Goal, CallSite0, ResultPred, 
                                              PositionStart, Module, StartGoal),
                CallSite is CallSite0 + 1
            ;
                StartGoal = Goal
            ),
            ( compound(FailPred) -> 
                concat_atoms(Position, '_fail', PositionFail),
                insert_instrumentation_result(StartGoal, CallSite0, ResultPred, 
                                              PositionFail, Module, FailGoal),
                CallSite is CallSite0 + 1
            ;
                FailGoal = StartGoal
            ),
            ( compound(RedoPred) -> 
                concat_atoms(Position, '_redo', PositionRedo),
                insert_instrumentation_result(FailGoal, CallSite0, ResultPred, 
                                              PositionRedo, Module, RedoGoal),
                CallSite is CallSite0 + 1
            ;
                RedoGoal = FailGoal
            ),
            ( compound(EndPred) -> 
                concat_atoms(Position, '_end', PositionEnd),
                insert_instrumentation_result(RedoGoal, CallSite0, ResultPred, 
                                              PositionEnd, Module, NewGoal),
                CallSite is CallSite0 + 1
            ;
                NewGoal = RedoGoal
            )
        ;
            NewGoal = Goal
        ),
        % CallSite only bound and incremented if instrumentation was inserted
        ( var(CallSite) -> 
            CallSite = CallSite0 
        ; 
            true 
        ).


insert_instrumentation(Goal, NewGoal, CallSite0, CallSite,
                       StartPred, EndPred, FailPred, RedoPred, Assert) :- 
        var(FailPred), var(RedoPred), 
        !,
        ( compound(StartPred) -> 
            compile_instrument_predicate(Assert, StartPred, 
                                         CallSite0, AuxVar, StartPredicate),
            StartGoal = (StartPredicate, Goal),
            CallSite is CallSite0 + 1
        ;
            StartGoal = Goal
        ),
        ( compound(EndPred) -> 
            compile_instrument_predicate(Assert, EndPred, 
                                         CallSite0, AuxVar, EndPredicate),
            NewGoal = (StartGoal, EndPredicate),
            CallSite is CallSite0 + 1
        ;
            NewGoal = StartGoal
        ),
        % CallSite only bound and incremented if instrumentation was inserted
        ( var(CallSite) -> 
            CallSite = CallSite0 
        ; 
            true 
        ).
insert_instrumentation(Goal, NewGoal, CallSite0, CallSite,
                       StartPred, EndPred, FailPred, RedoPred, Assert) :- 
        !,
        % Inserting at least Fail or Redo or both instrumentation
        % Definitely inserting  some instrumentation so increment site count
        CallSite is CallSite0 + 1,
        % Definitely need a timestamp (since one of Fail or Redo)
        TimeStampCreate = (TimeStamp = f(_TS)),
        ( compound(StartPred) -> 
            compile_instrument_predicate(Assert, StartPred, CallSite0, AuxVar, StartPredicate),
            StartGoal = (StartPredicate, RedoGoal)
        ;
            StartGoal = RedoGoal
        ),
        % Always age the time stamp to stop the trail frame from being gc'd 
        % (TimeStamp would be out of scope) when inserting Fail but not Redo instrumentation.
        ( compound(EndPred) -> 
            compile_instrument_predicate(Assert, EndPred, CallSite0, AuxVar, EndPredicate),
            EndGoal = (Goal, EndPredicate, sepia_kernel:timestamp_age(TimeStamp, 1, Age))
        ;
            EndGoal = (Goal, sepia_kernel:timestamp_age(TimeStamp, 1, Age))
        ),
        ( compound(RedoPred) ->
            compile_instrument_predicate(Assert, RedoPred, CallSite0, AuxVar, RedoPredicate),
            TimeStampUpdateStartGoal = (sepia_kernel:timestamp_update(TimeStamp, 1), StartGoal),
            ( compound(FailPred) ->
                % Re-enable fail fail-heap-event
                RedoGoal = (EndGoal,
                            ( Age = current ->
                                true
                            ;
                                sepia_kernel:event_create((sepia_kernel:event_enable(FailEventHandle), 
                                                           RedoPredicate), RedoEventHandle),
                                sepia_kernel:request_fail_event(TimeStamp, TimeStamp, 1, RedoEventHandle)

                            )
                           )
            ;
                RedoGoal = (EndGoal,
                            ( Age = current ->
                                true
                            ;
                                sepia_kernel:event_create(RedoPredicate, RedoEventHandle),
                                sepia_kernel:request_fail_event(TimeStamp, TimeStamp, 1, RedoEventHandle)
                            )
                           )
            )
        ;
            RedoGoal = EndGoal,
            TimeStampUpdateStartGoal = StartGoal
        ),
        ( compound(FailPred) ->
            compile_instrument_predicate(Assert, FailPred, CallSite0, AuxVar, FailPredicate),
            NewGoal = (TimeStampCreate, sepia_kernel:timestamp_init(TimeStamp, 1),
                        sepia_kernel:event_create(FailPredicate, FailEventHandle), 
                        sepia_kernel:request_fail_event(TimeStamp, TimeStamp, 1, FailEventHandle), 
                        TimeStampUpdateStartGoal
                       )
        ;
            NewGoal = (TimeStampCreate, TimeStampUpdateStartGoal)
        ).


compile_instrument_predicate(Assert, Module:PredName/0, _Arg0, _Arg1, Predicate) :-
        var(Assert),
        !,
        Predicate = Module:PredName.
compile_instrument_predicate(Assert, Module:PredName/1, Arg0, _Arg1, Predicate) :-
        var(Assert),
        !,
        UnqualPred =.. [PredName, Arg0],
        Predicate = Module:UnqualPred.
compile_instrument_predicate(Assert, Module:PredName/2, Arg0, Arg1, Predicate) :-
        var(Assert),
        !,
        UnqualPred =.. [PredName, Arg0, Arg1],
        Predicate = Module:UnqualPred.
compile_instrument_predicate(on, Module:PredName/0, _Arg0, _Arg1, Predicate) :-
        !,
        concat_atom([Module, PredName, 0], ControlPred),
        ControlPredicate =.. [ControlPred, on],
        concat_atom([PredName, '_r'], SurrogatePredicate),
        compile_term([:- export ControlPred/1, :- export SurrogatePredicate/0,
                                                  SurrogatePredicate :- PredName, ControlPredicate])@Module,
        Predicate = Module:SurrogatePredicate.
compile_instrument_predicate(on, Module:PredName/1, Arg0, _Arg1, Predicate) :-
        !,
        concat_atom([Module, PredName, 1], ControlPred),
        ControlPredicate =.. [ControlPred, on],
        concat_atom([PredName, '_r'], PredNameR),
        ActualPredicate =.. [PredName, SigArg0],
        SurrogatePredicate =.. [PredNameR, SigArg0],
        compile_term([:- export ControlPred/1, :- export PredNameR/1, 
                                                  SurrogatePredicate :- ActualPredicate, ControlPredicate])@Module,
        SigArg0 = Arg0,
        Predicate = Module:SurrogatePredicate.
compile_instrument_predicate(on, Module:PredName/2, Arg0, Arg1, Predicate) :-
        !,
        concat_atom([Module, PredName, 2], ControlPred),
        ControlPredicate =.. [ControlPred, on],
        concat_atom([PredName, '_r'], PredNameR),
        ActualPredicate =.. [PredName, SigArg0, SigArg1],
        SurrogatePredicate =.. [PredNameR, SigArg0, SigArg1],
        compile_term([:- export ControlPred/1, :- export PredNameR/2, 
                                                  SurrogatePredicate :- ActualPredicate, ControlPredicate])@Module,
        SigArg0 = Arg0,
        SigArg1 = Arg1,
        Predicate = Module:SurrogatePredicate.
compile_instrument_predicate(off, Module:PredName/0, _Arg0, _Arg1, Predicate) :-
        !,
        concat_atom([Module, PredName, 0], ControlPred),
        ControlPredicate =.. [ControlPred, off],
        concat_atom([PredName, '_r'], SurrogatePredicate),
        compile_term([:- export ControlPred/1, :- export SurrogatePredicate/0, 
                                                  SurrogatePredicate, ControlPredicate])@Module,
        Predicate = Module:SurrogatePredicate.
compile_instrument_predicate(off, Module:PredName/1, Arg0, _Arg1, Predicate) :-
        !,
        concat_atom([Module, PredName, 1], ControlPred),
        ControlPredicate =.. [ControlPred, off],
        concat_atom([PredName, '_r'], PredNameR),
        SurrogatePredicate =.. [PredNameR, SigArg0],
        compile_term([:- export ControlPred/1, :- export PredNameR/1, 
                                                  SurrogatePredicate, ControlPredicate])@Module,
        SigArg0 = Arg0,
        Predicate = Module:SurrogatePredicate.
compile_instrument_predicate(off, Module:PredName/2, Arg0, Arg1, Predicate) :-
        !,
        concat_atom([Module, PredName, 2], ControlPred),
        ControlPredicate =.. [ControlPred, off],
        concat_atom([PredName, '_r'], PredNameR),
        SurrogatePredicate =.. [PredNameR, SigArg0, SigArg1],
        compile_term([:- export ControlPred/1, :- export PredNameR/2, 
                                                  SurrogatePredicate, ControlPredicate])@Module,
        SigArg0 = Arg0,
        SigArg1 = Arg1,
        Predicate = Module:SurrogatePredicate.
compile_instrument_predicate(post_compile, MQualPredSpec, Arg0, Arg1, Predicate) :-
        compile_instrument_predicate(off, MQualPredSpec, Arg0, Arg1, Predicate),
        recordz(post_compile_goals, instrument_control_body(on, MQualPredSpec, _)).


insert_instrumentation_result(Goal, _CallSite, DefModule:ResultPredName/0, 
                              _Position, _Module, Goal) :-
        !,
        DefModule:ResultPredName.
insert_instrumentation_result(Goal, CallSite, DefModule:ResultPredName/1, 
                              _Position, _Module, Goal) :-
        !,
        ResultPredicate =.. [ResultPredName, CallSite],
        DefModule:ResultPredicate.
insert_instrumentation_result(Goal, CallSite, DefModule:ResultPredName/2, 
                              Position, _Module, Goal) :-
        !,
        ResultPredicate =.. [ResultPredName, CallSite, Position],
        DefModule:ResultPredicate.
insert_instrumentation_result(Goal, CallSite, DefModule:ResultPredName/3, 
                              Position, Module, Goal) :-
        !,
        ResultPredicate =.. [ResultPredName, CallSite, Position, Module],
        DefModule:ResultPredicate.
insert_instrumentation_result(Goal, CallSite, DefModule:ResultPredName/4, 
                              Position, Module, Goal) :-
        !,
        ResultPredicate =.. [ResultPredName, CallSite, Position, Module, Goal],
        DefModule:ResultPredicate.
insert_instrumentation_result(Goal, CallSite, DefModule:ResultPredName/5, 
                              Position, Module, ResultGoal) :-
        !,
        ResultPredicate =.. [ResultPredName, CallSite, 
                             Position, Module, Goal, ResultGoal],
        DefModule:ResultPredicate.
insert_instrumentation_result(Goal, CallSite, ResultPredSpec, 
                              Position, Module, ResultGoal) :-
        error(5,insert_instrumentation_result(Goal, CallSite, ResultPredSpec, 
                                              Position, Module, ResultGoal)).


% ----------------------------------------------------------------------
% Code weaving instrumentation
% ----------------------------------------------------------------------

:- mode weave_clause(?, ?, +, +, ?, +, +, +).
weave_clause(Clause, WeavedClause, 
             FileKey, MQualPredSpec, IMT, Mode, Module) :-
        !,
        ( (retrieve_predicate_itemplate(FileKey, Module, 
                                        MQualPredSpec, IMT, Template),
           Template = itemplate with [code_weaver:WeaverPred], nonvar(WeaverPred)) ->
            WeaverPred = DefModule:WeaverPredName/6,
            invoke_user_weaver(DefModule:WeaverPredName, FileKey,
                               Clause, clause, UserClause, Mode, Module),
            weave_clause_aux(DefModule:WeaverPredName, 
                             FileKey, UserClause, WeavedClause, Mode, Module),
            ( get_verbosity_level(debug) ->
                writeln(log_output, 
                        '\nWeaving of:\n'),
                writeclause(log_output, Clause),
                writeln(log_output, '\nYields:\n'),
                writeclause(log_output, WeavedClause)
            ;
                true
            )
        ;
            WeavedClause = Clause
        ).
weave_clause(Clause, Clause, _, _, _, _, _, _).


:- mode weave_clause_aux(+, +, ?, ?, +, +).
weave_clause_aux(WeaverPredName, 
                 FileKey, (Head :- -?-> Body), WeavedClause, Mode, Module) ?- 
        !,
        invoke_user_weaver(WeaverPredName, 
                           FileKey, Head, head, UserHead, Mode, Module),
        invoke_user_weaver(WeaverPredName, 
                           FileKey, Body, body, UserBody, Mode, Module),
        WeavedClause = (UserHead :- -?-> WeavedBody),
        weave_body(WeaverPredName, FileKey, UserBody, WeavedBody, Mode, Module).
weave_clause_aux(WeaverPredName, 
                 FileKey, (Head :- Body), WeavedClause, Mode, Module) ?- !,
        invoke_user_weaver(WeaverPredName, 
                           FileKey, Head, head, UserHead, Mode, Module),
        invoke_user_weaver(WeaverPredName, 
                           FileKey, Body, body, UserBody, Mode, Module),
        WeavedClause = (UserHead :- WeavedBody),
        weave_body(WeaverPredName, FileKey, UserBody, WeavedBody, Mode, Module).
weave_clause_aux(WeaverPredName, FileKey, Fact, WeavedClause, Mode, Module) :-
        invoke_user_weaver(WeaverPredName, 
                           FileKey, Fact, fact, WeavedClause, Mode, Module).


:- mode weave_body(+, +, ?, ?, +, +).
weave_body(WeaverPredName, FileKey, Goal, WeavedGoal, Mode, Module) :-
        var(Goal),
        !,
        invoke_user_weaver(WeaverPredName, 
                           FileKey, Goal, variable, WeavedGoal, Mode, Module).
weave_body(WeaverPredName, FileKey, (G1 , G2), WeavedGoal, Mode, Module) :- !,
        invoke_user_weaver(WeaverPredName, FileKey, 
                           (G1 , G2), conjunction, UserGoal, Mode, Module),
        ( var(UserGoal) -> 
            WeavedGoal = UserGoal
        ;
            ( UserGoal = (UserG1 , UserG2) ->
                weave_body(WeaverPredName, FileKey, 
                           UserG1, WeavedG1, Mode, Module),
                weave_body(WeaverPredName, FileKey, 
                           UserG2, WeavedG2, Mode, Module),
                WeavedGoal = (WeavedG1, WeavedG2)
            ;
                weave_body(WeaverPredName, FileKey, 
                           UserGoal, WeavedGoal, Mode, Module)
            )
        ).
weave_body(WeaverPredName, FileKey, (G1 ; G2), WeavedGoal, Mode, Module) :- !,
        invoke_user_weaver(WeaverPredName, FileKey, 
                           (G1 ; G2), disjunction, UserGoal, Mode, Module),
        ( var(UserGoal) -> 
            WeavedGoal = UserGoal
        ;
            ( UserGoal = (UserG1 ; UserG2) ->
                weave_body(WeaverPredName, FileKey, 
                           UserG1, WeavedG1, Mode, Module),
                weave_body(WeaverPredName, FileKey, 
                           UserG2, WeavedG2, Mode, Module),
                WeavedGoal = (WeavedG1 ; WeavedG2)
            ;
                weave_body(WeaverPredName, FileKey, 
                           UserGoal, WeavedGoal, Mode, Module)
            )
        ).
weave_body(WeaverPredName, FileKey, (G1->G2;G3), WeavedGoal, Mode, Module) :- !,
        invoke_user_weaver(WeaverPredName, FileKey, (G1->G2;G3), 
                           conditional, UserGoal, Mode, Module),
        ( var(UserGoal) -> 
            WeavedGoal = UserGoal
        ;
            ( UserGoal = (UserG1->UserG2;UserG3) ->
                weave_body(WeaverPredName, FileKey, 
                           UserG1, WeavedG1, Mode, Module),
                weave_body(WeaverPredName, FileKey, 
                           UserG2, WeavedG2, Mode, Module),
                weave_body(WeaverPredName, FileKey, 
                           UserG3, WeavedG3, Mode, Module),
                WeavedGoal = (WeavedG1->WeavedG2;WeavedG3)
            ;
                weave_body(WeaverPredName, FileKey, 
                           UserGoal, WeavedGoal, Mode, Module)
            )
        ).
weave_body(WeaverPredName, FileKey, 
           (Iterator do LoopBody), WeavedGoal, Mode, Module) :- !,
        invoke_user_weaver(WeaverPredName, FileKey, 
                           (Iterator do LoopBody), do, UserGoal, Mode, Module),
        ( var(UserGoal) -> 
            WeavedGoal = UserGoal
        ;
            ( UserGoal = (UserIterator do UserLoopBody) ->
                weave_body(WeaverPredName, FileKey, 
                           UserIterator, WeavedIterator, Mode, Module),
                weave_body(WeaverPredName, FileKey, 
                           UserLoopBody, WeavedLoopBody, Mode, Module),
                WeavedGoal = (WeavedIterator do WeavedLoopBody)
            ;
                weave_body(WeaverPredName, FileKey, 
                           UserGoal, WeavedGoal, Mode, Module)
            )
        ).
weave_body(WeaverPredName, FileKey, Goal, WeavedGoal, Mode, Module) :-
        % Handle module qualified and unqualified predicates
        invoke_user_weaver(WeaverPredName, FileKey, 
                           Goal, goal, UserGoal, Mode, Module),
        ( var(UserGoal) -> 
            WeavedGoal = UserGoal
        ;
            ( UserGoal = Goal ->
                ( UserGoal = DefModule:PredSig ->
                    functor(PredSig, F, N),
                    UnQualGoal = PredSig,
                    WeavedGoal = DefModule:MetaWeavedGoal
                ;
                    functor(Goal, F, N),
                    UnQualGoal = UserGoal,
                    WeavedGoal = MetaWeavedGoal
                ),
		( get_flag(F/N, meta_predicate, Pattern)@Module ->
                    % some of Goal's arguments may need to be weaved themselves
                    functor(MetaWeavedGoal, F, N),
                    (
                        for(I, 1, N),
                        param(WeaverPredName, FileKey, 
                              UnQualGoal, Pattern, MetaWeavedGoal, Mode, Module)
                    do
                        arg(I, UnQualGoal, Arg),
                        arg(I, Pattern, ArgSpec),
                        arg(I, MetaWeavedGoal, WeavedArg),
                        Specifier =.. [meta_argument, I, ArgSpec],
                        invoke_user_weaver(WeaverPredName, FileKey, 
                                           Arg, Specifier, UserArg, Mode, Module),
                        weave_body(WeaverPredName, FileKey, 
                                   UserArg, WeavedArg, Mode, Module)
                    )
                ;
                    % simple goal
                    WeavedGoal = UserGoal
                )
            ;
                weave_body(WeaverPredName, FileKey, 
                           UserGoal, WeavedGoal, Mode, Module)
            )
        ).


:- mode invoke_user_weaver(+, +, ?, +, ?, +, +).
invoke_user_weaver(DefModule:WeaverPredName,
                   FileKey, Code, Type, WeavedCode, Mode, Module) :-
        !,
        WeaverPredicate =.. [WeaverPredName, FileKey, Code, 
                             Type, WeavedCode, Mode, Module],
        DefModule:WeaverPredicate.
invoke_user_weaver(WeaverPredName,
                   FileKey, Code, Type, WeavedCode, Mode, Module) :-
        !,
        WeaverPredicate =.. [WeaverPredName, FileKey, Code, 
                             Type, WeavedCode, Mode, Module],
        WeaverPredicate.


% ----------------------------------------------------------------------
% Instrumentation assertion and retraction
% ----------------------------------------------------------------------

:- comment(instrument_control/2,
        [ summary:"Insert or remove instrumentation predicates dynamically 
   at runtime.",
          args:["Mode" : "Atom", "InstrumentPredSpec" : "PredSpec"],
          amode:instrument_control(+, +),
          fail_if: "InstrumentPredSpec is not a predicate specification.",
          resat:no,
          exceptions:[68 : "PredSpec is an undefined procedure."],
          see_also:[instrument/2, instrument/3, library(instrument),
                    struct(itemplate)],
          desc:html("<P>
   If the <TT>asserted</TT> option of an <TT>itemplate</TT> 
   is set to one of <TT>on</TT>, <TT>off</TT> or 
   <TT>post_compile</TT> then the template's instrumentation 
   predicates may be inserted and removed dynamically at runtime.
   <P>
   When Mode is <TT>on</TT> the instrumentation predicate 
   represented is PredSpec is inserted into the code at the 
   positions specified during instrumentation. This is done 
   efficiently with negligible runtime overhead. When Mode is 
   <TT>off</TT> the instrumentation predicate is removed from 
   the instrumented code.
   <P>
   If PredSpec is module qualified, the instrumentation predicate 
   defined in the context of the module is inserted / removed. If 
   unqualified the predicate is assumed to be defined in the context 
   of the calling module scope.
   <P>
   The predicate is a tool and an unqualified instrumentation 
   predicate defined in a module other than the current 
   calling module context can be inserted / removed by invoking 
   <TT>instrument_control(Mode, InstrumentPredSpec)@Module</TT>.")
         ]).

:- tool(instrument_control/2, instrument_control_body/3).

instrument_control_body(on, DefModule:PredName/0, _Module) :-
        !,
        concat_atom([DefModule, PredName, 0], ControlPred),
        ControlPredicate =.. [ControlPred, on],
        ( DefModule:ControlPredicate ->
            true
        ;
            concat_atom([PredName, '_r'], SurrogatePredicate),
            compile_term([SurrogatePredicate :- 
        PredName, ControlPredicate])@DefModule
        ).
instrument_control_body(on, DefModule:PredName/1, _Module) :-
        !,
        concat_atom([DefModule, PredName, 1], ControlPred),
        ControlPredicate =.. [ControlPred, on],
        ( DefModule:ControlPredicate ->
            true
        ;
            concat_atom([PredName, '_r'], PredNameR),
            ActualPredicate =.. [PredName, Arg0],
            SurrogatePredicate =.. [PredNameR, Arg0],
            compile_term([SurrogatePredicate :- 
        ActualPredicate, ControlPredicate])@DefModule
        ).
instrument_control_body(on, DefModule:PredName/2, _Module) :-
        !,
        concat_atom([DefModule, PredName, 2], ControlPred),
        ControlPredicate =.. [ControlPred, on],
        ( DefModule:ControlPredicate ->
            true
        ;
            concat_atom([PredName, '_r'], PredNameR),
            ActualPredicate =.. [PredName, Arg0, Arg1],
            SurrogatePredicate =.. [PredNameR, Arg0, Arg1],
            compile_term([SurrogatePredicate :- 
        ActualPredicate, ControlPredicate])@DefModule
        ).
instrument_control_body(on, PredName/0, Module) :-
        !,
        concat_atom([Module, PredName, 0], ControlPred),
        ControlPredicate =.. [ControlPred, on],
        ( Module:ControlPredicate ->
            true
        ;
            concat_atom([PredName, '_r'], SurrogatePredicate),
            compile_term([SurrogatePredicate :- 
        PredName, ControlPredicate])@Module
        ).
instrument_control_body(on, PredName/1, Module) :-
        !,
        concat_atom([Module, PredName, 1], ControlPred),
        ControlPredicate =.. [ControlPred, on],
        ( Module:ControlPredicate ->
            true
        ;
            concat_atom([PredName, '_r'], PredNameR),
            ActualPredicate =.. [PredName, Arg0],
            SurrogatePredicate =.. [PredNameR, Arg0],
            compile_term([SurrogatePredicate :- 
        ActualPredicate, ControlPredicate])@Module
        ).
instrument_control_body(on, PredName/2, Module) :-
        !,
        concat_atom([Module, PredName, 2], ControlPred),
        ControlPredicate =.. [ControlPred, on],
        ( Module:ControlPredicate ->
            true
        ;
            concat_atom([PredName, '_r'], PredNameR),
            ActualPredicate =.. [PredName, Arg0, Arg1],
            SurrogatePredicate =.. [PredNameR, Arg0, Arg1],
            compile_term([SurrogatePredicate :- 
        ActualPredicate, ControlPredicate])@Module
        ).
instrument_control_body(off, DefModule:PredName/0, _Module) :-
        !,
        concat_atom([DefModule, PredName, 0], ControlPred),
        ControlPredicate =.. [ControlPred, off],
        ( DefModule:ControlPredicate ->
            true
        ;
            concat_atom([PredName, '_r'], SurrogatePredicate),
            compile_term([SurrogatePredicate, ControlPredicate])@DefModule
        ).
instrument_control_body(off, DefModule:PredName/1, _Module) :-
        !,
        concat_atom([DefModule, PredName, 1], ControlPred),
        ControlPredicate =.. [ControlPred, off],
        ( DefModule:ControlPredicate ->
            true
        ;
            concat_atom([PredName, '_r'], PredNameR),
            SurrogatePredicate =.. [PredNameR, _Arg0],
            compile_term([SurrogatePredicate, ControlPredicate])@DefModule
        ).
instrument_control_body(off, DefModule:PredName/2, _Module) :-
        !,
        concat_atom([DefModule, PredName, 2], ControlPred),
        ControlPredicate =.. [ControlPred, off],
        ( DefModule:ControlPredicate ->
            true
        ;
            concat_atom([PredName, '_r'], PredNameR),
            SurrogatePredicate =.. [PredNameR, _Arg0, _Arg1],
            compile_term([SurrogatePredicate, ControlPredicate])@DefModule
        ).
instrument_control_body(off, PredName/0, Module) :-
        !,
        concat_atom([Module, PredName, 0], ControlPred),
        ControlPredicate =.. [ControlPred, off],
        ( Module:ControlPredicate ->
            true
        ;
            concat_atom([PredName, '_r'], SurrogatePredicate),
            compile_term([SurrogatePredicate, ControlPredicate])@Module
        ).
instrument_control_body(off, PredName/1, Module) :-
        !,
        concat_atom([Module, PredName, 1], ControlPred),
        ControlPredicate =.. [ControlPred, off],
        ( Module:ControlPredicate ->
            true
        ;
            concat_atom([PredName, '_r'], PredNameR),
            SurrogatePredicate =.. [PredNameR, _Arg0],
            compile_term([SurrogatePredicate, ControlPredicate])@Module
        ).
instrument_control_body(off, PredName/2, Module) :-
        concat_atom([Module, PredName, 2], ControlPred),
        ControlPredicate =.. [ControlPred, off],
        ( Module:ControlPredicate ->
            true
        ;
            concat_atom([PredName, '_r'], PredNameR),
            SurrogatePredicate =.. [PredNameR, _Arg0, _Arg1],
            compile_term([SurrogatePredicate, ControlPredicate])@Module
        ).


% ----------------------------------------------------------------------
% Call site store and access predicates
% ----------------------------------------------------------------------

:- comment(module_callsites/2,
        [ summary:"Retrieve module start and end callsite identifiers.",
          args:["StartId" : "Intger", "EndId" : "Integer"],
          amode:module_callsites(?, ?),
          fail_if: "StartId or EndId are not the module's starting and 
   ending callsite identifiers.",
          resat:no,
          see_also:[get_callsite_data/2, file_callsites/3, instrument/2, 
                    instrument/3, library(instrument), set_callsite_data/2,
                    struct(itemplate), defined_modules/2],
          desc:html("<P>
   Each code instrumentation point within a module is 
   uniquely identified by its callsite identifier.
   The callsite identifier is a monotonically increasing 
   integer incrementing from the initial value of 0.
   The predicate retrieves the start and end callsite 
   identifiers.
   <P>
   The predicate is a tool and the callsite identifiers 
   of a module other than the current calling module 
   context can be retrieved by invoking 
   <TT>module_callsites(StartId, EndId)@Module</TT>.")
         ]).

:- tool(module_callsites/2, module_callsites_body/3).

module_callsites_body(0, EndId, Module) :-
        store_get(insertion_point_values, Module, EndId).


:- comment(file_callsites/3,
        [ summary:"Retrieve start and end callsite identifiers for named file.",
          args:["File" : "Atom or string",
                "StartId" : "Intger", "EndId" : "Integer"],
          amode:file_callsites(+, ?, ?),
          fail_if: "StartId or EndId are not the file's starting and 
    ending callsite identifiers.",
          resat:no,
          see_also:[get_callsite_data/2, instrument/2, instrument/3, 
                    library(instrument), module_callsites/2,
                    set_callsite_data/2, struct(itemplate), defined_modules/2],
          desc:html("<P>
   Each code instrumentation point within a file is 
   uniquely identified by its callsite identifier.
   The callsite identifier is a monotonically increasing 
   integer incrementing from the initial value of 0 for a 
   new module. 
   <P>
   As additional files are instrumented and 
   compiled into a module, the start identifier is 
   incremented from the end identifier of the previous file. 
   The predicate retrieves the start and end callsite 
   identifiers for the file.
   <P>
   The predicate is a tool and the callsite identifiers 
   of a file instrumented in a module other than the current 
   calling module context can be retrieved by invoking 
   <TT>file_callsites(File, StartId, EndId)@Module</TT>.")
         ]).

:- tool(file_callsites/3, file_callsites_body/4).

file_callsites_body(File, StartId, EndId, Module) :-
        suffixed_canonical_path_name(File, CanonicalFile),
        store_get(insertion_point_values, Module:CanonicalFile, StartId:EndId).


:- comment(set_callsite_data/2,
        [ summary:"Associate arbitrary data with an instrumentation callsite 
   in a non-logical store.",
          args:["SiteId" : "Integer", "UserData" : "Valid Prolog term"],
          amode:set_callsite_data(+, ?),
          fail_if:no,
          resat:no,
          exceptions:[4 : "SiteId is not instantiated.", 
                      5 : "SiteId is not an integer."],
          see_also:[file_callsites/3, get_callsite_data/2, instrument/2, 
                    instrument/3, library(instrument), module_callsites/2,
                    struct(itemplate), defined_modules/2],
          desc:html("<P>
   Each code instrumentation point within a file is 
   uniquely identified by its callsite identifier.
   The callsite identifier is a monotonically increasing 
   integer incrementing from the initial value of 0 for a 
   new module. 
   <P>
   The predicate associates arbitrary data (a valid Prolog term) 
   with a specified instrumentation callsite within a module or 
   file.
   <P>
   The data is stored in a non-logical store and can be 
   retrieved using <TT>instrument:get_callsite_data/2</TT>.
   <P>
   The predicate is a tool and data for a callsite 
   of a module other than the current calling module context 
   can be stored by invoking 
   <TT>set_callsite_data(SiteId, UserData)@Module</TT>.")
         ]).

:- tool(set_callsite_data/2, set_callsite_data_body/3).

set_callsite_data_body(SiteId, UserData, Module) :-
        var(SiteId),
        !,
        error(4, set_callsite_data(SiteId, UserData, Module)).
set_callsite_data_body(SiteId, UserData, Module) :-
        integer(SiteId),
        !,
        ( store_get(module_callsite_store, Module, CallSiteDataStore) ->
            store_set(CallSiteDataStore, SiteId, UserData)
        ;
            store_create(CallSiteDataStore),
            store_set(CallSiteDataStore, SiteId, UserData),
            store_set(module_callsite_store, Module, CallSiteDataStore)
        ).
set_callsite_data_body(SiteId, UserData, Module) :-
        error(5, set_callsite_data(SiteId, UserData, Module)).


:- comment(get_callsite_data/2,
        [ summary:"Retrieve data associated with an instrumentation callsite 
   from its non-logical store.",
          args:["SiteId" : "Integer", "UserData" : "Valid Prolog term"],
          amode:get_callsite_data(+, ?),
          fail_if: "Data has not been stored for callsite identifier SiteId.",
          resat:no,
          exceptions:[4 : "SiteId is not instantiated.", 
                      5 : "SiteId is not an integer."],
          see_also:[file_callsites/3, instrument/2, instrument/3, 
                    library(instrument), module_callsites/2,
                    set_callsite_data/2,  struct(itemplate),
                    defined_modules/2],
          desc:html("<P>
   Each code instrumentation point within a file is uniquely identified 
   by its callsite identifier. The callsite identifier is a monotonically 
   increasing integer incrementing from the initial value of 0 for a new 
   module. 
   <P>
   The predicate retrieves the data (a valid Prolog term) that has been 
   associated with the specified instrumentation callsite within a module 
   or file.
   <P>
   The data is stored in a non-logical store and can be stored using 
   <TT>instrument:set_callsite_data/2</TT>.
   <P>
   The predicate is a tool and data for a callsite of a module other than 
   the current calling module context can be retrieved by invoking 
   <TT>get_callsite_data(SiteId, UserData)@Module</TT>.")
         ]).

:- tool(get_callsite_data/2, get_callsite_data_body/3).

get_callsite_data_body(SiteId, UserData, Module) :-
        var(SiteId),
        !,
        error(4, get_callsite_data(SiteId, UserData, Module)).
get_callsite_data_body(SiteId, UserData, Module) :-
        integer(SiteId),
        !,
        store_get(module_callsite_store, Module, CallSiteDataStore),
        store_get(CallSiteDataStore, SiteId, UserData).
get_callsite_data_body(SiteId, UserData, Module) :-
        error(5, get_callsite_data(SiteId, UserData, Module)).


:- comment(defined_modules/2,
        [ summary:"Retrieve the modules defined by a file.",
          args:["File" : "Atom or string", "Modules" : "List of atoms"],
          amode:defined_modules(+, -),
          fail_if:no,
          resat:no,
          see_also:[get_callsite_data/2, file_callsites/3, instrument/2, 
                    instrument/3, library(instrument), set_callsite_data/2,
                    struct(itemplate), defined_modules/2],
          desc:html("<P>
   It is often necessary to know what modules a file has defined in order 
   to operate on module specific data.  This predicate can be used to return 
   the modules defined by a file which has been compiled with the instrument 
   compiler.")
         ]).

defined_modules(File, Modules) :- 
        suffixed_canonical_path_name(File, CanonicalFile),
        stored_keys(insertion_point_values, Keys),
        ( foreach(Key, Keys), fromto([], ModList, NewModList, Modules),
          param(CanonicalFile)
        do
            ( Key = Module:CanonicalFile ->
                NewModList = [Module|ModList]
            ;
                NewModList = ModList
            )
        ).
