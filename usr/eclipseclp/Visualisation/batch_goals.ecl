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
% Copyright (C) 2001 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Josh Singer, Parc Technologies
% 
% END LICENSE BLOCK

:- module(batch_goals).

:- comment(summary, 
           "Module to support the batched execution of several goals.").

:- comment(author, "Josh Singer").

:- comment(desc, html("Module to support the batched execution of several goals independently within a single call to ECLiPSe. The advantage of this is when there is a large cost to set up the execution of an ECLiPSe goal e.g. when a remote peer executes an rpc over a network.<p> The exported predicate <code>execute_batch</code> allows the execution of goals in this way.")).

:- export execute_batch/2.
:- comment(execute_batch/2, 
           [amode : execute_batch(+, -), 
            args : ["BatchGoal" : 
		   "A list, compound term or string: the batch goal.", 
		   "BatchGoalResults" : 
		   "Instantiated to a list, atom or term representing the results of executing the the batch goal."],
            summary : "Execute a batch goal.",
            eg: "
[eclipse 1]: lib(batch_goals).
batch_goals.eco loaded traceable 0 bytes in 0.01 seconds

Yes (0.01s cpu)
[eclipse 2]: execute_batch([writeln(\"hello\"), \"X = 12\", [\"X = 60\", fail]], BatchGoalResults).
hello

BatchGoalResults = [writeln(\"hello\"), 12 = 12, [60 = 60, execute_batch_fail]]
Yes (0.00s cpu)
[eclipse 3]: execute_batch(\"             writeln(hello)\", BatchGoalResults).
hello

BatchGoalResults = writeln(hello)
Yes (0.00s cpu)
[eclipse 4]: execute_batch(4 is X, BatchGoalResults).
instantiation fault in 4 is X

X = X
BatchGoalResults = execute_batch_throw
Yes (0.00s cpu)
[eclipse 5]: 

",
            desc : html("Batch goal may be a non-list compound term or a string. In the string case it is converted to a term and <code>execute_batch</code> is re-executed on the result. If it is a non-list compound term, <i>BatchGoal</i> is considered a goal to be executed, and we attempt to call this goal.<p>If this execution is successful, <i>BatchGoalResults</i> is instantiated to <i>BatchGoal</i> with the appropriate substitutions applied. If the goal is unsuccessful due to failure, <i>BatchGoalResults</i> is instantiated to the atom <code>'execute_batch_fail'</code>. If the goal is unsuccessful due to an exception, <i>BatchGoalResults</i> is instantiated to the atom <code>'execute_batch_throw'</code>.<p> If <i>BatchGoal</i> is a list, <code>execute_batch/2</code> is executed recursively on each of its elements, with the result corresponding to each member appearing at the corresponding position in the the list <i>BatchGoalResults</i>."), 
            resat: yes]).

execute_batch(BatchGoals, BatchResults):-
	length(BatchGoals, _), % test for list
	!,
	(foreach(BatchGoal, BatchGoals), 
	 foreach(BatchResult, BatchResults)
	do
	    execute_batch(BatchGoal, BatchResult)).

execute_batch(BatchGoalString, BatchResult):-
	string(BatchGoalString), 
	!,
	term_string(BatchGoalTerm, BatchGoalString), 
	execute_batch(BatchGoalTerm, BatchResult).

execute_batch(BatchGoal, BatchResult):-
	block(call(BatchGoal), 
	      _, 
	      BatchResult = execute_batch_throw),
	!,
	(var(BatchResult) -> 
	     BatchResult = BatchGoal ; true).

execute_batch(_, execute_batch_fail).
