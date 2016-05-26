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
% The Original Code is  CPViz Constraint Visualization System
% The Initial Developer of the Original Code is  Helmut Simonis
% Portions created by the Initial Developer are
% Copyright (C) 2009-2010 Helmut Simonis
% 
% Contributor(s): 	Helmut Simonis, 4C, Univerity College Cork, Cork
%			
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
:-module(top).

:-export(top/0).

:- lib(ic).
:- lib(branch_and_bound).
:- lib(cpviz).

:- local struct(task(name,start,duration,need,use)).

top :-
	Tasks = [
	    L, T1, T2, T3, T4, T5, M1, M2, M3, M4, M5, M6,
	    S1, S2, S3, S4, S5, S6, A1, A2, A3, A4, A5, A6, P1, P2,
	    B1, B2, B3, B4, B5, B6, V1, V2,
	    AB1, AB2, AB3, AB4, AB5, AB6, UA, UE, PA, PE
	],
	PA = task{name:pa,duration : 0, need : []},
	A1 = task{name:a1,duration : 4, need : [PA], use : excavator},
	A2 = task{name:a2,duration : 2, need : [PA], use : excavator},
	A3 = task{name:a3,duration : 2, need : [PA], use : excavator},
	A4 = task{name:a4,duration : 2, need : [PA], use : excavator},
	A5 = task{name:a5,duration : 2, need : [PA], use : excavator},
	A6 = task{name:a6,duration : 5, need : [PA], use : excavator},
	P1 = task{name:p1,duration : 20, need : [A3], use : pile-driver},
	P2 = task{name:p2,duration : 13, need : [A4], use : pile-driver},
	UE = task{name:ue,duration : 10, need : [PA]},
	StartOfF = task{name:start_of_f,duration : 0, need : []},
	S1 = task{name:s1,duration : 8, need : [A1,StartOfF], use : carpentry},
	S2 = task{name:s2,duration : 4, need : [A2,StartOfF], use : carpentry},
	S3 = task{name:s3,duration : 4, need : [P1,StartOfF], use : carpentry},
	S4 = task{name:s4,duration : 4, need : [P2,StartOfF], use : carpentry},
	S5 = task{name:s5,duration : 4, need : [A5,StartOfF], use : carpentry},
	S6 = task{name:s6,duration : 10, need : [A6,StartOfF], use : carpentry},
	B1 = task{name:b1,duration : 1, need : [S1], use : concrete-mixer},
	B2 = task{name:b2,duration : 1, need : [S2], use : concrete-mixer},
	B3 = task{name:b3,duration : 1, need : [S3], use : concrete-mixer},
	B4 = task{name:b4,duration : 1, need : [S4], use : concrete-mixer},
	B5 = task{name:b5,duration : 1, need : [S5], use : concrete-mixer},
	B6 = task{name:b6,duration : 1, need : [S6], use : concrete-mixer},
	AB1 = task{name:ab1,duration : 1, need : [B1]},
	AB2 = task{name:ab2,duration : 1, need : [B2]},
	AB3 = task{name:ab3,duration : 1, need : [B3]},
	AB4 = task{name:ab4,duration : 1, need : [B4]},
	AB5 = task{name:ab5,duration : 1, need : [B5]},
	AB6 = task{name:ab6,duration : 1, need : [B6]},
	M1 = task{name:m1,duration : 16, need : [AB1], use : bricklaying},
	M2 = task{name:m2,duration : 8, need : [AB2], use : bricklaying},
	M3 = task{name:m3,duration : 8, need : [AB3], use : bricklaying},
	M4 = task{name:m4,duration : 8, need : [AB4], use : bricklaying},
	M5 = task{name:m5,duration : 8, need : [AB5], use : bricklaying},
	M6 = task{name:m6,duration : 20, need : [AB6], use : bricklaying},
	EndOfM = task{name:end_of_m,duration : 0, need : [M1,M2,M3,M4,M5,M6]},
	L = task{name:l,start : 30, duration : 2, need : [], use : crane},
	T1 = task{name:t1,duration : 12, need : [M1,M2,L], use : crane},
	T2 = task{name:t2,duration : 12, need : [M2,M3,L], use : crane},
	T3 = task{name:t3,duration : 12, need : [M3,M4,L], use : crane},
	T4 = task{name:t4,duration : 12, need : [M4,M5,L], use : crane},
	T5 = task{name:t5,duration : 12, need : [M5,M6,L], use : crane},
	UA = task{name:ua,duration : 10, need : [UE]},
	V1 = task{name:v1,duration : 15, need : [T1], use : caterpillar},
	V2 = task{name:v2,duration : 10, need : [T5], use : caterpillar},
	PE = task{name:pe,duration : 0, need : [T2,T3,T4,UA,V1,V2], start:EndDate},
	(foreach(task{start:S},Tasks), 
         foreach(S,Starts) do 
            true 
        ),
        Starts :: 0..200,
        writeln(tasks),
	% Distance constraints  -------------------
	end_to_end_max(S6, B6, 4),
	end_to_end_max(S5, B5, 4),
	end_to_end_max(S4, B4, 4),
	end_to_end_max(S3, B3, 4),
	end_to_end_max(S2, B2, 4),
	end_to_end_max(S1, B1, 4),
	end_to_start_max(A6, S6, 3),
	end_to_start_max(A5, S5, 3),
	end_to_start_max(P2, S4, 3),
	end_to_start_max(P2, S4, 3),
	end_to_start_max(P1, S3, 3),
	end_to_start_max(A2, S2, 3),
	end_to_start_max(A1, S1, 3),
	start_to_start_min(UE, StartOfF, 6),
	end_to_start_min(EndOfM, UA, -2),

        writeln(distance),
	% Precedence constraints  -------------------
	( foreach(task{start:Si,need:NeededTasks}, Tasks) do
	    Si #>= 0,
	    ( foreach(task{start:Sj,duration:Dj}, NeededTasks), param(Si) do
		Si #>= Sj+Dj
	    )
	),

        writeln(prec),
	% Search and optimisation -------------------

        create_visualization([output:"Viz_bridge_RESULT"],Handle),
        root(Handle),
	bb_min((no_overlaps(Handle,Tasks),	% disjunctions
                labeling(Starts),        
                solution(Handle)
               ), EndDate, bb_options{strategy:step}),
        writeln(EndDate),
        close_visualization(Handle),
        viz(Handle, [tool{show:tree,repeat:final}]).
       


% some auxliliary definitions

start_to_start_min(task{start:S1}, task{start:S2} , Min) :-
	S1+Min #=< S2.

end_to_end_max(task{start:S1,duration:D1}, task{start:S2,duration:D2}, Max) :-
	S1+D1+Max #>= S2+D2.

end_to_start_max(task{start:S1,duration:D1}, task{start:S2}, Max) :-
	S1+D1+Max #>= S2.

end_to_start_min(task{start:S1,duration:D1}, task{start:S2}, Min) :-
	S1+D1+Min #=< S2.


% tasks can't overlap if they use the same resource
% this is where the disjunctions are

no_overlaps(Handle,Tasks) :-
	(fromto(Tasks, [Task0|Tasks0], Tasks0, []),
         param(Handle) do
	    (foreach(Task1,Tasks0), 
             param(Handle,Task0) do
		Task0 = task{name:Name0,start:S0, duration:D0, use:R0},
		Task1 = task{name:Name1,start:S1, duration:D1, use:R1},
		( R0 == R1 ->
		    no_overlap(Handle,Name0,S0, D0, Name1,S1, D1)
		;
		    true
		)
	    )
	).

no_overlap(Handle,Namei,Si,Di,Namej,Sj,_Dj) :-
        sprintf(Choice,"%w - %w",[Namei,Namej]),
	(Sj #>= Si+Di,true ->
            try_c(Handle,"",-1,Choice)
        ;
            failure_c(Handle,"",-1,Choice),
            fail
        ).
no_overlap(Handle,Namei,Si,_Di,Namej,Sj,Dj) :-
        sprintf(Choice,"%w - %w",[Namej,Namei]),
	(Si #>= Sj+Dj,true ->
            try_c(Handle,"",-1,Choice)
        ;
            failure_c(Handle,"",-1,Choice),
            fail
        ).
