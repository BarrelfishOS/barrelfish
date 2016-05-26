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
% The Original Code is  The Cardinal Constraint Solver for ECLiPSe. 
% The Initial Developer of the Original Code is  Francisco M.C.A. Azevedo. 
% Portions created by the Initial Developer are  Copyright (C) 2000-2004.
% All Rights Reserved.
% 
% Contributor(s): Francisco M. C. A. Azevedo <fa@di.fct.unl.pt>. 
% 
% Alternatively, the contents of this file may be used under the terms of
% either of the GNU General Public License Version 2 or later (the "GPL"),
% or the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
% in which case the provisions of the GPL or the LGPL are applicable instead
% of those above. If you wish to allow use of your version of this file only
% under the terms of either the GPL or the LGPL, and not to allow others to
% use your version of this file under the terms of the MPL, indicate your
% decision by deleting the provisions above and replace them with the notice
% and other provisions required by the GPL or the LGPL. If you do not delete
% the provisions above, a recipient may use your version of this file under
% the terms of any one of the MPL, the GPL or the LGPL.
% END LICENSE BLOCK
%
% cardinal_minmax.pl      By Francisco Azevedo    2000 - 2004
%
% Minimum and Maximum set functions of Cardinal.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- lib(fd).


%--------
% minimum_function(+Min, +Set)
%  Inferences for Set's minimum function.
%--
minimum_function(Min, Set):-
	cardinality(Set, CardSet),
	integers_only(Set, CardSet),
	(var(Min) -> MinVar=Min ; MinVar::Min),
	set_minimum(Set, MinVar),
	CardSet #>= 1,
	suspend_and_call(minimum_bounded(MinVar,Set,CardSet,[SuspMB,SuspCLM]), 4,
		[Set->cardinal:bounded,CardSet->fd:min], SuspMB),
	suspend_and_call(check_lower_min(MinVar,Set), 4, MinVar->fd:min, SuspCLM),
	(ground(MinVar) -> min_inst(MinVar,Set,[SuspMB,SuspCLM])
	;suspend(min_inst(MinVar,Set,[SuspMB,SuspCLM,SuspMI]), 3, MinVar->inst, SuspMI)
	).

%--------
% integers_only(+Set, +CardSet)
%  Force Set (with cardinality CardSet) to have only integers.
%--
integers_only(Set, CardSet):-
	domain(Set, CardSet, [Glb:NIn,Poss:_]),
	\+ (member(X,Glb), \+ integer(X)),
	integers_list(Poss, NewPoss, NIn,NewNMax),
	set_poss(Set, NewPoss,NewNMax).

%--------
% integers_list(+List, -Integers, Ni,No)
%  Integers is the list of the No-Ni integers of List.
%--
integers_list([], [], N,N).
integers_list([H|T], [H|Is], Ni,No):- integer(H), !, N1 is Ni+1, integers_list(T,Is,N1,No).
integers_list([_|T], Is, Ni,No):- integers_list(T,Is,Ni,No).

%--------
% minimum_bounded(+Min, +Set, +CardSet, +Susps)
%  Update Set's minimum function Min due to the Set's domain change (bounded Set).
% CardSet is Set's cardinality.
%--
:-demon minimum_bounded/4.
minimum_bounded(Min, Set, _, Susps):- nonvar(Set), !, kill_suspensions(Susps), Set=[Min|_].
minimum_bounded(Min, Set, CardSet, Susps):-
	domain(Set, [Glb:NIn,Poss:NMax]),
	mindomain(CardSet, MinCardSet),
%	MinPoss is MinCardSet-NIn, LengthPoss is NMax-NIn, MaxNthMinPoss is LengthPoss-MinPoss+1,
	MaxNthMinPoss is NMax-MinCardSet+1,
	till_nth(MaxNthMinPoss, Poss, PossTillNth),
	PossTillNth = [MinPoss|_],
	(Glb=[] -> MinGlb = MinPoss ; Glb=[MinGlb|_], Min #<= MinGlb),
	(MinGlb < MinPoss -> kill_suspensions(Susps), Min #= MinGlb
	;insert(PossTillNth, MinGlb, DomMin, _, Tail),
	 (Tail=[], MinCardSet > NIn -> Min::PossTillNth ; Min::DomMin)
	).

%--------
% check_lower_min(+Min, +Set)
%  Update Set's domain due to the lower bound change of the Set's minimum function Min.
%--
:-demon check_lower_min/2.
check_lower_min(Min, Set):-
	mindomain(Min, LowerMin),
	domain(Set, [Glb:_,Poss:NMax]),
	(Glb=[] -> true ; Glb=[MinGlb|_], MinGlb >= LowerMin),
	LowerMin_1 is LowerMin-1,
	insert_count(Poss, LowerMin_1, _, _, NewPoss, 0,NOut),
	NewNMax is NMax-NOut,
	set_poss(Set, NewPoss, NewNMax).

%--------
% min_inst(+Min, +Set, +Susps)
%  Update Set's domain due to the instantiation of the Set's minimum function Min.
%--
min_inst(Min, Set, Susps):-
	kill_suspensions(Susps),
	Min `@ Set,
	domain(Set, [[Min|_]:_,Poss:NMax]),
	insert_count(Poss, Min, _, _, NewPoss, 0,NOut),
	NewNMax is NMax-NOut,
	set_poss(Set, NewPoss, NewNMax).




%--------
% maximum_function(+Max, +Set)
%  Inferences for Set's maximum function.
%--
maximum_function(Max, Set):-
	cardinality(Set, CardSet),
	integers_only(Set, CardSet),
	(var(Max) -> MaxVar=Max ; MaxVar::Max),
	set_maximum(Set, MaxVar),
	CardSet #>= 1,
	suspend_and_call(maximum_bounded(MaxVar,Set,CardSet,[SuspMB,SuspCUM]), 4,
		[Set->cardinal:bounded,CardSet->fd:min], SuspMB),
	suspend_and_call(check_upper_max(MaxVar,Set), 4, MaxVar->fd:max, SuspCUM),
	(ground(MaxVar) -> max_inst(MaxVar,Set,[SuspMB,SuspCUM])
	;suspend(max_inst(MaxVar,Set,[SuspMB,SuspCUM,SuspMI]), 3, MaxVar->inst, SuspMI)
	).

%--------
% maximum_bounded(+Max, +Set, +CardSet, +Susps)
%  Update Set's maximum function Max due to the Set's Poss change (bounded Set).
% CardSet is Set's cardinality.
%--
:-demon maximum_bounded/4.
maximum_bounded(Max, Set, _, Susps):- nonvar(Set), !, kill_suspensions(Susps), reverse(Set,[Max|_]).
maximum_bounded(Max, Set, CardSet, Susps):-
	domain(Set, [Glb:NIn,Poss:_NMax]),
	mindomain(CardSet, MinCardSet),
	MinNthMaxPoss is MinCardSet-NIn,
	(MinNthMaxPoss > 0 -> from_nth(MinNthMaxPoss, Poss, PossFromNth) ; PossFromNth=Poss),
	reverse(PossFromNth, [MaxPoss|_]),
	(Glb=[] -> MaxGlb = MaxPoss ; reverse(Glb,[MaxGlb|_]), Max #>= MaxGlb),
	(MaxGlb > MaxPoss -> kill_suspensions(Susps), Max #= MaxGlb
	;MinCardSet > NIn, PossFromNth=[MinPoss|_], MaxGlb < MinPoss -> Max::PossFromNth
	;Max::[MaxGlb|PossFromNth]
	).

%--------
% check_upper_max(+Max, +Set)
%  Update Set's domain due to the upper bound change of the Set's maximum function Max.
%--
:-demon check_upper_max/2.
check_upper_max(Max, Set):-
	maxdomain(Max, UpperMax),
	domain(Set, [Glb:NIn,Poss:_]),
	(Glb=[] -> true ; reverse(Glb,[MaxGlb|_]), MaxGlb =< UpperMax),
	UpperMax1 is UpperMax+1,
	insert_count(Poss, UpperMax1, _, NewPoss-[], _, 0,NPoss),
	NewNMax is NIn+NPoss,
	set_poss(Set, NewPoss, NewNMax).

%--------
% max_inst(+Max, +Set, +Susps)
%  Update Set's domain due to the instantiation of the Set's maximum function Max.
%--
max_inst(Max, Set, Susps):-
	kill_suspensions(Susps),
	Max `@ Set,
	domain(Set, [Glb:NIn,Poss:_]),
	reverse(Glb, [Max|_]),
	insert_count(Poss, Max, _, NewPoss-[], _, 0,NPoss),
	NewNMax is NIn+NPoss,
	set_poss(Set, NewPoss, NewNMax).
