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
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK
% -------------------------------
% module working on ground sets which are ordered largest first.
%
% opposite of lib(ordset).
% -------------------------------

:-module(lex_set).

%:-lib(queues).
%:-use_module(myqueues).

%:-pragma(nodebug).

:-export
  op(700, xfx, lex_leq),
  op(700, xfx, lex_geq),
  op(700, xfx, lex_less),
  op(700, xfx, lex_greater).


% X is lex less-than-or-equal to Y
:-export lex_leq/2.
lex_leq([],_Y).
lex_leq([X|Xs],[Y|Ys]):-
        lex_leq_aux(X,Xs,Y,Ys).
lex_leq_aux(X,_Xs,Y,_Ys):-
        X < Y,!.
lex_leq_aux(X,Xs,Y,Ys):-
        X == Y,
        lex_leq(Xs,Ys).

% X is lex greater-than-or-equal to Y
:-export lex_geq/2.
lex_geq(_X,[]).
lex_geq([X|Xs],[Y|Ys]):-
        lex_geq_aux(X,Xs,Y,Ys).
lex_geq_aux(X,_Xs,Y,_Ys):-
        X > Y,!.
lex_geq_aux(X,Xs,Y,Ys):-
        X == Y,
        lex_geq(Xs,Ys).

% X is strictly lex less-than Y
:-export lex_less/2.
lex_less([],[_Y|_Ys]).
lex_less([X|Xs],[Y|Ys]):-
        lex_less_aux(X,Xs,Y,Ys).
lex_less_aux(X,Xs,Y,Ys):-
        X == Y,!,
        lex_less(Xs,Ys).
lex_less_aux(X,_Xs,Y,_Ys):-
        X < Y.

% X is strictly lex greater-than Y
:-export lex_greater/2.
lex_greater([_Y|_Ys],[]).
lex_greater([X|Xs],[Y|Ys]):-
        lex_greater_aux(X,Xs,Y,Ys).
lex_greater_aux(X,_Xs,Y,_Ys):-
        X > Y,!.
lex_greater_aux(X,Xs,Y,Ys):-
        X == Y,
        lex_greater(Xs,Ys).

% X /\ Y = I
:-export lex_int/3.
lex_int([],_Y,[]).
lex_int([X|Xs],Y,I):-
        lex_int_aux(X,Xs,Y,I).
lex_int_aux(_X,_Xs,[],[]).
lex_int_aux(X,Xs,[Y|Ys],I):-
        lex_int_aux2(X,Xs,Y,Ys,I).
lex_int_aux2(X,Xs,Y,Ys,[X|Is]):-
        X==Y,!,
        lex_int(Xs,Ys,Is).
lex_int_aux2(X,Xs,Y,Ys,Is):-
        X>Y,!,
        lex_int_aux(Y,Ys,Xs,Is).
lex_int_aux2(X,Xs,_Y,Ys,Is):-
        %X<Y,!,
        lex_int_aux(X,Xs,Ys,Is).

% | X /\ Y | = C
:-export lex_int_card/3.
lex_int_card([],_Y,0).
lex_int_card([X|Xs],Y,C):-
        lex_int_card_aux(X,Xs,Y,C).
lex_int_card_aux(_X,_Xs,[],0).
lex_int_card_aux(X,Xs,[Y|Ys],C):-
        lex_int_card_aux2(X,Xs,Y,Ys,C).
lex_int_card_aux2(X,Xs,Y,Ys,C1):-
        X==Y,!,
        lex_int_card(Xs,Ys,C0),
        C1 is C0+1.
lex_int_card_aux2(X,Xs,Y,Ys,C):-
        X>Y,!,
        lex_int_card_aux(Y,Ys,Xs,C).
lex_int_card_aux2(X,Xs,_Y,Ys,C):-
        %X<Y,!,
        lex_int_card_aux(X,Xs,Ys,C).


:-export lex_subtract/3.
% Xs must be a subset of Ys
lex_subtract([],Ys,Ys).
lex_subtract([X|Xs],[Y|Ys],Zs):-
        X == Y,!,
        lex_subtract(Xs,Ys,Zs).
lex_subtract([X|Xs],[Y|Ys],[Z|Zs]):-
        X < Y,!,
        Z=Y,
        lex_subtract([X|Xs],Ys,Zs).

:-export lex_union/3.
lex_union([],Ys,Ys).
lex_union([X|Xs],Ys,Zs):-
        lex_union_aux(X,Xs,Ys,Zs).
lex_union_aux(X,Xs,[],[X|Xs]).
lex_union_aux(X,Xs,[Y|Ys],Zs):-
        lex_union_aux2(X,Xs,Y,Ys,Zs).
lex_union_aux2(X,Xs,Y,Ys,[Z|Zs]):-
        X==Y,!,
        Z=X,
        lex_union(Xs,Ys,Zs).
lex_union_aux2(X,Xs,Y,Ys,[Z|Zs]):-
        X>Y,!,
        Z=X,
        lex_union_aux(Y,Ys,Xs,Zs).
lex_union_aux2(X,Xs,Y,Ys,[Z|Zs]):-
        % X<Y,!,
        Z=Y,
        lex_union_aux(X,Xs,Ys,Zs).


:-export lex_diff/3.
% lex_difference(X,Y,Z)  Z = X \ Y
lex_diff([],_Ys,[]).
lex_diff([X|Xs],Ys,Zs):-
        lex_diff_aux(X,Xs,Ys,Zs).

lex_diff_aux(X,Xs,[],[X|Xs]).
lex_diff_aux(X,Xs,[Y|Ys],Zs):-
        lex_diff_aux2(X,Xs,Y,Ys,Zs).

lex_diff_aux2(X,Xs,Y,Ys,Zs):-
        X == Y,!,
        lex_diff(Xs,Ys,Zs).
lex_diff_aux2(X,Xs,Y,Ys,Zs):-
        X < Y,!,
        lex_diff_aux(X,Xs,Ys,Zs).
lex_diff_aux2(X,Xs,Y,Ys,[X|Zs]):-
        % X > Y,
        lex_diff(Xs,[Y|Ys],Zs).

:-export lex_memberchk/2.
lex_memberchk(E,[X|Xs]):-
        E < X,!,
        lex_memberchk(E,Xs).
lex_memberchk(X,[X|_Xs]).
%lex_memberchk(E,[X|Xs]):-
%        E < X,
%        lex_memberchk(E,Xs).


:-export lex_subset/2.
lex_subset([],[_Y|_Ys]):-!.
lex_subset([X|Xs],[Y|Ys]):-
        lex_subset_aux(X,Xs,Y,Ys).

lex_subset_aux(X,Xs,Y,Ys):-
        X < Y,!,
        lex_subset([X|Xs],Ys).
lex_subset_aux(X,Xs,Y,Ys):-
        X = Y,
        lex_subset(Xs,Ys).

:-export lex_subseteq/2.
lex_subseteq([],_Ys):-!.
lex_subseteq([X|Xs],[Y|Ys]):-
        lex_subseteq_aux(X,Xs,Y,Ys).

lex_subseteq_aux(X,Xs,Y,Ys):-
        X < Y,!,
        lex_subseteq([X|Xs],Ys).
lex_subseteq_aux(X,Xs,Y,Ys):-
        X = Y,
        lex_subseteq(Xs,Ys).


:-export lex_add_fixed_minimal/3.
% keep the minimal values
lex_add_fixed_minimal(E, [_], [E]):-!.
lex_add_fixed_minimal(E, [X|Xs], [Y|Ys]):-
        E >= X, !,
        Y=E,
        Ys = Xs.
lex_add_fixed_minimal(E, [X2,X1|Xs], [Y2,Y1|Ys]):-
        % E < X2,!
        E >= X1,!,
        Y2 = X2,
        Y1 = E,
        Ys = Xs.
lex_add_fixed_minimal(E, [_X2,X1|Xs], [Y2,Y1|Ys]):-
        % E < X2,!
        % E < X1,!
        Y2 = X1,
        lex_add_fixed_minimal(E,[X1|Xs],[Y1|Ys]).

:-export lex_add_fixed_maximal/3.
% keep the maximal values
lex_add_fixed_maximal(E, [_], [E]):-!.
lex_add_fixed_maximal(E, [X|Xs], [Y|Ys]):-
        E > X, !,
        Y=E,
        lex_add_fixed_maximal(X, Xs,Ys).
lex_add_fixed_maximal(E, [X|Xs], [Y|Ys]):-
        E == X, !,
        Y = X,
        Ys = Xs.
lex_add_fixed_maximal(E, [X|Xs], [Y|Ys]):-
        % E < X
        Y = X,
        lex_add_fixed_maximal(E,Xs,Ys).

:-export lex_add/3.
lex_add(E,[],[E]).
lex_add(E,[X|Xs],Ys):-
        E > X,!,
        Ys = [E,X|Xs].
lex_add(E,[X|Xs],Ys):-
        E == X,!,
        Ys = [X|Xs].
lex_add(E,[X|Xs],[Y|Ys]):-
        % E < X,
        Y=X,
        lex_add(E,Xs,Ys).

downto(Min,Min,Min):-!.  % base case
downto(_Min,Max,Max):-true.
downto(Min,Max,I):-
        NewMax is Max - 1,
        downto(Min,NewMax,I).

upto(Max,Max,Max):-!.  % base case
upto(Min,_Max,Min):-true.
upto(Min,Max,I):-
        NewMin is Min + 1,
        upto(NewMin,Max,I).

% Ys is the lex smallest K-set in [Glb,Lub] which is lex larger than Xs
:-export lex_next/5.
lex_next(Xs,Glb,Lub,K,Ys):-
        %subset_increasing(Lub,Glb,K,K,Ys),
        subset_geq_increasing(Xs,Glb,Lub,K,K,Ys),
        lex_less(Xs,Ys).
% Ys is the lex smallest KOut-set in [Glb,Lub] which is lex larger
% than Xs, and KOut is initialy KMin upto KMax.
:-export lex_next/7.
old_lex_next(Xs,Glb,Lub,KMin,KMax,KOut,Ys):-
        subset_increasing(Glb,Lub,KMin,KMax,Ys),
        lex_less(Xs,Ys),
        KOut=KMin.    % what should this value be?
lex_next(Xs,Glb,Lub,KMin,KMax,KOut,Ys):-
        lex_next(Xs,Glb,Lub,KMin,KMax,KOut,Ys,_).

:-export lex_next/8.
lex_next(Xs,Glb,Lub,KMin,KMax,KOut,Ys,Goal):-
        subset_geq_increasing(Xs,Glb,Lub,KMin,KMax,Ys,Goal),
        %lex_subseteq(Glb,Ys),
        lex_less(Xs,Ys),
        KOut=KMin,    % what should this value be?
        true.

% Ys is the lex largest K-set in [Glb,Lub] which is lex smaller than Xs
:-export lex_prev/5.
lex_prev(Xs,Glb,Lub,K,Ys):-
        %subset_decreasing(Lub,Glb,K,K,Ys),
        subset_leq_decreasing(Xs,Glb,Lub,K,K,Ys),
        lex_subseteq(Glb,Ys),
        lex_less(Ys,Xs).

% Ys is the lex largeest KOut-set in [Glb,Lub] which is lex smaller
% than Xs, and KOut is initialy KMax downto KMin.
:-export lex_prev/7.
old_lex_prev(Xs,Glb,Lub,KMin,KMax,KOut,Ys):-
        subset_decreasing(Glb,Lub,KMin,KMax,Ys),
        (lex_less(Ys,Xs);writeln(failed-lex_less(Ys,Xs)),fail),
        KOut=KMax.   % what should this value be?

lex_prev(Xs,Glb,Lub,KMin,KMax,KOut,Ys):-
        lex_prev(Xs,Glb,Lub,KMin,KMax,KOut,Ys,_).

:-export lex_prev/8.
lex_prev(Xs,Glb,Lub,KMin,KMax,KOut,Ys,Goal):-
        subset_leq_decreasing(Xs,Glb,Lub,KMin,KMax,Ys,Goal),
        lex_less(Ys,Xs),
        KOut=KMax.   % what should this value be?



safe_call(Goal,Tail,Args,Values):-
        arg(1,Values,Result),
        shelf_create(result(_Var),Shelf),
        not(
               not((
                       Tail=[], % ground the tail of the currently
                                % oipen SubSet list
                       Args=Values, % unfify the arguments
                       Goal,  % call the goal
                              %store value of Result variable
                       (nonvar(Result) ->
                           shelf_set(Shelf, 1, Result)
                       ;
                           true
                       ),
                       true
                  ))
           ),
        % unify the stored result with the callers Result variable
        shelf_get(Shelf, 1, Result).



:-export subset_leq_decreasing/6.
subset_leq_decreasing(OldLexMax,Glb,Lub,Min,Max,SubSet):-
        subset_leq_decreasing(OldLexMax,Glb,Lub,Min,Max,SubSet,_Goal).

:-export subset_leq_decreasing/7.
orig_subset_leq_decreasing(OldLexMax,Glb,Lub,Min,Max,SubSet,_Goal-_Args):-
        old_subset_leq_decreasing(OldLexMax,Glb,Lub,Min,Max,SubSet).

subset_leq_decreasing(OldLexMax,Glb,Lub,Min,Max,SubSet,Goal-Args):-
        %writeln(calling-subset_leq_decreasing(Goal,OldLexMax,Lub,Glb,Min,Max,SubSet)),
        % optimise initial domain assignement case
        length(Lub, LubRemaining),
        length(Glb, GlbRemaining),
        (fromto(Lub,[X|RemainingIn],RemainingOut,[]),
         fromto(LubRemaining,LubRemainIn,LubRemainOut,_),
         fromto(GlbRemaining,GlbRemainIn,GlbRemainOut,_),
         fromto(0,SubSetLenIn,SubSetLenOut,_),
         fromto(Glb,GlbIn,GlbOut,_),
         fromto(SubSet,In,Out,[]),
         param(Min,Max,OldLexMax,Goal,Args,SubSet) do
            LubRemainIn >= Min - SubSetLenIn ,
            ( (GlbRemainIn =:= Max - SubSetLenIn) ->
                % only the remaining glb elements can be included
                %append(In,GlbIn,Out),
                In=GlbIn,
                Out=[],
                RemainingOut=[]
            ; LubRemainIn =:= Min - SubSetLenIn ->
                % all the remaining lub elemenmts must be in the
                % subset
                %append(In,[X|RemainingIn],Out),
                In=[X|RemainingIn],
                Out=[],
                RemainingOut=[]
%            ; (append(In,[X],TempOut),lex_set:lex_less(OldLexMax,TempOut)) ->
            ; (not not (In=[X],lex_set:lex_less(OldLexMax,SubSet))) ->
                % Never add elements which may make the subset
                % lex_great than OldLexMax
                RemainingOut=RemainingIn,
                LubRemainOut is LubRemainIn-1,
                (GlbIn = [X|GlbOut] ->
                    fail
                ;
                    true
                ),
                GlbOut=GlbIn,
                GlbRemainOut=GlbRemainIn,
                SubSetLenOut = SubSetLenIn,
                Out = In,
                true
            ;
                RemainingOut=RemainingIn,
                LubRemainOut is LubRemainIn-1,
                % always include glb elements
                ( GlbIn = [X|GlbOut] -> % NOTE: the unification binds
                                        % GlbOut if successfull
                    SubSetLenOut is SubSetLenIn+1,
                    %append(In,[X],Out),
                    In=[X|Out],
                    GlbRemainOut is GlbRemainIn-1
                ;
                    GlbOut=GlbIn,
                    GlbRemainOut=GlbRemainIn,
                    % choice point to first exclude, then include the
                    % elemement X
                    (nonvar(Goal) ->
                        % If an optimsing goal is given the call it
                        safe_call(Goal,In,Args,[](Result,SubSet,SubSetLenIn,X,
                                               GlbOut,RemainingOut,Min,
                                               Max)),
                        %% If an optimsing goal is given the call it
                        %NewGoal =.. [Pred,Result,In,SubSetLenIn,X,GlbOut,RemainingOut,Min,Max|Args],
                        %call(Mod:NewGoal),
                        true
                    ;
                        true
                    ),
                    % check what to do
                    (var(Result) ->
                        % leave a choice point
                        (
                            %include
                            SubSetLenOut is SubSetLenIn+1,
                            %append(In,[X],Out)
                            In=[X|Out]
                        ;
                            % exclude
                            SubSetLenOut = SubSetLenIn,
                            Out = In
                        )
                    ; Result==include ->
                        % always include
                        SubSetLenOut is SubSetLenIn+1,
                        %append(In,[X],Out)
                        In=[X|Out]                        
                    ; % Result=exclude
                      % always exclude
                        SubSetLenOut = SubSetLenIn,
                        Out = In
                    )
                )
            ),
            true
        ),
        % check the returned subset
        ( lex_leq(SubSet,OldLexMax),
          length(SubSet,SubSetLen),
          SubSetLen >= Min,
          SubSetLen =< Max,
          lex_subseteq(Glb,SubSet)
        ->
            true
        ;
            %writeln("failing check"-subset_leq_decreasing(Goal,OldLexMax,Lub,Glb,Min,Max,SubSet)),
            fail
        ),
        true.




:-export old_subset_leq_decreasing/6.
old_subset_leq_decreasing(Xs,Glb,Lub,Min,Max,SubSet):-
        old_subset_leq_decreasing(Xs,Glb,Lub,Min,Max,0,SubSet).

old_subset_leq_decreasing([],_Glb,_Lub,Min,Max,Count,[]):-
        !,Min =< Count, Count =< Max.
old_subset_leq_decreasing([X|Xs],Glb,Lub,Min,Max,Count,SubSet):-
        old_subset_leq_decreasing_aux([X|Xs],Glb,Lub,Min,Max,Count,SubSet).

old_subset_leq_decreasing_aux(_Xs,_Glb,_Lub,_Min,Max,Count,_SubSet):-
        Count > Max,!,fail.
old_subset_leq_decreasing_aux(_Xs,Glb,_Lub,_Min,Max,Count,[]):-
        Count = Max,!,Glb=[].   % allow termination if no GLB elements remaining
old_subset_leq_decreasing_aux(_Xs,_Glb,[],Min,_Max,Count,[]):-
        Min =< Count,!.
old_subset_leq_decreasing_aux([X|Xs],[GlbH|GlbTail],[LubH|LubTail],Min,Max,Count,[SubH|SubTail]):-
        GlbH=LubH,
        !,     % always includ GLB elements
        SubH=LubH,
        (LubH < X ->
            NewXs = [X|Xs]
        ;
            LubH=X,
            NewXs = Xs
        ),
        NewCount is Count+1,
        old_subset_leq_decreasing(NewXs,GlbTail,LubTail,Min,Max,NewCount,SubTail).
old_subset_leq_decreasing_aux([X|Xs],Glb,[LubH|LubTail],Min,Max,Count,[SubH|SubTail]):-
        % GlbH\=LubH,
        (LubH < X ->
            NewXs = [X|Xs]
        ;
            LubH=X,
            NewXs = Xs
        ),
        SubH=LubH,  % include element
        NewCount is Count+1,
        old_subset_leq_decreasing(NewXs,Glb,LubTail,Min,Max,NewCount,SubTail).

old_subset_leq_decreasing_aux(Xs,Glb,[_LubH|Tail],Min,Max,Count,Sub):-
        % exclude element
        NewXs=Xs,
        NewCount=Count,
        old_subset_leq_decreasing(NewXs,Glb,Tail,Min,Max,NewCount,Sub).

% split the given LUB into two lists, where the Upper list never
% contains elements less than E.
% resatisfiable. returns increasingly long "Lower" lists
%
% split_on_equal_or_greater(E, Lub, Upper, Lower).
:-export split_on_equal_or_greater/4.
split_on_equal_or_greater(E, Lub, Upper, RevLower):-
        reverse(Lub,RevLub),
        append(Lower, Upper, RevLub),
        Upper=[Y|_],
        Y >= E,
        reverse(Lower,RevLower).

%
% subset_geq_increasing(OldLexMin,Lub,Glb,Min,Max,SubSet)
%
:-export subset_geq_test/1.
subset_geq_test(S):-
        (for(I,21,1,-1), foreach(I,Lub) do true),
        subset_geq_increasing([],Lub,[],19,20,S).

:-export subset_geq_increasing/7.
orig_subset_geq_increasing(OldLexMin,Glb,Lub,Min,Max,SubSet,_Goal):-
        old_subset_geq_increasing(OldLexMin,Glb,Lub,Min,Max,SubSet),
        %writeln(SubSet),
        true.

subset_geq_increasing(OldLexMin,Glb,Lub,Min,Max,SubSet):-
        subset_geq_increasing(OldLexMin,Glb,Lub,Min,Max,SubSet,_Goal).
        



subset_geq_increasing(OldLexMin,Glb,Lub,Min,Max,SubSet,Goal-Args):-
        % optimise initial domain assignement case
        LubRemaining is length(Lub),
        GlbRemaining is length(Glb),
        (fromto(Lub,[X|RemainingIn],RemainingOut,[]),
         fromto(LubRemaining,LubRemainIn,LubRemainOut,_),
         fromto(GlbRemaining,GlbRemainIn,GlbRemainOut,_),
         fromto(0,SubSetLenIn,SubSetLenOut,_),
         fromto(Glb,GlbIn,GlbOut,_),
         fromto(SubSet,In,Out,[]),
         param(Min,Max,OldLexMin,Goal,Args,SubSet) do
            LubRemainIn >= Min - SubSetLenIn ,
            ( (GlbRemainIn =:= Max - SubSetLenIn) ->
                % only the remaining glb elements can be included
                %append(In,GlbIn,Out),
                In=GlbIn,
                Out=[],
                RemainingOut=[]
            ; LubRemainIn =:= Min - SubSetLenIn ->
                % all the remaining lub elemenmts must be in the
                % subset
                %append(In,[X|RemainingIn],Out),
                In=[X|RemainingIn],
                Out=[],
                RemainingOut=[]
%            ; (append(In,[Y|_],OldLexMin), X < Y) ->
            ; (not not (SubSet=OldLexMin,In=[Y|_],X < Y)) ->
                % Never add elements which may make the subset
                % lex_less than OldLexMin
                RemainingOut=RemainingIn,
                LubRemainOut is LubRemainIn-1,
                (GlbIn = [X|GlbOut] ->
                    fail
                ;
                    true
                ),
                GlbOut=GlbIn,
                GlbRemainOut=GlbRemainIn,
                SubSetLenOut = SubSetLenIn,
                Out = In,
                true
            ;
                RemainingOut=RemainingIn,
                LubRemainOut is LubRemainIn-1,
                % always include glb elements
                ( GlbIn = [X|GlbOut] -> % NOTE: the unification binds
                                        % GlbOut if successfull
                    SubSetLenOut is SubSetLenIn+1,
                    %append(In,[X],Out),
                    In=[X|Out],
                    GlbRemainOut is GlbRemainIn-1
                ;
                    GlbOut=GlbIn,
                    GlbRemainOut=GlbRemainIn,
                    % choice point to first exclude, then include the
                    % elemement X
                    (nonvar(Goal) ->
                        %safe_call(Goal, Args, [](Result,In,SubSetLenIn,X,GlbOut,RemainingOut,Min,Max)),
                        safe_call(Goal, In, Args, [](Result,SubSet,SubSetLenIn,X,GlbOut,RemainingOut,Min,Max)),
                        %% If an optimsing goal is given the call it
                        %NewGoal =.. [Pred,Result,In,SubSetLenIn,X,GlbOut,RemainingOut,Min,Max|Args],
                        %call(Mod:NewGoal),
                        true
                    ;
                        true
                    ),
                    % check what to do
                    (var(Result) ->
                        % leave a choice point
                        (
                            % exclude
                            SubSetLenOut = SubSetLenIn,
                            Out = In
                        ;
                            %include
                            SubSetLenOut is SubSetLenIn+1,
                            %append(In,[X],Out)
                            In=[X|Out]
                        )
                    ; Result==include ->
                        % always include
                        SubSetLenOut is SubSetLenIn+1,
                        %append(In,[X],Out)
                        In=[X|Out]
                    ; % Result=exclude
                      % always exclude
                        SubSetLenOut = SubSetLenIn,
                        Out = In
                    )
                )
            ),
            true
        ),
        % check the returned subset
        ( length(SubSet,SubSetLen),
          SubSetLen >= Min,
          SubSetLen =< Max,
          lex_subseteq(Glb,SubSet),
          lex_leq(OldLexMin,SubSet)),
        true.






:-export old_subset_geq_increasing/6.
old_subset_geq_increasing([],Glb,Lub,Min,Max,SubSet):-
        !,
        subset_increasing(Glb, Lub, Min, Max, SubSet).
old_subset_geq_increasing(LexMin,Glb,Lub,Min,Max,SubSet):-
        lex_leq(LexMin, Glb),!,
        subset_increasing(Glb, Lub, Min, Max, SubSet).
old_subset_geq_increasing([X|Xs],Glb,Lub,Min,Max,SubSet):-
        split_on_equal_or_greater(X, Lub, RevUpper, Lower), % resat
        %reverse(Upper, RevUpper),
        RevUpper=[MinUpper|_Rest],
        (MinUpper==X ->
            % equal case
            SubSet=[X|Ys],
            Min1 is Min-1,
            Max1 is Max-1,
            lex_diff(Glb,[X],Glb1),
            old_subset_geq_increasing(Xs, Glb1, Lower, Min1, Max1, Ys)
        ;
            %strictly greater case
            SubSet=[MinUpper|Ys],
            Min1 is Min-1,
            Max1 is Max-1,
            lex_diff(Glb,[MinUpper],Glb1),
            old_subset_geq_increasing_aux(Glb1, Lower, Min1, Max1, Ys),
            true
        ).

old_subset_geq_increasing_aux(Glb, Lub, Min, Max, Ys):-
        subset_increasing(Glb, Lub, Min, Max, Ys).


% Ys is the lex largest K-set in [Glb,Lub] which is lex smaller than
% or equal to Xs
:-export lex_prev_eq/5.
lex_prev_eq(Xs,Glb,Lub,K,Ys):-
        subset_leq_decreasing(Xs,Glb,Lub,K,K,Ys),
        %subset_decreasing(Lub,Glb,K,K,Ys),
        (lex_leq(Ys,Xs);writeln(arse-subset_leq_decreasing(Xs,Glb,Lub,K,K,Ys)),fail),
        true.
% Ys is the lex largeest KOut-set in [Glb,Lub] which is lex smaller
% than or equal to Xs, and KOut is initialy KMax downto KMin.
:-export lex_prev_eq/7.
lex_prev_eq(Xs,Glb,Lub,KMin,KMax,KOut,Ys):-
        subset_leq_decreasing(Xs,Glb,Lub,KMin,KMax,Ys),        
%         ( (lex_subseteq(Glb,Ys), lex_subseteq(Ys,Lub), lex_leq(Ys,Xs))
%         ->
%             true
%         ;
%             writeln(lex_prev_eq:failed-lex_leq(Ys,Xs)),
%             fail
%         ),
 %       lex_subseteq(Glb,Ys), lex_subseteq(Ys,Lub), lex_leq(Ys,Xs),
        KOut=KMax.   % what should this value be?

% Ys is the lex smallest K-set in [Glb,Lub] which is lex larger than
% or equal to Xs
:-export lex_next_eq/5.
lex_next_eq(Xs,Glb,Lub,K,Ys):-
        %subset_increasing(Lub,Glb,K,K,Ys),
        subset_geq_increasing(Xs,Glb,Lub,K,K,Ys),
        lex_leq(Xs,Ys).
% Ys is the lex smallest KOut-set in [Glb,Lub] which is lex larger
% than or equal to Xs, and KOut is initialy KMin upto KMax.
:-export lex_next_eq/7.
lex_next_eq(Xs,Glb,Lub,KMin,KMax,KOut,Ys):-
        %subset_increasing(Lub,Glb,KMin,KMax,Ys),
        subset_geq_increasing(Xs,Glb,Lub,KMin,KMax,Ys),
%        lex_subseteq(Glb,Ys),
%        lex_subseteq(Ys,Lub),
%        lex_leq(Xs,Ys),
        KOut=KMin.    % what should this value be?


fucker.

% % Ys is the lex smallest K-set in [Glb,Lub] which is lex larger than
% % or equal to Xs
% :-export lex_next_eq/5.
% lex_next_eq(Xs,Glb,Lub,K,Ys):-
%         ord_subset_increasing(Glb,Lub,K,Ys),
%         lex_leq(Xs,Ys),!.
% :-export lex_next_eq/7.
% lex_next_eq(Xs,Glb,Lub,KMin,KMax,K,Ys):-
%         %(KMin=\=KMax -> fucker,abort; true),
%         upto(KMin,KMax,K),
%         ord_subset_increasing(Glb,Lub,K,Ys),
%         lex_leq(Xs,Ys),!.
% % Ys is the lex largest K-set in [Glb,Lub] which is lex smaller than
% % or equal to Xs
% :-export lex_prev_eq/5.
% lex_prev_eq(Xs,Glb,Lub,K,Ys):-
%         ord_subset_decreasing(Glb,Lub,K,Ys),
%         lex_leq(Ys,Xs),!.
% :-export lex_prev_eq/7.
% lex_prev_eq(Xs,Glb,Lub,KMin,KMax,K,Ys):-
%         %(KMin=\=KMax -> fucker,abort; true),
%         downto(KMin,KMax,K),
%         ord_subset_decreasing(Glb,Lub,K,Ys),
%         lex_leq(Ys,Xs),!.



% return the common prefix
:-export lex_common_prefix/3.
lex_common_prefix([],_Ys,[]).
lex_common_prefix([X|Xs],Ys,Common):-
        lex_common_prefix2(X,Xs,Ys,Common).

lex_common_prefix2(_X,_Xs,[],[]).
lex_common_prefix2(X,Xs,[Y|Ys],Common):-
        lex_common_prefix3(X,Xs,Y,Ys,Common).

lex_common_prefix3(X,Xs,X,Ys,[X|Common]):-
        lex_common_prefix(Xs,Ys,Common),!.
lex_common_prefix3(_X,_Xs,_Y,_Ys,[]).
        
%
% subset_decreasing(+Lub,+Glb,+Min,+Max,-SubSet)
%
:-export subset_decreasing/5.
subset_decreasing(Glb,Lub,Min,Max,SubSet):-
        lex_subtract(Glb,Lub,Remaining),
        length(Glb,LenGlb),
        subset_decreasing_aux(Remaining, Min, Max, LenGlb, RemainingSubSet),
        lex_union(RemainingSubSet,Glb,SubSet).

subset_decreasing_aux([], Min, Max, Count, []):-
        Count >= Min, Count =< Max.
subset_decreasing_aux([LubH|LubTail], Min, Max, Count, SubSet):-
        (Count < Max ->
            subset_decreasing_aux2(LubH,LubTail,Min,Max,Count,SubSet)
        ;
            Count=Max,
            SubSet=[]
        ).
subset_decreasing_aux2(LubH,LubTail, Min, Max, Count, [LubH|SubSet]):-
        NewCount is Count+1,
        subset_decreasing_aux(LubTail, Min, Max, NewCount, SubSet).
subset_decreasing_aux2(_LubH,LubTail, Min, Max, Count, SubSet):-
        subset_decreasing_aux(LubTail, Min, Max, Count, SubSet).


%
% subset_increasing(+Lub,+Glb,+Min,+Max,-SubSet)
%
:-export subset_increasing/5.
subset_increasing(Glb,Lub,Min,Max,SubSet):-
        lex_subtract(Glb,Lub,Remaining),
        length(Glb,LenGlb),
        subset_increasing_aux(Remaining, Min, Max, LenGlb, RemainingSubSet),
        lex_union(RemainingSubSet,Glb,SubSet).

subset_increasing_aux([], Min, Max, Count, []):-
        Count >= Min, Count =< Max.
subset_increasing_aux([LubH|LubTail], Min, Max, Count, SubSet):-
        (Count < Max ->
            subset_increasing_aux2(LubH,LubTail,Min,Max,Count,SubSet)
        ;
            Count=Max,
            SubSet=[]
        ).
subset_increasing_aux2(_LubH,LubTail, Min, Max, Count, SubSet):-
        subset_increasing_aux(LubTail, Min, Max, Count, SubSet).
subset_increasing_aux2(LubH,LubTail, Min, Max, Count, [LubH|SubSet]):-
        NewCount is Count+1,
        subset_increasing_aux(LubTail, Min, Max, NewCount, SubSet).

slow_subset_increasing(Glb,Lub,Min,Max,SubSet):-
        subset_increasing(Glb,Lub,Min,Max,0,SubSet).
subset_increasing([],[],Min,Max,Count,[]):-
        Min =< Count, Count =< Max,!.
subset_increasing([],_Lub,_Min,Max,Count,[]):-
        Count = Max,!.   % allow termination if no GLB elements remaining
subset_increasing([GlbH|GlbTail],[LubH|LubTail],Min,Max,Count,[SubH|SubTail]):-
        GlbH=LubH,
        !,     % always includ GLB elements
        Count<Max,
        SubH=LubH,
        NewCount is Count+1,
        subset_increasing(GlbTail,LubTail,Min,Max,NewCount,SubTail).
subset_increasing(Glb,[_|Tail],Min,Max,Count,Sub):-
        % exclude element
        Count<Max,
        NewCount=Count,
        subset_increasing(Glb,Tail,Min,Max,NewCount,Sub).
subset_increasing(Glb,[LubH|LubTail],Min,Max,Count,[SubH|SubTail]):-
        % GlbH\=LubH,
        Count<Max,
        SubH=LubH,  % include element
        NewCount is Count+1,
        subset_increasing(Glb,LubTail,Min,Max,NewCount,SubTail).

