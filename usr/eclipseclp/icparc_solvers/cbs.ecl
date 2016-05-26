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
% Copyright (C) 1999 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK
%----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: cbs.ecl,v 1.1 2006/09/23 01:53:22 snovello Exp $
%
% Credit search examples
%
%----------------------------------------------------------------------*/


:- lib(fd).
:- lib(fd_global).


%----------------------------------------------------------------------
% Credit search for boolean variables only
%----------------------------------------------------------------------

credit_search_01(Credit, Xs) :-
        (
            foreach(X, Xs),
            fromto(Credit, ParentCredit, ChildCredit, _)
        do
            ( var(X) ->
                ParentCredit > 0,  % possibly cut-off search here
                ( % Choice
                    X = 0, ChildCredit is (ParentCredit+1)//2
                ;
                    X = 1, ChildCredit is ParentCredit//2
                )
            ;
                ChildCredit = ParentCredit
            )
        ).



%----------------------------------------------------------------------
% Credit search for general finite domain variables
%----------------------------------------------------------------------

credit_search(_Credit, []).
credit_search(Credit, XXs) :-
        XXs = [X|Xs],           % select variable
%       deleteff(X, XXs, Xs),   % select variable
        ( var(X) ->
            ( Credit>1 ->
                    dcs(X,Credit,CreditOut),
                    credit_search(CreditOut, Xs)
              ;  
		    setval(saved_credit,0),
		    mindomain_all(XXs) 
            )
        ;
            credit_search(Credit, Xs)
        ).

    dcs(X,1,0) :- mindomain(X,X).
    dcs(X,Credit,CreditOut) :- 
             integer(Credit),
	     Credit>1,
             mindomain(X,Min),
	     distribute_credit(Credit, VCredit, CreditOver),
             (    setval(saved_credit,VCredit),
                  X #= Min,
                  CreditOut=VCredit
                  ;
                  getval(saved_credit,Saved),
                  X ## Min,
                  NewCredit is Saved+CreditOver,
                  ( Saved>0 -> writeln(use_saved_credit(Saved)) ; true ),
%                  NewCredit = CreditOver,                  
                  dcs(X,NewCredit,CreditOut)
             ). 
             
    mindomain_all(List) :-
           foreach(Y,List) do mindomain(Y,Y).

    distribute_credit(Credit, VCredit, CreditOver) :-
            Credit >= 1,
            % value V gets half remaining credit (rounded up) 
            VCredit is (Credit+1)//2,          
            CreditOver is Credit-VCredit.


%----------------------------------------------------------------------
% Example
%----------------------------------------------------------------------

%try
%test_cbs(10,500).

test_cbs(N, Credit) :-
    setval(count,0),
     length(List,N),
    (
%    List::1..10,
%    M is 2*N, List::1..M, ordered(<,List),
     M is 1+5*(N-1), List::1..M,  alldifferentN(List,5),
        credit_search(Credit,List),
        writeln(List),
        incval(count),
        fail
    ;
        getval(count,Count),
        writeln(Count)
    ).

alldifferentN([_],_N).
alldifferentN([X|Tail],N) :-
    alldiffN(X,Tail,N),
    alldifferentN(Tail,N).

alldiffN(_X,[],_).
alldiffN(X,[Y|Rest],N) :-
    X-Y #>= N #\/ Y-X #>= N,
    alldiffN(X,Rest,N).

