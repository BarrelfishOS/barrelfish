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
:-module(problem).

:-export(problem/3).

:-use_module(structures).
:-lib(util).

problem(Dir,File,problem{cars:Cars,
                         models:Models,
                         required:Required,
                         using_options:UsingOptions,
                         value_order:Ordered}):-
        concat_string([Dir,'/',File],Full),
        open(Full,read,S),
        read_line(S,_),
        read_line(S,_),
        read_line(S,_),
        read_numbers(S,3,[Cars,Options,Models]),
        read_numbers(S,Options,Ks),
        read_numbers(S,Options,Ns),
        Size is Options+2,
        dim(Matrix,[Models,Options]),
        (for(I,1,Models),
         foreach(Req,Required),
         param(S,Size,Matrix) do
            read_numbers(S,Size,[_,Req|Values]),
            (foreach(Value,Values),
             count(J,1,_),
             param(I,Matrix) do
                subscript(Matrix,[I,J],Value)
            )
        ),
        close(S),
        (for(J,1,Options),
         foreach(K,Ks),
         foreach(N,Ns),
         foreach(option{k:K,n:N,
                        values:Using,
                        index_set:IndexSet,
                        slack:Slack,
                        total_use:Count},UsingOptions),
         param(Matrix,Models,Required,Cars) do
            (for(I,1,Models),
             foreach(V,IndexSet),
             foreach(Req,Required),
             fromto(Using,A1,A,[]),
             fromto(0,B,B1,Count),
             param(Matrix,J) do
                subscript(Matrix,[I,J],V),
                (V = 1 ->
                    A1 = [I|A],
                    B1 is B+Req
                ;
                    A1 = A,
                    B1 = B
                )
            ),
            Slack is 1- (K/N - Count/Cars),
            writeln(option(Slack,K,N,Count,Cars))
        ),
        (for(I,1,Models),
         foreach(Key-I,Keyed),
         param(Matrix,Options,UsingOptions) do
            (for(J,1,Options),
             foreach(option{slack:Slack},UsingOptions),
             fromto(0,A,A1,Key),
             param(I,Matrix) do
                subscript(Matrix,[I,J],V),
                A1 is A+V*Slack
            )
        ),
        sort(1,>=,Keyed,Sorted),
        (foreach(_-X,Sorted),
         foreach(X,Ordered) do
            true
        ).


read_numbers(_S,0,[]):-
        !.
read_numbers(S,N,[X|R]):-
        read_token(S,X,integer),
        N1 is N-1,
        read_numbers(S,N1,R).
