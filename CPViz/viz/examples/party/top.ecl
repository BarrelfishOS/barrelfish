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

:-lib(ic).
:-lib(ic_global).
:-lib(ic_global_gac).
:-lib(timeout).
:-lib(lists).
:-lib(cpviz).

:-local struct(boat(nr,cap,crew,sorting)).

top:-
        problem(10,Hosts,Guests),
%        once(model(Hosts,Guests,6,naive,"Viz_party_NAIVE")),
%        once(model(Hosts,Guests,6,first_fail,"Viz_party_FF")),
%        once(model(Hosts,Guests,6,layered,"Viz_party_LAYERED")),
        once(model(Hosts,Guests,6,credit,"Viz_party_CREDIT")),
        once(model(Hosts,Guests,6,layered_random,"Viz_party_RANDOM")),

        true.

model(Hosts,Guests,NrPeriods,Method,Output):-
        seed(0),
        writeq(Hosts),nl,
        writeq(Guests),nl,
        length(Hosts,NrHosts),
        length(Guests,NrGuests),
        dim(Matrix,[NrGuests,NrPeriods]),
        Matrix[1..NrGuests,1..NrPeriods] :: 1..NrHosts,

        create_visualization([output:Output,
                              range_to:2000],Handle),
        add_visualizer(Handle,
                       domain_matrix(Matrix),
                       [group:1,
                        display:text]),
        
        (for(I,1,NrGuests),
         param(Matrix,NrPeriods) do
            ic:alldifferent(Matrix[I,1..NrPeriods])
        ),

        (for(J,1,NrPeriods),
         param(Matrix,NrHosts,NrPeriods,NrGuests,Guests,Hosts,Handle) do
            make_bins(Hosts,Bins),
%            bin_packing_alone(Matrix[1..NrGuests,J],Guests,Bins),
            bin_packing(Matrix[1..NrGuests,J],Guests,Bins),
            X is NrPeriods+2+((J-1)//3)*(NrHosts+2),
            Y is ((J-1) mod 3)*13,
            add_visualizer(Handle,
                           vector(Bins),
                           [group:other,
                            x:X,
                            y:Y])
        ),

        (for(I,1,NrGuests-1),
         param(Matrix,NrGuests,NrPeriods) do
            (for(I1,I+1,NrGuests),
             param(Matrix,NrPeriods,I) do
                card_eq(Matrix[I,1..NrPeriods],
                        Matrix[I1,1..NrPeriods],1)
            )
        ),
        extract_array(Handle,col,1,Matrix,List),
        root(Handle),
        assign(Method,List,Matrix,NrPeriods,NrGuests,Handle),
        solution(Handle),
        close_visualization(Handle),
        viz(Handle, _).

assign(naive,List,_Matrix,_NrPeriods,_NrGuests,Handle):-
        !,
        search(List,1,input_order,tree_indomain(Handle,_),complete,[]).

assign(first_fail,List,_Matrix,_NrPeriods,_NrGuests,Handle):-
        !,
        search(List,1,first_fail,tree_indomain(Handle,_),complete,[]).
assign(layered,_,Matrix,NrPeriods,NrGuests,Handle):-
        !,
        repeat,
        (for(J,1,NrPeriods),
         param(Matrix,NrGuests,Handle) do
            writeln(period(J)),
            collection_to_list(Matrix[1..NrGuests,J],Vars),
            Start is (J-1)*NrGuests+1,
            indices(Vars,Start,J,1,List),
            once(search(List,1,first_fail,tree_indomain(Handle,_),
                        complete,[]))
        ),
        !.
assign(credit,_,Matrix,NrPeriods,NrGuests,Handle):-
        !,
        repeat,
        (for(J,1,NrPeriods),
         param(Matrix,NrGuests,Handle) do
            writeln(period(J)),
            collection_to_list(Matrix[1..NrGuests,J],Vars),
            Start is (J-1)*NrGuests+1,
            indices(Vars,Start,J,1,List),
            NSq is NrGuests*NrGuests,
            once(search(List,1,first_fail,tree_indomain(Handle,_),
                        credit(NSq,10),[]))
        ),
        !.
assign(layered_random,_,Matrix,NrPeriods,NrGuests,Handle):-
        !,
        repeat,
        (for(J,1,NrPeriods),
         param(Matrix,NrGuests,Handle) do
            writeln(period(J)),
            collection_to_list(Matrix[1..NrGuests,J],Vars),
            Start is (J-1)*NrGuests+1,
            indices(Vars,Start,J,1,List),
            NSq is NrGuests*NrGuests,
            once(search(List,1,first_fail,tree_indomain_random(Handle,_),
                        credit(NSq,10),[]))
        ),
        !.

indices([],_,_,_,[]).
indices([X|X1],N,J,K,[t(X,N,group(1,K-J))|T1]):-
        N1 is N+1,
        K1 is K+1,
        indices(X1,N1,J,K1,T1).

make_bins(HostCapacity,Bins):-
        (foreach(Cap,HostCapacity),
         foreach(B,Bins) do
            B :: 0..Cap
        ).

card_eq(Vector1,Vector2,Card):-
        collection_to_list(Vector1,List1),
        collection_to_list(Vector2,List2),
        (foreach(X,List1),
         foreach(Y,List2),
         fromto(0,A,A+B,Term) do
            #=(X,Y,B)
        ),
        eval(Term) #=< Card.

problem(Instance,HostCapacity,GuestSize):-
        problem_data(Instance,List),
        findall(boat{nr:Nr,cap:Cap,crew:Crew,sorting:Space},
                (boat(Nr,Cap,Crew),
                 Space is (Cap-Crew)*100+Crew,
                 memberchk(Nr,List)),Hosts),
        findall(boat{nr:Nr,cap:Cap,crew:Crew,sorting:Space},
                (boat(Nr,Cap,Crew),
                 Space is (Cap-Crew)*100+Crew,
                 not memberchk(Nr,List)),Guests),
        sort(sorting of boat,>=,Hosts,SortedHosts),
        sort(crew of boat,>=,Guests,SortedGuests),
        
        guest_pattern(SortedGuests,GuestSize),
        host_capacity(SortedHosts,HostCapacity).

host_capacity(Hosts,HostCapacity):-
        (foreach(boat{cap:Cap,crew:Crew},Hosts),
         foreach(Capacity,HostCapacity) do
            Capacity is Cap-Crew
        ).

guest_pattern(Guests,Height):-
        (foreach(boat{crew:Crew},Guests),
         foreach(Crew,Height) do
            true
        ).

boat(1 , 6 , 2).
boat(2 , 8 , 2).
boat(3 , 12 , 2).
boat(4 , 12 , 2).
boat(5 , 12 , 4).
boat(6 , 12 , 4).
boat(7 , 12 , 4).
boat(8 , 10 , 1).
boat(9 , 10 , 2).
boat(10 , 10 , 2).
boat(11 , 10 , 2).
boat(12 , 10 , 3).
boat(13 , 8 , 4).
boat(14 , 8 , 2).
boat(15 , 8 , 3).
boat(16 , 12 , 6).
boat(17 , 8 , 2).
boat(18 , 8 , 2).
boat(19 , 8 , 4).
boat(20 , 8 , 2).
boat(21 , 8 , 4).
boat(22 , 8 , 5).
boat(23 , 7 , 4).
boat(24 , 7 , 4).
boat(25 , 7 , 2).
boat(26 , 7 , 2).
boat(27 , 7 , 4).
boat(28 , 7 , 5).
boat(29 , 6 , 2).
boat(30 , 6 , 4).
boat(31 , 6 , 2).
boat(32 , 6 , 2).
boat(33 , 6 , 2).
boat(34 , 6 , 2).
boat(35 , 6 , 2).
boat(36 , 6 , 2).
boat(37 , 6 , 4).
boat(38 , 6 , 5).
boat(39 , 9 , 7).
boat(40 , 0 , 2).
boat(41 , 0 , 3).
boat(42 , 0 , 4 ).

% problem 1-9  are from Symmetry breaking paper
problem_data(1,[2,3,4,5,6,7,8,9,10,11,12,14,16]).
problem_data(2,[3,4,5,6,7,8,9,10,11,12,13,14,16]).
problem_data(3,[3,4,5,6,7,8,9,10,11,12,14,15,16]).
problem_data(4,[3,4,5,6,7,8,9,10,11,12,14,16,25]).
problem_data(5,[3,4,5,6,7,8,9,10,11,12,14,16,23]).
problem_data(6,[3,4,5,6,7,8,9,10,11,12,15,16,25]).
problem_data(7,[1,3,4,5,6,7,8,9,10,11,12,14,16]).
problem_data(8,[3,4,5,6,7,8,9,10,11,12,16,25,26]).
problem_data(9,[3,4,5,6,7,8,9,10,11,12,14,16,30]).
% this is my favorite host selection
problem_data(10,[1,2,3,4,5,6,7,8,9,10,11,12,14]).
% problems 11-16 are problems 1-6 from Van Hentenryck/Michel
problem_data(11,[1,2,3,4,5,6,7,8,9,10,11,12,16]).
problem_data(12,[1,2,3,4,5,6,7,8,9,10,11,12,13]).
problem_data(13,[1,3,4,5,6,7,8,9,10,11,12,13,19]).
problem_data(14,[3,4,5,6,7,8,9,10,11,12,13,25,26]).
problem_data(15,[1,2,3,4,5,6,7,8,9,10,11,19,21]).
problem_data(16,[1,2,3,4,5,6,7,8,9,16,17,18,19]).

problem_data(20,[3,4,5,6,7,8,9,10,11,12,16,39]).
