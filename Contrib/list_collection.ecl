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
% The Original Code is The List Collection for ECLiPSe.
% The Initial Developer of the Original Code is Lukasz Domagala.
% Portions created by the Initial Developer are Copyright (C) 2009.
% All Rights Reserved.


:- module(list_collection).

:- comment(categories, ["Data Structures"]).
:- comment(summary, "Collection of lists library").
:- comment(author, "Lukasz Domagala").
:- comment(copyright, "Silesian University of Technology").
:- comment(date, "$Date: 2009/07/16 09:11:23 $").
    
:- comment(desc,ascii("
    A library for creation and management of list collections (LCOL).
    Internally the library uses either the a hashtable or arrays to store lists and their tails. 
    The advantage of a hash LCOL is that its size does not have to be known in advance and that 
    non number indexes can be used to address lists in the collection. The drawback of a hash LCOL 
    is that access to collection elements is slower than for array LCOL. Elements of array LCOL 
    can be addressed only by positive integers which internally are array indexes. See create/2 
    for more details.
    
    The motivation for creating this library was to be able to easily build input lists for global 
    constraints from collections of input structures (see example). For this purpose an array(3) LCOL 
    was usually sufficient, and only a limited set of predicates needed (create/2 , append_element/3, 
    terminate_all_lists/2, get_list/3).
    
    But the idea was extended, additional predicates were added for prefixing the lists, and 
    reinitialisation of lists. The support for hash LCOL has been added. Hash LCOL may be useful for 
    some filtering, grouping predicates in cases when the number of lists may vary or indexing by 
    atoms/ground terms is more readable/convenient.

    ")).
:- comment(eg,"
llcol_example:-
    lib(ic),
    lib(ic_edge_finder),

    ActivityList=[
        act(4,7 ,2,machine_1),
        act(3,4 ,1,machine_2 ),
        act(2,4 ,2,removed),
        act(7,10,1,machine_1),
        act(6,15,3,machine_3)
        % ...
    ],
    
    %create a collection of activity lists for corresponding machines
    list_collection:create(hash,LCOL_Machines),
    
    %sort the activities for machines
    (foreach(Activity,ActivityList),
     param(LCOL_Machines) do 
        Activity=act(_Start,_End,_Resource,Machine),
        (Machine \\= removed ->
            %add an activity to the list for the particular machine
            list_collection:append_element(LCOL_Machines,Machine,Activity)
        ;true)
    ),
    
    %close the lists get the machine names
    list_collection:terminate_all_lists(LCOL_Machines,Machines_Indexes),
    
    %for each machine
    (foreach(Machines_Index,Machines_Indexes),
     param(LCOL_Machines) do 
        %Note that get_list/3 can be used even if the lists are not terminated but in such a case the operation
        %is more time consuming, because all the list elements are copied to new closed list and then returned.
        %Instead of closing all the lists at once it is also possible to use terminate_and_get_list/3 to ensure 
        %that the list is closed.
        
        %get the activity list for the machine
        list_collection:get_list(LCOL_Machines,Machines_Index,MActivityList),
        
        %create a collection for start,duration and resource variable lists
        list_collection:create(array(3),LCOL_Start_Dur_Res),
        
        (foreach(MActivity,MActivityList),
         param(LCOL_Start_Dur_Res) do
         
            MActivity=act(Start,End,Resource,_Machine),
            ic:(Duration #= End - Start),
            list_collection:append_element(LCOL_Start_Dur_Res, 1, Start),
            list_collection:append_element(LCOL_Start_Dur_Res, 2, Duration),
            list_collection:append_element(LCOL_Start_Dur_Res, 3, Resource)
        ),
        
        %terminate the start,duration,resource lists
        list_collection:terminate_all_lists(LCOL_Start_Dur_Res,_Indexes),
        
        %get the lists for a cumulative constraint
        list_collection:get_list(LCOL_Start_Dur_Res, 1, StartList),
        list_collection:get_list(LCOL_Start_Dur_Res, 2, DurationList),
        list_collection:get_list(LCOL_Start_Dur_Res, 3, ResourceList),

        %post the constraint
        ic_edge_finder:cumulative(StartList, DurationList, ResourceList, 3)
    ).
    ").

:- lib(hash).


:- local struct(list_collection(
        type,
        open_list_array,
        tail_array,
        list_tail_hash 
        )).

:- comment(create/2, 
    [
        amode:(create(++,-) is det),
        args:["Type":"A ground term", "LCOL":"A list collection"],
        summary:"Creates a list collection",
        desc:ascii("
        If (Type==hash) a LCOL internally uses a hashtable, this is useful when the number of lists is unknown.
        If (Type=array(Size)) where Size is a natural number, the LCOL internally uses an array to collect lists,
        the access to any of the lists is faster, but the Size has to be known in advance and only number 
        indexes can be used to identify lists. 0<Index=<Size."),
        %eg:"",
        see_also:[append_element/3,terminate_all_lists/2,get_list/3],
        %fail_if:"" ,
        exceptions:[
            5:"Not supported Type."
        ]
    ]).

:-export create/2.  
:- mode(create(++,-)).  
create(array(Size),LCOL):-
    LCOL=list_collection{
        type:array(Size), 
        open_list_array:OpenListArray,
        tail_array     :TailArray,
        list_tail_hash :not_used
        },
    number(Size),0<Size,
    !,
    dim(OpenListArray,[Size]),
    dim(TailArray,[Size]),
    (count(Index,1,Size),
     param(OpenListArray,TailArray) do 
        setarg(Index,OpenListArray,ListTail),
        setarg(Index,TailArray,ListTail)
    ),
    true.

create(hash, LCOL):-
    LCOL=list_collection{
        type:hash, 
        open_list_array:not_used,
        tail_array     :not_used,
        list_tail_hash :ListTailHash
        },
    !,
    hash_create(ListTailHash),
    true.
    
create(_Type, _LCOL):-
    exc_not_supported_type.

:- comment(append_element/3, 
    [
        amode:(append_element(+,++,+) is det),
        args:["LCOL":"A list collection", "Index":"A ground term or number",
            "Elem":"Term or variable"],
        summary:"Append an Elem to the internal list identified by Index.",
        desc:"Complexity O(1).",
        %eg:"",
        see_also:[append_list/3,prefix_element/3,prefix_list/3],
        %fail_if:"",
        exceptions:[
            5:"LCOL is not a list collection.",
            5:"Index is not a number or is out of range.",
            5:"Index is not ground.",
            1:"List identified by Index is closed."
            ]
    ]).
:-export append_element/3.  
:- mode(append_element(+,++,+)).
append_element(LCOL, Index, Elem):-
    check_lcol_exc(LCOL),
    check_index_exc(LCOL, Index),
    ensure_entry_exists_4_index(LCOL, Index, LTTerm),
    check_open_tail_exc(LCOL, Index, LTTerm),
    
    inner_tail_get(LCOL,Index,LTTerm, [Elem|NewTail]),
    inner_tail_set(LCOL,Index,LTTerm, NewTail),
    
    true.


    
:- comment(append_list/3, 
    [
        amode:(append_list(+,++,+) is det),
        args:["LCOL":"A list collection", "Index":"A ground term or number",
            "List":"A list of terms or variables"],
        summary:"Append a List to the internal list identified by Index.",
        desc:"Complexity O(|List|).",
        %eg:"",
        see_also:[append_element/3,prefix_element/3,prefix_list/3],
       % fail_if:"",
        exceptions:[
            5:"LCOL is not a list collection.",
            5:"Index is not a number or is out of range.",
            5:"Index is not ground.",
            1:"List identified by Index is closed."
            ]
    ]).
:-export append_list/3.
:- mode(append_list(+,++,+)).   
append_list(LCOL, Index, List):-
    check_lcol_exc(LCOL),
    check_index_exc(LCOL, Index),
    ensure_entry_exists_4_index(LCOL, Index,LTTerm),
    check_open_tail_exc(LCOL, Index,LTTerm),
    
    inner_tail_get(LCOL,Index,LTTerm,OldTail),
    
    (foreach(Elem,List),
     fromto(OldTail,[Elem|OldTailOut],OldTailOut,NewTail) do 
        true
    ),
    
    inner_tail_set(LCOL,Index,LTTerm,NewTail),

    true.
    
:- comment(prefix_element/3, 
    [
        amode:(prefix_element(+,++,+) is det),
        args:["LCOL":"A list collection", "Index":"A ground term or number",
            "Elem":"Term or variable"],
        summary:"Prefixes the internal list identified by Index with an Elem.",
        desc:" Complexity O(1)",
        %eg:"",
        see_also:[append_element/3,append_list/3,prefix_list/3],
       % fail_if:"",
        exceptions:[
            5:"LCOL is not a list collection.",
            5:"Index is not a number or is out of range.",
            5:"Index is not ground."
            ]
    ]). 
:- export prefix_element/3.
:- mode(prefix_element(+,++,+)).    
prefix_element(LCOL, Index, Elem):-
    prefix_list(LCOL, Index, [Elem]),
    true.
    

:- comment(prefix_list/3, 
    [
        amode:(prefix_list(+,++,+) is det),
        args:["LCOL":"A list collection", "Index":"A ground term or number",
            "List":"A list of terms or variables"],
        summary:"Prefixes the internal list identified by Index with a List.",
        desc:"Complexity O(|List|).",
        %eg:"",
        see_also:[append_element/3,append_list/3,prefix_list/3],
       % fail_if:"",
        exceptions:[
            5:"LCOL is not a list collection.",
            5:"Index is not a number or is out of range.",
            5:"Index is not ground."
            ]
    ]).     
:-export prefix_list/3.
:- mode(prefix_list(+,++,+)).   
prefix_list(LCOL, Index, List):-
    check_lcol_exc(LCOL),
    check_index_exc(LCOL, Index),
    ensure_entry_exists_4_index(LCOL, Index,LTTerm),

    (foreach(Elem,List),
     fromto(NewList,[Elem|NewListOut],NewListOut,NewListTail) do 
        true
    ),
    
    inner_list_get(LCOL,Index,LTTerm,OldList),
    inner_list_set(LCOL,Index,LTTerm,NewList),
    NewListTail = OldList,
    true.
    
    
:- comment(get_list/3, 
    [
        amode:(get_list(+,++,-) is det),
        args:["LCOL":"A list collection", "Index":"A ground term or number",
            "List":"A list of terms"],
        summary:"Retrieves the list of elements identified by Index",
        desc:"If the internal list identified by Index is open, the elements from the list are copied to a closed List and returned. The internal list remains open. Complexity O(|List|).
            If the internal list identified by Index is closed, the internal list is returned immediately as List. Complexity O(1).",
        %eg:"",
        see_also:[terminate_all_lists/2,terminate_and_get_list/3],
        %fail_if:"" ,
        exceptions:[
            5:"LCOL is not a list collection.",
            5:"Index is not a number or is out of range.",
            5:"Index is not ground."
            ]
    ]).     
:-export get_list/3.    
:- mode(get_list(+,++,-)).      
get_list(LCOL, Index, List):-
    check_lcol_exc(LCOL),
    check_index_exc(LCOL, Index),
    ensure_entry_exists_4_index(LCOL, Index,LTTerm),
    
    inner_list_get(LCOL,Index,LTTerm,OpenList),
    inner_tail_get(LCOL,Index,LTTerm,OpenListTail),
    
    (OpenListTail==[] ->
        %if the list is closed there is no need to copy the elements, just get list
        List = OpenList
    ;
        (free(OpenList)->
            %empty list
            List=[]
        
        ;
            (fromto(OpenList,[Elem|OpenListNext],OpenListOut,[]),
             foreach(Elem,List),
             param(OpenListTail) do
                (OpenListNext\==OpenListTail->
                    OpenListOut = OpenListNext
                ;
                    OpenListOut=[]
                )
            )
        )
    ),
    true.
    
:- comment(terminate_and_get_list/3, 
    [
        amode:(terminate_and_get_list(+,++,-) is det),
        args:["LCOL":"A list collection", "Index":"A ground term or number",
            "List":"A list of terms"],
        summary:"Retrieves the List of elements identified by Index",
        desc:"Ensures that the internal List of elements identified by Index is terminated, and returns the internal List. Complexity O(1).",
        %eg:"",
        see_also:[get_list/3, terminate_all_lists/2,reinit/2],
       % fail_if:"" ,
        exceptions:[
            5:"LCOL is not a list collection.",
            5:"Index is not a number or is out of range.",
            5:"Index is not ground."
            ]
    ]).     
:-export terminate_and_get_list/3.
:- mode(terminate_and_get_list(+,++,-)).    
terminate_and_get_list(LCOL, Index, List):-
    check_lcol_exc(LCOL),
    check_index_exc(LCOL, Index),
    ensure_entry_exists_4_index(LCOL, Index,LTTerm),
    
    inner_tail_get(LCOL,Index,LTTerm,[]),   
    inner_list_get(LCOL,Index,LTTerm,List),
    true.

:- comment(terminate_all_lists/2, 
    [
        amode:(terminate_all_lists(+,-) is det),
        args:["LCOL":"A list collection", "Indexes":"List of indexes"],
        summary:"Terminates all internal lists in the collection, and gets the list of indexes",
        desc:"Complexity O(ListCount).",
        %eg:"",
        see_also:[get_list/3,terminate_and_get_list/3,reinit/2,get_indexes/2],
        %fail_if:"" 
        exceptions:[
            5:"LCOL is not a list collection."
        ]
    ]). 
    
    
:-export terminate_all_lists/2.
:- mode(terminate_all_lists(+,-)).  
terminate_all_lists(LCOL,Indexes):-
    check_lcol_exc(LCOL),
    LCOL=list_collection{
        type:hash, 
        open_list_array:_OpenListArray,
        tail_array     :_TailArray,
        list_tail_hash :ListTailHash
        },
    !,
    hash_list(ListTailHash,IndexesUnsorted,LTTermLi),
    sort(IndexesUnsorted,Indexes),
    (foreach(list_tail(_,[]),LTTermLi) do 
        true
    ),
    true.

terminate_all_lists(LCOL,Indexes):-
    check_lcol_exc(LCOL),
    LCOL=list_collection{
        type:array(_Size), 
        open_list_array:_OpenListArray,
        tail_array     :TailArray,
        list_tail_hash :_ListTailHash
        },
    !,
    (foreacharg([],TailArray,Index),
     foreach(Index,Indexes) do 
        true
    ),
    true.
    
:- comment(get_indexes/2, 
    [
        amode:(get_indexes(+,-) is det),
        args:["LCOL":"A list collection", "Indexes":"List of indexes"],
        summary:"Retrieves the list of indexes",
        desc:"Complexity O(ListCount).",
        %eg:"",
        see_also:[get_list/3,terminate_and_get_list/3,terminate_all_lists/2],
        %fail_if:"" 
        exceptions:[
            5:"LCOL is not a list collection."
        ]
    ]). 
    
    
:-export get_indexes/2.
:- mode(get_indexes(+,-)).  
get_indexes(LCOL,Indexes):-
    check_lcol_exc(LCOL),
    LCOL=list_collection{
        type:hash, 
        open_list_array:_OpenListArray,
        tail_array     :_TailArray,
        list_tail_hash :ListTailHash
        },
    !,
    hash_list(ListTailHash,IndexesUnsorted,_LTTermLi),
    sort(IndexesUnsorted,Indexes),
    true.

get_indexes(LCOL,Indexes):-
    check_lcol_exc(LCOL),
    LCOL=list_collection{
        type:array(_Size), 
        open_list_array:_OpenListArray,
        tail_array     :TailArray,
        list_tail_hash :_ListTailHash
        },
    !,
    (foreacharg(_Tail,TailArray,Index),
     foreach(Index,Indexes) do 
        true
    ),
    true.



    
:- comment(reinit/2, 
    [
        amode:(reinit(+,++) is det),
        args:["LCOL":"A list collection", "Index":"A ground term or number"],
        summary:"Creates a new list identified by Index",
        desc:"Discards the internal list of elements identified by Index, and replaces with a new list. Complexity O(1).",
        %eg:"",
        see_also:[terminate_all_lists/2,terminate_and_get_list/3],
        %fail_if:"" ,
        exceptions:[
            5:"LCOL is not a list collection.",
            5:"Index is not a number or is out of range.",
            5:"Index is not ground."
            ]
    ]).     
:-export reinit/2.
:- mode(reinit(+,++)).
reinit(LCOL, Index):-
    check_lcol_exc(LCOL),
    !,
    check_index_exc(LCOL, Index),
    ensure_entry_exists_4_index(LCOL, Index,LTTerm),
    inner_list_set(LCOL,Index,LTTerm,ListTail),
    inner_tail_set(LCOL,Index,LTTerm,ListTail),
    true.
    
inner_tail_get(LCOL,Index,LTTerm,Tail):-
    LCOL=list_collection{
        type:Type, 
        open_list_array:_OpenListArray,
        tail_array     :TailArray,
        list_tail_hash :_ListTailHash
        },
    (Type == hash, not free(LTTerm) ->
        LTTerm = list_tail(_OpenList,Tail)
    ;
        (instance(Type,array(_Size))->
            arg(Index,TailArray,Tail)
        ;
            exc_not_supported_type
        )
    ).
    
inner_tail_set(LCOL,Index,LTTerm,Tail):-
    LCOL=list_collection{
        type:Type, 
        open_list_array:_OpenListArray,
        tail_array     :TailArray,
        list_tail_hash :_ListTailHash
        },
    (Type == hash,not free(LTTerm) ->
        setarg(2,LTTerm,Tail)
    ;
        (instance(Type,array(_Size))->
            setarg(Index,TailArray,Tail)
        ;
            exc_not_supported_type
        )
    ).
inner_list_get(LCOL,Index,LTTerm,OpenList):-
    LCOL=list_collection{
        type:Type, 
        open_list_array:OpenListArray,
        tail_array     :_TailArray
        },
    (Type == hash,not free(LTTerm) ->
        LTTerm= list_tail(OpenList,_Tail)
    ;
        (instance(Type,array(_Size))->
            arg(Index,OpenListArray,OpenList)
        ;
            exc_not_supported_type
        )
    ).
inner_list_set(LCOL,Index,LTTerm,OpenList):-
    LCOL=list_collection{
        type:Type, 
        open_list_array:OpenListArray,
        tail_array     :_TailArray,
        list_tail_hash :_ListTailHash
        },
    (Type == hash,not free(LTTerm) ->
        setarg(1,LTTerm,OpenList)
    ;
        (instance(Type,array(_Size))->
            setarg(Index,OpenListArray,OpenList)
        ;
            exc_not_supported_type
        )
    ).

    
ensure_entry_exists_4_index(LCOL, Index,LTTerm):-
    LCOL=list_collection{
        type:hash, 
        open_list_array:_OpenListArray,
        tail_array     :_TailArray,
        list_tail_hash :ListTailHash
        },
    !,
    
    (hash_contains(ListTailHash,Index)->
        hash_get(ListTailHash,Index,LTTerm)
    ;
        functor(LTTerm,list_tail,2),
        setarg(1,LTTerm,ListTail),
        setarg(2,LTTerm,ListTail),
        hash_add(ListTailHash,Index,LTTerm)
    ),
    true.   
    
ensure_entry_exists_4_index(LCOL, _Index,_LTTerm):-
    LCOL=list_collection{
        type:array(_Size), 
        open_list_array:_OpenListArray,
        tail_array     :_TailArray
        },
    !,
    true.   

check_index_exc(LCOL, Index):-  
    LCOL=list_collection{
        type:hash, 
        open_list_array:_OpenListArray,
        tail_array     :_TailArray,
        list_tail_hash :_ListTailHash
        },
    (ground(Index)->
        true
    ;
        exc_index_is_not_ground
    ),!.
    
check_index_exc(LCOL, Index):-  
    LCOL=list_collection{
        type:array(Size), 
        open_list_array:_OpenListArray,
        tail_array     :_TailArray,
        list_tail_hash :_ListTailHash
        },
    (number(Index),0<Index,Index=<Size->
        true
    ;
        exc_index_is_not_a_num_or_out_of_range
    ),!.
    
check_open_tail_exc(LCOL, Index,LTTerm):-
    inner_tail_get(LCOL,Index,LTTerm,Tail),
    (free(Tail)->
        true
    ;
        exc_list_is_closed
    ).
    
check_lcol_exc(LCOL):-
    (instance(LCOL,list_collection{}),
     LCOL=list_collection{
        type:Type, 
        open_list_array:_OpenListArray,
        tail_array     :_TailArray,
        list_tail_hash :_ListTailHash
        },
        ground(Type) ->
        
        true
    ;
        exc_lcol_is_not_a_proper_structure
    ),
    true.
    
exc_lcol_is_not_a_proper_structure:-
    writeln(error,"LCOL is not a list collection."),
    exit_block(lcol_exception).
    
exc_not_supported_type:-
    writeln(error,"Not supported Type."),
    exit_block(lcol_exception).
    
exc_list_is_closed:-
    writeln(error,"List identified by Index is closed."),
    exit_block(lcol_exception).
    
exc_index_is_not_a_num_or_out_of_range:-
    writeln(error,"Index is not a number or is out of range."),
    exit_block(lcol_exception).
    
exc_index_is_not_ground:-
    writeln(error,"Index is not ground."),
    exit_block(lcol_exception).


%----------------------------------------------------------------------
end_of_file.
%----------------------------------------------------------------------

%just for testing if documentation is generated properly
lcol_test_generate_doc:-
    get_flag(cwd, Cwd),
    concat_string([Cwd,"list_collection.ecl"],File),
    document:icompile(File),
    document:eci_to_html("list_collection.eci", Cwd, ""),

    true.

%self testing of the library, the predicate has to be true
lcol_self_test:-
    block(
        lcol_self_test_lcol_type(wrong_type)
    ,lcol_exception, true),
    
    lcol_self_test_lcol_type(hash),
    lcol_self_test_lcol_type(array(3)),
    true.
    
lcol_self_test_lcol_type(LCOL_Type):-
    %Create a Collection Of Lists 
        list_collection:create(LCOL_Type,LCOL),
    
    %appending elements to List1 and List2
        list_collection:append_element(LCOL, 1, list_1_elem_1),
        list_collection:append_element(LCOL, 1, list_1_elem_2),
        list_collection:append_element(LCOL, 2, list_2_elem_1),
        list_collection:append_element(LCOL, 2, list_2_elem_2),
    
    %prefixig the List1 by an element
        list_collection:prefix_element(LCOL, 1, list_1_pref_1),
    %prefixing  the List2 by a list of elements
        list_collection:prefix_list(LCOL,2,[list_2_pref_2,list_2_pref_1]),
    %appending a list of alements to List3
        list_collection:append_list(LCOL,3,[list_3_elem_1,list_3_elem_2]),
    
    %getting the List1 elements without closing the internal List1
        get_list(LCOL,1,List1Elements),
    List1Elements = [list_1_pref_1,list_1_elem_1,list_1_elem_2],

    %still possible to append to List1 because it is not closed
        list_collection:append_element(LCOL, 1, list_1_elem_3),
    %terminating the List1 and retrieving it
        terminate_and_get_list(LCOL, 1, List1),
    List1 == [list_1_pref_1,list_1_elem_1,list_1_elem_2,list_1_elem_3],
    block(
        %the following would cause an exception because the List1 is closed 
        (
            list_collection:append_element(LCOL, 1, list_1_elem_4),
            fail
        )
    ,lcol_exception, true),
    %but it is possible to prefix the List1
        list_collection:prefix_element(LCOL, 1, list_1_pref_2),
        terminate_and_get_list(LCOL, 1, List1Prefixed),
    List1Prefixed == [list_1_pref_2,list_1_pref_1,list_1_elem_1,list_1_elem_2,list_1_elem_3],
    %internally List1 is discarded and a new List1  is created
        reinit(LCOL,1),
        get_list(LCOL,1,List1NewElements),
    List1NewElements == [],
        list_collection:append_element(LCOL, 1, list_1_elem_1),
        terminate_and_get_list(LCOL, 1, List1New),
    List1New == [list_1_elem_1],
    block(
        %the following would cause an exception but only for the array(3) LCOL
        %because the index is out of range
        (LCOL_Type == array(3) ->
            terminate_and_get_list(LCOL, 4, _List4Treminated),
            fail
        ;true)
        %for a hash LCOL it would not cause an exception,  
        %a new list for index 4 would be created closed and returned
    ,lcol_exception, true),
    
    %terminate all the lists
        terminate_all_lists(LCOL,Indexes),
    Indexes = [1,2,3],
    %check the get_indexes
        get_indexes(LCOL,Indexes),
    %after all the lists are terminated get_list/3 does not need to copy the elements 
    %to a new terminated list as above, it just gives back the internal lists  
        get_list(LCOL, 2, List2Treminated),
    List2Treminated == [list_2_pref_2,list_2_pref_1,list_2_elem_1,list_2_elem_2],
        get_list(LCOL, 3, List3Treminated),
    List3Treminated == [list_3_elem_1,list_3_elem_2],
        
    true.
    
%example
lcol_example:-
    lib(ic),
    lib(ic_edge_finder),

    ActivityList=[
        act(4,7 ,2,machine_1),
        act(3,4 ,1,machine_2 ),
        act(2,4 ,2,removed),
        act(7,10,1,machine_1),
        act(6,15,3,machine_3)
        % ...
    ],
    
    %create a collection of activity lists for corresponding machines
    list_collection:create(hash,LCOL_Machines),
    
    %sort the activities for machines
    (foreach(Activity,ActivityList),
     param(LCOL_Machines) do 
        Activity=act(_Start,_End,_Resource,Machine),
        (Machine \= removed ->
            %add an activity to the list for the particular machine
            list_collection:append_element(LCOL_Machines,Machine,Activity)
        ;true)
    ),
    
    %close the lists get the machine names
    list_collection:terminate_all_lists(LCOL_Machines,Machines_Indexes),
    
    %for each machine
    (foreach(Machines_Index,Machines_Indexes),
     param(LCOL_Machines) do 
        %Note that get_list/3 can be used even if the lists are not terminated but in such a case the operation
        %is more time consuming, because all the list elements are copied to new closed list and then returned.
        %Instead of closing all the lists at once it is also possible to use terminate_and_get_list/3 to ensure 
        %that the list is closed.
        
        %get the activity list for the machine
        list_collection:get_list(LCOL_Machines,Machines_Index,MActivityList),
        
        %create a collection for start,duration and resource variable lists
        list_collection:create(array(3),LCOL_Start_Dur_Res),
        
        (foreach(MActivity,MActivityList),
         param(LCOL_Start_Dur_Res) do
         
            MActivity=act(Start,End,Resource,_Machine),
            ic:(Duration #= End - Start),
            list_collection:append_element(LCOL_Start_Dur_Res, 1, Start),
            list_collection:append_element(LCOL_Start_Dur_Res, 2, Duration),
            list_collection:append_element(LCOL_Start_Dur_Res, 3, Resource)
        ),
        
        %terminate the start,duration,resource lists
        list_collection:terminate_all_lists(LCOL_Start_Dur_Res,_Indexes),
        
        %get the lists for a cumulative constraint
        list_collection:get_list(LCOL_Start_Dur_Res, 1, StartList),
        list_collection:get_list(LCOL_Start_Dur_Res, 2, DurationList),
        list_collection:get_list(LCOL_Start_Dur_Res, 3, ResourceList),

        %post the constraint
        ic_edge_finder:cumulative(StartList, DurationList, ResourceList, 3)
    ).