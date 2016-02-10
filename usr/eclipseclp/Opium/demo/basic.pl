/*
          B A S I C   P R O C E D U R E S     (in alphabetic order)
*/

% bind_up(List,ConStr,Result) connects the Strings in List by ConStr
% e.g. bind_up(["aa","bb"],"**","aa**bb")

bind_up([X],_,X).
bind_up([X|L],AscL,String) :-
    bind_up(L,AscL,SL),
    conc(AscL,SL,L1),
    conc(X,L1,String).

% clear_below(Line) clears the lines below Line

clear_below(Line) :-
   Line >= 24,
   !,
   clearline(24,1).

clear_below(Line) :-
   clearline(Line,1),
   NewLine is Line + 1,
   clear_below(NewLine).

complete_filename(Str,CompName) :-
    conc("/home/lp/ode/bugs/sepia/data/",Str,CompStr),
    name(CompName,CompStr).

conc(L1, L2, L3) :-
    append(L1, L2, L3).

% concatenation of two atoms
conc_atom(Atom1,Atom2,Atom3) :-
   concat_atoms(Atom1, Atom2, Atom3).

% concatenation of a list of atoms
/*
conc_atom_list(List,ConcAtom) :-
    concat_atom(List,ConcAtom).
*/
% concatenation of a list of strings

conc_str_list([],[]).
conc_str_list([X|L],ConcList) :-
    conc_str_list(L,ConcL),
    conc(X,ConcL,ConcList).

del(X,[X|Tail],Tail).
del(X,[Y|Tail],[Y|Tail1]) :-
    del(X,Tail,Tail1).

del_all_atts(cop,List,List) :-
    not(member([cop,X,Y],List)),
    !.
del_all_atts(cop,List,Result) :-
    del([cop,X,Y],List,List1),
    !,
    del_all_atts(cop,List1,Result).
del_all_atts(Attr,List,List) :-
    not(member([Attr,Val],List)),
    !.
del_all_atts(Attr,List,Result) :-
    del([Attr,Val],List,List1),
    !,
    del_all_atts(Attr,List1,Result).

empty([]).

error_message(Message,Line,Col,BackCol) :-
    clearline(Line,Col),
    write('    ** '),
%   write('*** error: '),
    write(Message),
    write(' **'),
    readln(X),
    clearline(Line,BackCol).

fill_blanks(Word,TotLength) :-
    name(Word,List),
    len(List,Length),
    Diff is TotLength - Length,
    f_blanks(Diff).

f_blanks(N) :-
    N =< 0,
    !.
f_blanks(N) :-
    write(' '),
    R is N - 1,
    f_blanks(R).

get_answer(Question,Answer,Line,Col) :-
    cursor(Line,Col),
    cleartoeol,
    write(Question),
    readln(Answer).

% head of a list is the list without its last element
head(List,Head) :-
  last(List,Head,Last). 

header(Title) :-
   clearpage,
   print_line(Title,1,1),
   print_dash(2).

insert(X,List,[X|List]).
insert(X,[Y|List],[Y|Tail]) :-
    insert(X,List,Tail).

intersection(Set,[],[]) :- !.
intersection([],Set,[]) :- !.
intersection([X|L],Set,[X|IS]) :-
    member(X,Set),
    !,
    intersection(L,Set,IS).
intersection([X|L],Set,IS) :-
    intersection(L,Set,IS).

% conversion of an integer to a string

int_to_str(Int,String) :-
  Int =< 9,
  !,
  X is Int + 48,
  String = [X].

int_to_str(Int,String) :-
  M is Int mod 10,
  D is Int - M,
  I is D / 10,
  int_to_str(I,SI),
  int_to_str(M,SM),
  conc(SI,SM,String).

% last(List,Head,Last)

last([X],[],X).
last([X|L],[X|HL],Last) :-
    last(L,HL,Last).

lastelem(List,Last) :-
  last(List,Head,Last).

len(List, N) :-
	length(List, N).

member(X,[X|Tail]).
member(X,[Head|Tail]) :-
    member(X,Tail).

next_elem([],X) :-
    !,
    fail.
next_elem([X|L],X).
next_elem([X|L],Z) :-
    next_elem(L,Z).

notempty_subset(Set,Subset) :-
    subset(Set,Subset),
    not(empty(Subset)).

% ordered(Entry,OrderedEntry)

ordered([DocType|ValList],[DocType|OrderedList]) :-
    doc_has_atts(DocType,AttrList),
    ordered(AttrList,ValList,OrderedList).

:- skipped(ordered/2).

ordered([],L,[]).
ordered([Attr|L],ValList,[[Attr,Val]|OrderedL]) :-
    member([Attr,Val],ValList),
    !,
    ordered(L,ValList,OrderedL).
ordered([Attr|L],ValList,OrderedL) :-
    ordered(L,ValList,OrderedL).


permutation([],[]).
permutation([X|L],PList) :-
    permutation(L,L1),
    insert(X,L1,PList).

print_line(Message,Line,Col) :-
    cursor(Line,Col),
    cleartoeol,
    write(Message).
       
print_dash(Line) :-
    D = '-------------------------------------------------------------------------------',
    print_line(D,Line,1).
   
% readln reads all the characters up to a LF

readln(Line) :-
    get(X),
    read_rest(X,Line).

read_rest(10,[]) :-
    !.
read_rest(34,[96|Line]) :-             % translates " into `
    !,
    readln(Line).
read_rest(39,[96|Line]) :-             % translate ' into `
    !,
    readln(Line).
read_rest(X,[X|Line]) :-
    readln(Line).

real_subset(Set,SubSet) :-
    subset(Set,SubSet),
    Set \== SubSet.

% replace_item([Attr,Val],List,ResList) will change 
% [Attr,OldVal] in List to [Attr,Val]

replace_item([Attr,Val],List,ResList) :-
    not(member([Attr,X],List)),
    !,
    conc([[Attr,Val]],List,ResList).
replace_item([Attr,Val],List,ResList) :-
    del([Attr,X],List,List1),
    conc([[Attr,Val]],List1,ResList).

% conversion of an integer to a string

str_to_int(Str,Int) :-
    strint(Str,0,Int).

strint([C],Fac,Int) :-
    !,
    stoi(C,V),
    Int is 10 * Fac + V.
strint([C|L],Fac,Int) :-
    stoi(C,V),
    NewFac is 10 * Fac + V,
    strint(L,NewFac,Int).

stoi(N,I) :-
    N =< 57,
    N >= 48,
    !,
    I is N - 48.

is_string(X) :-
    nonvar(X),
    X = [].
is_string([X|L]) :-
    is_string(L).

% subset(Set,Subset)

subset([],[]).
subset([X|L],[X|S]) :-
    subset(L,S).
subset([X|L],S) :-
    subset(L,S).

% up_to_char(<char>,"<str1><char><str2>","<str1>","<str2>")

up_to_char(Char,[X|L],[],L) :-
  X = Char,
  !.
up_to_char(Char,[X|L],[X|Head],Tail) :-
  up_to_char(Char,L,Head,Tail).


%  ----  display of entries ---------

standard_display(Result,NewLine) :-
    clearpage,
    ActLine = 4,
    Col = 5,
    cursor(ActLine,Col),
    write('Document type :  '),
    doc(DocType),
    cap_type_abbr(UcDocType,DocType),
    write(UcDocType),
    Line is ActLine + 2,
    disp(Result,Line,Col,NewLine).

standard_disp_copy(CopyRes,ActLine,NewLine) :-
    Col = 5,
    disp(CopyRes,ActLine,Col,NewLine).
 
disp([],Line,Col,Line).
disp([[Attr,'-']|AList],Line,Col,NewLine) :-
    !,
    disp(AList,Line,Col,NewLine).
disp([[Attr,Val]|AList],Line,Col,NewLine) :-
    cursor(Line,5),
    cap_attr_abbr(Attribute,Attr),
    write(Attribute),
    fill_blanks(Attribute,13),
    write(' :  '),
    write(Val),
    ActLine is Line + 1,
    disp(AList,ActLine,Col,NewLine).

standard_disp_kyws([kyw,KywList],ActLine,NewLine) :-
    Col1 = 5,
    Col2 = 18,
    clear_below(ActLine),
    print_line('Keywords :  ',ActLine,Col1),
    fill_line(KywList,ActLine,18,60,NewLine).

fill_line([],Line,X,Y,Line) :- !.
fill_line([Keyword|KL],Line,Col,Free,NewLine) :-
    length(Keyword,Length),
    Length =< Free,
    !,
    print_line(Keyword,Line,Col),
    NewCol is Col + Length + 4,
    StillFree is Free - Length - 4,
    fill_line(KL,Line,NewCol,StillFree,NewLine).
fill_line([Keyword|KL],Line,Col,Free,NewLine) :-
    length(Keyword,Length),
    Length > Free,
    NewL is Line + 1,
    fill_line([Keyword|KL],NewL,18,60,NewLine).



lob_build_lib_name(FileA, FullFile) :-
	get_flag(library_path, Path),
	atom_string(FileA, FileStr),
	member(LibDir, Path),
	(
		Fpl = FileStr
	;
		get_flag(prolog_suffix, SuffixList),
		member(Suffix, SuffixList),
		concat_strings(FileStr, Suffix, Fpl)
	),
	concat_strings(LibDir, "/", Path1),
	concat_strings(Path1, Fpl, FullFileStr),
	get_file_info(FullFileStr, mode, Mode),	
	8'40000 =\= Mode /\ 8'170000,	/* must not be a directory */
	atom_string(FullFile, FullFileStr).
