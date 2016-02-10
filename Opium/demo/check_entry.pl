% MISSING33

:- module(check_entry).


getbug :-
	writeln('\nCheck that you are in "module(check_entry).", then'),
	writeln('to start the program type "test_check_entry.".\n').

test_check_entry :- 
    open('/home/lp/ode/bugs/sepia/data/incorrect',write,WriteStream2),
    setval(write_stream_2, WriteStream2),
    open('/home/lp/ode/bugs/sepia/data/rubbish',write,WriteStream3),
    setval(write_stream_3, WriteStream3),
    open('/home/lp/ode/bugs/sepia/data/database',write,WriteStream4),
    setval(write_stream_4, WriteStream4),
    check_entry([bok, [aut,[69, 46, 89, 46, 32, 83, 104, 97, 112, 105, 114, 111]], 
      [key,[83, 104, 97, 112, 105, 114, 111]], 
      [yea,[49, 57, 56, 54]], 
      [tit, [65, 100, 118, 97, 110, 99, 101, 100, 32, 80, 114, 111, 103, 114, 97, 109, 109, 105, 110, 103, 32, 105, 110, 32, 80, 114, 111, 108, 111, 103]], 
      [pub,[65, 100, 100, 105, 115, 111, 110, 32, 87, 101, 115, 108, 101, 121]]]),
    close(WriteStream2),
    close(WriteStream3),
    close(WriteStream4).


:- lib(porting).
:-set_all_strings_to_lists.

:- lib(basic).
:- lib(facts.
:- lib(screen).


bug :- 
	nl,
	explanation.

explanation :-	
writeln(' \n \
Missing base clause for clean_val/2, therefore predicate always fails, \n \
and the treatment of the bibliography entry is not finished. \n \
 \n \
The same program with the same bug appears in side13. \n \
Here the test has been abstracted by hand. \n \
 \n \
GOAL:	 check_entry(...) \n \
CORRECT: display correct bibliography entry \n \
BUGGY:   no (more) solution \n \
').

% ============================================================================

% --- test --- 5.7.89

insert(Entry) :-
	nl, writeln('Entry:'), writeln(Entry), nl.


/*                         UPDATE OF THE DATABASE                


A)  interactive input help
B)  input checker
C)  erase/modify/add (Key)
D)  translation of Scribe bibliography files into LOB files

---------------

A)  INTERACTIVE INPUT HELP  

build_file will help the user to build a file of future database entries 
which can be read by the input checker. For each document which shall be 
entered the user is asked for the mandatory or optional attributes with 
respect to the document's type.

Representation of a document in the file:

     [bok,[aut,"L. Sterling, E.Y. Shapiro"],
          [tit,"The Art of Prolog - Advanced Programming Techniques"],
          [key,"Sterling"],[yea,"1986"],
          [pub,"MIT Press"],[ser,"Logic programming"],
          [kyw,"logic programming"],[kyw,"prolog"],
          [cop,"library","library"],
          [cop,"library","mireille"]].

*/

% =============================================== main program ================

build_file :-
    clearpage,
    get_answer('Please enter filename : ',Fname,2,5),
    complete_filename(Fname,Filename),
    open(Filename,write,10),		% open(Filename,10,w,2), what is the 2 for ??
    setval(write_stream_1, WriteStream1),
    build_file(Filename),
    close(WriteStream1).

build_file(Filename) :-
    clearpage,
    process_date,
    process_file(Filename).

process_file(Filename) :-
    repeat,
       cursor(2,5),
       write('Document type :  '),
       readln(Input),
       checkinp_1(Input,CheckedInp),
       process_doc(CheckedInp,Filename,4).

process_doc(no,Filename,Line) :-
    !,
    clearpage,
    cursor(3,5),
    write('File '),
    write(Filename),
    write(' has been written.'),
    nl,nl.

process_doc(wronginp,Filename,Line) :-
    NewL1 is Line - 2,
    error_message('Wrong input.',NewL1,40,21),
    fail.

process_doc(DocType,Filename,Line) :-
    prompted_atts(DocType,AttrList),                                 
    askfor_atts(AttrList,ValueList,Line,XLine),
    write_entry([DocType|ValueList]),
    clearpage,
    fail.

% askfor_atts will prompt the user to enter the values of the attributes

askfor_atts([Attr],Res,ActLine,NewLine) :-
    !,
    askfor_att(Attr,Res,ActLine,NewLine).

askfor_atts([Attr|AttrList],ValueList,ActLine,NewLine) :-
    askfor_att(Attr,Res,ActLine,ActL),
    askfor_atts(AttrList,ResList,ActL,NewLine),
    conc(Res,ResList,ValueList).

% Entry of (several) keywords: ResList = [[kyw,"logic"],[kyw,"Prolog"]]

askfor_att([kyw,o],ResList,ActLine,NewLine) :-
    !,
    Line is ActLine + 1,
    askfor_att_k(ResList,Line,NewLine).
askfor_att([cop,o],ResList,ActLine,NewLine) :-
    !,
    Line is ActLine + 1,
    askfor_att_c(ResList,Line,NewLine).
askfor_att([Attr,R],Res,ActLine,NewLine) :-
    !,
    cap_attr_abbr(Attribute,Attr),
    clearline(ActLine,5),
    write(Attribute),
    fill_blanks(Attribute,13),
    write(' :  '),
    receive(Attr,R,Res,ActLine,NewLine,22).

askfor_att_k(ResList,ActLine,NewLine) :-
    askfor_kyw(Res,ActLine,ActL),
    process_kyw(Res,ResList,ActL,NewLine).

process_kyw([],[],ActLine,ActLine) :-
    !.

process_kyw(Res,ResList,ActLine,NewLine) :-
    askfor_att_k(Rlist,ActLine,NewLine),
    conc(Res,Rlist,ResList).

askfor_kyw(Res,ActLine,NewLine) :-
    cap_attr_abbr(Attribute,kyw),
    clearline(ActLine,5),
    write(Attribute),
    fill_blanks(Attribute,13),
    write(' :  '),
    receive(kyw,o,Res,ActLine,NewLine,22).

% Entry of copies: 
% ResList = [[cop,"library","library"],[cop,"library","anna"]]


askfor_att_c(ResList,ActLine,NewLine) :-
    askfor_att([own,o],Res,ActLine,ActL),
    process_cop(Res,ResList,ActL,NewLine).

process_cop([],[],ActLine,ActLine) :- !.

process_cop([[Attr,Owner]],ResList,ActLine,NewLine) :-
    askfor_att([loc,m],[[loc,Location]],ActLine,ActL),
    process_nxtcop(RList,ActL,NewLine),
    conc([[cop,Owner,Location]],RList,ResList).

process_nxtcop(ResList,ActLine,NewLine) :-
    askfor_att([own,o],Own,ActLine,ActL),
    process_own(Own,ResList,ActL,NewLine).

process_own([],[],ActLine,ActLine) :-
    !.

process_own([own,Owner],ResList,ActLine,NewLine) :-
    askfor_att([loc,m],[[loc,Location]],ActLine,ActL2),
    process_nxtcop(RList,ActL2,NewLine),
    conc([[cop,Owner,Location]],RList,ResList).

% Entry of remaining attributes (which may have one value at most)


             

% ---------------------
% input check functions
% ---------------------

checkinp_1("",no) :- !.
checkinp_1("no",no) :- !.
checkinp_1("n",no) :- !.
checkinp_1("e",no) :- !.
checkinp_1("x",no) :- !.
checkinp_1("exit",no) :- !.

checkinp_1(X,Y) :-
    name(Z,X),
    type_abbr(Z,Y),
    !.

checkinp_1(X,Y) :-
    name(Y,X), 
    type_abbr(_,Y),
    !.

checkinp_1(_,wronginp).


checkinp_2(_,o,[],[],ActLine,ActLine) :-
    !,
    clearline(ActLine,5).
checkinp_2(yea,m,[],Result,ActLine,ActLine) :-
    !,
    error_message('Attribute mandatory!',ActLine,40,22),
    fail.
checkinp_2(yea,R,Value,Result,ActLine,NewLine) :-
    !,
    str_to_int(Value,Int),
    check_year(Int,Result,Value,R,ActLine,NewLine).
checkinp_2(Attr,m,[],Res,ActLine,ActLine) :-
    !,
    error_message('Attribute mandatory!',ActLine,40,22),
    fail.
checkinp_2(Attr,R,Value,[[Attr,Value]],ActLine,NewLine) :- 
    check_length(Attr,Value),
    !,
    NewLine is ActLine + 1.
checkinp_2(Attr,R,Value,Result,ActLine,ActLine) :- 
    !,
    error_message('String too long!',ActLine,40,22),
    fail.

check_year(Int,[[yea,Result]],Result,R,ActLine,NewLine) :-
    Int >= 1900,
    Int =< 3000,
    !,
    NewLine is ActLine + 1.

check_year(Int,Res,Result,R,ActLine,ActLine) :-
    error_message('4 digits, please!',ActLine,40,22),
    fail.


check_length(Attr,Value) :-
    len(Value,ActLength),
    attr_len(Attr,MaxLength),
    ActLength =< MaxLength.

% --------------
% subprocedures
% --------------

process_date :-
    repeat,
      (    get_answer('Please enter today`s date (DD.MM.YY) : ',Date,2,5),
               check_date(Date), 
	       getval(write_stream_1, WriteStream1),      
               write(WriteStream1,Date),
               writeln(WriteStream1,'. '),
               clearpage
      ;
               error_message('Wrong format!',2,43,5),
               fail
      ).

check_date(String) :-
    up_to_char(46,String,Day,Rest),
    up_to_char(46,Rest,Month,Year),
    str_to_int(Day,IDay),
    1 =< IDay,
    IDay =< 31,
    str_to_int(Month,IMonth),
    1 =< IMonth,
    IMonth =< 12,
    str_to_int(Year,IYear),
    0 =< IYear,
    IYear =< 99.


receive(Attr,R,Res,ActLine,NewLine,FromCol) :-
    put_dot(Attr,ActLine,22),
    readln(Value),
    checkinp_2(Attr,R,Value,Res,ActLine,NewLine),
    !.

receive(Attr,R,Res,ActLine,NewLine,FromCol) :-
    receive(Attr,R,Res,ActLine,NewLine,FromCol).

write_entry(Entity) :-
    getval(write_stream_1, WriteStream1),
    write(WriteStream1,Entity),
    writeln(WriteStream1,'. ').

put_dot(Attr,Line,Col) :-
    !,
    attr_len(Attr,Length),
    Col2 is Length + Col,
    dot(Line,Col2),
    cursor(Line,Col).

dot(Line,Col) :-
    Col =< 80,
    !,
    cursor(Line,Col),
    write('.').

dot(Line,Col).

/* ------------------

B)  INPUT CHECKER

check_input will read new references from a file (written in a format like
the output of build_file) and check whether there are values for all the 
mandatory attributes, whether the values are not too long, and whether there 
are entries in the database that are similar to the new one (i.e. have the
same key). Correct references are directly inserted into the database.
Incorrect references and those which are suspected to be already contained
in the database are collected in a  file 'incorrect'. These references
are presented to the moderator after the first check, s/he has the possibility
to correct/reject them.

*/

check_input :-
    clearpage,
    get_answer('Please enter filename :  ',Input,4,10),
    complete_filename(Input,Filename),
    open(Filename,read,ReadStream1),
    setval(read_stream_1, ReadStream1),
    check_input(Filename),
    close(ReadStream1).

check_input(Filename) :-
    open('/home/lp/ode/bugs/sepia/data/incorrect',write,WriteStream2),
    setval(write_stream_2, WriteStream2),
    open('/home/lp/ode/bugs/sepia/data/rubbish',write,WriteStream3),
    setval(write_stream_3, WriteStream3),
    open('/home/lp/ode/bugs/sepia/data/database',write,WriteStream4),
    setval(write_stream_4, WriteStream4),
    cursor(7,10),
    write('checking file '),
    write(Filename),
    write(' ...'),
    nl,nl,
    getval(read_stream_1, ReadStream1),
    read(ReadStream1,Date),
    (    check_date(Date),
            !
    ;
            write('Format of date is wrong / date is missing!'),
            nl,
            close(ReadStream1),
            close(WriteStream2),
            close(WriteStream3)
    ),
    assert(dat(Date)),
    assert(double([])),
    check_file,
    retract(double(List1)),
    close(WriteStream2),
    close(WriteStream3),
    !,
    open('/home/lp/ode/bugs/sepia/data/incorrect',read,ReadStream2),
    setval(read_stream_2, ReadStream2),
    assert(double([])),
    correct_entries,
    close(ReadStream2),
%  ---- test only -----
%    close(WriteStream4),
%  --------------------
    retract(dat(Date)),
    retract(double(List2)).


% --------------------------------
%        C H E C K   F I L E
% --------------------------------

check_file :-
    repeat,
       getval(read_stream_1, ReadStream1),
       read(ReadStream1,Entry),
       check_entry(Entry).

check_entry(end_of_file) :- !.

check_entry(Entry) :-
    usable(Entry),
    !,
    clean(Entry,CEntry),                     
    process_entry(CEntry).

check_entry(Entry) :-
    getval(write_stream_3, WriteStream3),
    write(WriteStream3,Entry),
    writeln(WriteStream3,'. '),
    fail.

process_entry(E) :-
    E = [DocType|ValList],
    member([key,Key],ValList),
    write(Key),nl,
    add_ide(E,Entry),
    !,
    proc_entry(Entry).

process_entry(Entry) :-
    write_errdoc(noide,Entry,[[key,m],[yea,m]]),
    !,
    fail.

proc_entry(Entry) :-
    double(Entry,Similars),
    !,
    first_double(Entry,N),
    write_errdoc(double,Entry,[N,Similars]),
    fail.

proc_entry(Entry) :-
    val_too_long(Entry,Attr),
    !,
    write_errdoc(toolong,Entry,Attr),
    fail.

proc_entry(Entry) :-
    val_missing(Entry,Attr),
    !,
    write_errdoc(missing,Entry,Attr),
    fail.

proc_entry(Entry) :-
    enter(Entry),
    !,
    fail.

double(_,_) :- !, fail.
double([DocType|ValList],Similars) :-
    member([ide,Ide],ValList),
    bagof(Result,
          general_search([[ide,m,Ide]],[ide,aut,tit],[DocType],Result),
          Similars).

% check each attribute for a string that is too long
val_too_long([DocType|ValList],TooLong) :-
    too_long(ValList,TooLong).

% check whether a mandatory attribute is missing
val_missing([DocType|ValList],Attr) :-
    mandatory_atts(DocType,AttList),
    check_mand_atts(AttList,ValList,Attr).

% add document identifier
add_ide([DocType|ValList],[DocType|ComplList]) :-
    generate_ide(ValList,Ide),
    conc([[ide,Ide]],ValList,ComplList).

generate_ide(ValList,Ide) :-
    member([key,Key],ValList),
    len(Key,Length),
    Length =< 17,
    first_nonabbr(Key,HeadKey),
    member([yea,Year],ValList),
    str_to_int(Year,Num),
    Num >= 1900,
    Num =< 3000,
    ending(Year,EndYear),
    conc(HeadKey,EndYear,Ide).

% ---------------------------
% process correct(ed) entries
% ---------------------------

enter([]) :- !.

enter(Entry) :-
    add_date(Entry,ComplEntry),
    enter_doc(ComplEntry),
    enter_copies(ComplEntry),
    enter_kyws(ComplEntry).

add_date([DT|List],[DT|ResList]) :-
    dat(Date),
    conc([[dat,Date]],List,ResList).

enter_doc(ComplEntry) :-
    build_docentry(ComplEntry,DocEntry),
    insert_docentry(DocEntry).

build_docentry([DocType|ValList],[DocType|CheckedList]) :-
    rel_has_atts(DocType,AttList),
    check_atts(AttList,ValList,CheckedList).    

enter_copies([DocType|ValList]) :-
    member([ide,Ide],ValList),
    dat(Date),
    collect(cop,ValList,Copies),
    insert_copies(DocType,Ide,Date,Copies).

enter_kyws([DocType|ValList]) :-
    member([ide,Ide],ValList),
    collect(kyw,ValList,KywList),
    insert_kyws(DocType,Ide,KywList).

insert_docentry([DocType|ValList]) :-
    conc_atom('insert_',DocType,ProcName),
    strings_to_atoms_list(ValList,ValAtoms),
    InsertProcess =.. [ProcName|[ValAtoms]],
    call(InsertProcess).

insert_copies(DocType,Ide,Date,[]) :- !.

insert_copies(DocType,Ide,Date,[[Own,Loc]|CopList]) :-
    name(DocType,DTString),
    strings_to_atoms_list([Ide,DTString,Own,Loc,"-",Date],ValList),
    InsertProcess =.. [insert_cop|[ValList]],
    call(InsertProcess),
    insert_copies(DocType,Ide,Date,CopList).

insert_kyws(DocType,Ide,[]) :- !.

insert_kyws(DocType,Ide,[Kyw|KywList]) :-
    name(DocType,DTString),
    strings_to_atoms_list([Ide,DTString,Kyw],ValList),
    InsertProcess =.. [insert_kyw|[ValList]],
    call(InsertProcess),
    insert_kyws(DocType,Ide,KywList).

/* ------ test procedures -----
*/
general_search(A,B,C,D) :-
   !,
   fail.
/*
general_search([[ide,m,Ide]],L1,L2,Result) :-
   !,
   write('Database search for ide = '),
   write(Ide),
   nl,
   write('Please enter result: '),
   read(Result),
   nl,
   (   Result = x,
           !,
           fail
   ;
       true
   ).
*/
/*
insert_art(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_bok(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_bol(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_con(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_inb(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_inc(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_inp(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_man(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_mas(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_mis(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_phd(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_pro(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_tec(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_unp(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_cop(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
insert_kyw(List) :- getval(write_stream_4, WriteStream4),write(WriteStream4,List),writeln(WriteStream4,'. '),!.
*/
/*% ---------- end test procedures ------ */

insert_art([Ide,Aut,Key,Tit,Jou,Vol,Num,Pag,Yea,Mon,Nte,Dat]) :-
    insert(art(ide = ide, aut = Aut, key = Key, tit = Tit, jou = Jou, 
               vol = Vol, num = Num, pag = Pag, yea = Yea, mon = Mon, 
               nte = Nte, dat = Dat)),!.
insert_art(List) :- write('                     Insert failed!'),nl.

insert_bok([Ide,Aut,Key,Tit,Edr,Pub,Add,Edi,Ser,Vol,Num,How,Yea,Nte,Dat]) :-
    insert(bok(ide = Ide, aut = Aut, key = Key, tit = Tit, edr = Edr, 
               pub = Pub, add = Add, edi = Edi, ser = Ser, vol = Vol, 
               num = Num, how = How, yea = Yea, nte = Nte, dat = Dat)),!.
insert_bok(List) :- write('                     Insert failed!'),nl.

insert_bol([Ide,Aut,Key,Tit,Pub,Add,How,Yea,Mon,Nte,Dat]) :-
    insert(bol(ide = ide, aut = Aut, key = Key, tit = Tit, pub = Pub, 
               add = Add, how = How, yea = Yea, mon = Mon, nte = Nte, 
               dat = Dat)),!. 
insert_bol(List) :- write('                     Insert failed!'),nl.

insert_con([Ide,Aut,Key,Tit,Org,Mee,Yea,Mon,Nte,Dat]) :-
    insert(con(ide = Ide, aut = Aut, key = Key, tit = tit, org = Org, 
               mee = Mee, yea = Yea, mon = Mon, nte = Nte, dat = Dat)),!.
insert_con(List) :- write('                     Insert failed!'),nl.

insert_inb([Ide,Aut,Key,Tit,Bot,Cha,Pag,Edr,Pub,Add,Edi,Ser,Vol,Num,How,Yea,
                  Nte,Dat]) :-
    insert(inb(ide = Ide, aut = Aut, key = Key, tit = Tit, bot = Bot, 
               cha = Cha, pag = Pag, edr = Edr, pub = Pub, add = Add, 
               edi = Edi, ser = Ser, vol = Vol, num = Num, how = How, 
               yea = Yea, nte = Nte, dat = Dat)),!.
insert_inb(List) :- write('                     Insert failed!'),nl.

insert_inc([Ide,Aut,Key,Tit,Bot,Cha,Pag,Edr,Pub,Add,Ser,Vol,Num,Yea,Nte,
                  Dat]) :-
    insert(inc(ide = Ide, aut = Aut, key = Key, tit = Tit, bot = Bot, 
               cha = Cha, pag = Pag, edr = Edr, pub = Pub, add = Add, 
               ser = Ser, vol = Vol, num = Num, yea = Yea, nte = Nte, 
               dat = Dat)),!.
insert_inc(List) :- write('                     Insert failed!'),nl.

insert_inp([Ide,Aut,Key,Tit,Bot,Org,Pag,Edr,Pub,Add,Yea,Mon,Nte,Dat]) :-
    insert(inp(ide = Ide, aut = Aut, key = Key, tit = Tit, bot = Bot, 
               org = Org, pag = Pag, edr = Edr, pub = Pub, add = Add, 
               yea = Yea, mon = Mon, nte = Nte, dat = Dat)),!.
insert_inp(List) :- write('                     Insert failed!'),nl.

insert_man([Ide,Aut,Key,Tit,Org,Add,Edi,Yea,Mon,Nte,Dat]) :-
    insert(man(ide = Ide, aut = Aut, key = Key, tit = Tit, org = Org, 
               add = Add, edi = Edi, yea = Yea, mon = Mon, nte = Nte, 
               dat = Dat)),!.
insert_man(List) :- write('                     Insert failed!'),nl.

insert_mas([Ide,Aut,Key,Tit,Sch,Add,Yea,Mon,Nte,Dat]) :-
    insert(mas(ide = Ide, aut = Aut, key = Key, tit = Tit, sch = Sch, 
               add = Add, yea = Yea, mon = Mon, nte = Nte, dat = Dat)),!.
insert_mas(List) :- write('                     Insert failed!'),nl.

insert_mis([Ide,Aut,Key,Tit,How,Yea,Mon,Nte,Dat]) :-
    insert(mis(ide = Ide, aut = Aut, key = Key, tit = Tit, how = How, 
               yea = Yea, mon = Mon, nte = Nte, dat = Dat)),!.
insert_mis(List) :- write('                     Insert failed!'),nl.

insert_phd([Ide,Aut,Key,Tit,Sch,Add,Yea,Mon,Nte,Dat]) :-
    insert(phd(ide = Ide, aut = Aut, key = Key, tit = Tit, sch = Sch, 
               add = Add, yea = Yea, mon = Mon, nte = Nte, dat = Dat)),!.
insert_phd(List) :- write('                     Insert failed!'),nl.

insert_pro([Ide,Key,Org,Tit,Bot,Edr,Pub,Add,Yea,Mon,Nte,Dat]) :-
    insert(pro(ide = Ide, key = Key, org = Org, tit = Tit, bot = Bot, 
               edr = Edr, pub = Pub, add = Add, yea = yea, mon = Mon, 
               nte = Nte, dat = Dat)),!.
insert_pro(List) :- write('                     Insert failed!'),nl.

insert_tec([Ide,Aut,Key,Tit,Ins,Org,Add,Typ,Num,Yea,Mon,Nte,Dat]) :-
    insert(tec(ide = Ide, aut = Aut, key = Key, tit = Tit, ins = Ins, 
               org = Org, add = Add, typ = Typ, num = Num, yea = Yea, 
               mon = Mon, nte = Nte, dat = Dat)),!.
insert_tec(List) :- write('                     Insert failed!'),nl.

insert_unp([Ide,Aut,Key,Tit,Yea,Mon,Nte,Dat]) :-
    insert(unp(ide = Ide, aut = Aut, key = Key, tit = Tit, yea = Yea, 
               mon = Mon, nte = Nte, dat = Dat)),!.
insert_unp(List) :- write('                     Insert failed!'),nl.

insert_cop([Ide,DocType,Own,Loc,Llo,Dat]) :-
    insert(cop(ide = Ide, doc = DocType, own = Own, loc = Loc,
               llo = Llo, dat = Dat)),!.
insert_cop(List) :- write('                     Insert failed!'),nl.

insert_kyw([Ide,DocType,Kyw]) :-
    insert(kyw(ide = Ide, doc = DocType, kyw = Kyw)),!.
insert_kyw(List) :- write('                     Insert failed!'),nl.

% --- collect complaints in 'incorrect' ---

write_errdoc(Symptom,Entry,Bug) :-
    getval(write_stream_2, WriteStream2),
    write(WriteStream2,Symptom),
    writeln(WriteStream2,'. '),
    write(WriteStream2,Entry),
    writeln(WriteStream2,'. '),
    write(WriteStream2,Bug),
    writeln(WriteStream2,'. ').


% -----------------------------------------
%       C O R R E C T   E N T R I E S
% -----------------------------------------

correct_entries :-
    repeat,
       read_comp(Complaint),
       process_comp(Complaint).
    
process_comp(end_of_file) :- !.

process_comp([Symptom,Entry,Bug]) :-
    correct_entry(Symptom,Entry,Bug,CorrectEntry),
    enter(CorrectEntry),
    fail.

correct_entry(noide,[DocType|ValList],MissingL,CorrectEntry) :-
    entry_display([DocType|ValList],Line),
    NewLine is Line + 2,
    correction(noide,MissingL,ResultL,NewLine),
    del_all_atts(key,ValList,List1),
    del_all_atts(yea,List1,List2),
    conc(ResultL,List2,CList),
    generate_ide(CList,Ide),
    conc([[ide,Ide]],CList,CorrList),
    check_and_correct([DocType|CorrList],CorrectEntry).
correct_entry(missing,[DocType|ValList],Attr,[DocType|CorrectList]) :-
    entry_display([DocType|ValList],Line),
    NewLine is Line + 2,
    correction(missing,Attr,Result,NewLine),
    conc([Result],ValList,CList),
    complete([DocType|CList],[DocType|CorrectList]).
correct_entry(toolong,[DocType|ValList],Attr,[DocType|CorrectList]) :-
    entry_display([DocType|ValList],Line),
    NewLine is Line + 2,
    correction(toolong,Attr,Result,NewLine),
    replace_item(Result,ValList,CList1),
    not_too_long([DocType|CList1],[DocType|CList2]),
    complete([DocType|CList2],[DocType|CorrectList]).
correct_entry(double,Entry,[f,Similars],CorrectEntry) :-
    entry_display(Entry,Line),
    ide_ending(Similars,End),
    check_similars(Entry,Similars,CorrectEntry,End).
correct_entry(double,Entry,[m,Similars],CorrectEntry) :-
    double(Entry,RealSimilars),
    entry_display(Entry,Line),
    ide_ending(RealSimilars,End),
    check_similars(Entry,RealSimilars,CorrectEntry,End).

check_and_correct(Entry,CorrectEntry) :-
    double(Entry,Similars),
    !,
    first_double(Entry,N),
    correct_entry(double,Entry,[N,Similars],CorrectEntry).

check_and_correct(Entry,CorrectEntry) :-
    not_too_long(Entry,NTLEntry),
    complete(NTLEntry,CorrectEntry).


check_similars([DocType|ValList],[],[DocType|CorrectList],End) :-
    !,
    correct_ide(ValList,End,CList),
    not_too_long([DocType|CList],[DocType|CList1]),
    complete([DocType|Clist1],[DocType|CorrectList]).
    
check_similars(Entry,[Similar|L],C,E) :-
    clear_below(17),
    cursor(17,1),
    write('-----------------------------------------------------------------'),
    m_display(Similar,19),
    get_answer('Next similar entry (N) / reject entry (R) : ',Answer,23,5),
    process_answer(Answer,Entry,L,C,E).

process_answer("R",Entry,Similars,[],End) :- !.

process_answer(Answer,Entry,Similars,CorrectEntry,End) :-
    check_similars(Entry,Similars,CorrectEntry,End).

complete([DocType|ValList],[DocType|CorrectList]) :-
    val_missing([DocType|ValList],Attr),
    !,
    correct_entry(missing,[DocType|ValList],Attr,[DocType|CorrectList]).

complete(Entry,Entry).

not_too_long([DocType|ValList],[DocType|CorrectList]) :-
    val_too_long([DocType|ValList],Attr),
    !,
    correct_entry(toolong,[DocType|ValList],Attr,[DocType|CorrectList]).

not_too_long(Entry,Entry).    

read_comp(Input) :-
    getval(read_stream_2, ReadStream2),
    read(ReadStream2,Line1),
    (    Line1 = end_of_file,
            Input = Line1
    ;
            read(ReadStream2,Line2),
            read(ReadStream2,Line3),
            Input = [Line1,Line2,Line3]
    ).

correction(noide,MissingL,ResultL,Line) :-
    print_line('Key/Year are incorrect!',Line,5),
    NewL is Line + 1,
    askfor_atts(MissingL,ResultL,NewL,NewLine).

correction(missing,Attr,Result,Line) :-
    print_line('Attribute missing!',Line,5),
    NewL is Line + 1,
    askfor_att([Attr,m],[Result],NewL,NewLine).

correction(toolong,Attr,Result,Line) :-
    print_line('Attribute too long!',Line,5),
    NewL is Line + 1,
    askfor_att([Attr,m],[Result],NewL,NewLine).

% -------------------------------------
%       S U B P R O C E D U R E S
% -------------------------------------

correct_ide(List,End,Result) :-
    del([ide,Ide],List,List1),
    conc(Ide,End,NewIde),
    conc([[ide,NewIde]],List1,Result).

ending(Year,EndYear) :-
    last(Year,S,X),
    last(S,T,Y),
    conc([Y],[X],EndYear).    

entry_display([DocType|ValList],Line) :-
    assert(doc(DocType)),
    del_all_atts(kyw,ValList,List1),
    del_all_atts(cop,List1,List2),
    ordered([DocType|List2],[DocType|List3]),
    strings_to_atoms_result(List3,List4),    
    standard_display(List4,Line),
    retract(doc(DocType)).

first_nonabbr([],"-").

first_nonabbr(String,NonAbbr) :-
    up_to_char(32,String,Str1,Str2),
    !,
    non_abbr(Str1,Str2,NonAbbr).
  
first_nonabbr(String,String) :-
    non_abbr(String,[],String).

m_display([],L).

m_display([[Attr,Val]|L],Line) :-
    cap_attr_abbr(Attribute,Attr),
    cursor(Line,5),
    write(Attribute),
    fill_blanks(Attribute,13),
    write(' :  '),
    name(AVal,Val),
    write(AVal),
    NewLine is Line + 1,
    m_display(L,NewLine).

non_abbr(Str,String,NonAbbr) :-
    lastelem(Str,46),
    !,
    first_nonabbr(String,NonAbbr).

non_abbr(Str,X,Str).

too_long([],X) :-
    !,
    fail.

too_long([[cop,Own,Loc]|L],own) :-
    !,
    len(Own,Length),
    attr_len(own,Len),
    Length > Len.

too_long([[cop,Own,Loc]|L],loc) :-
    !,
    len(Loc,Length),
    attr_len(loc,Len),
    Length > Len.

too_long([[Attr,Val]|L],Attr) :-
    len(Val,Length),
    attr_len(Attr,Len),
    Length > Len,
    !.

too_long([X|L],Attr) :-
    too_long(L,Attr).

% check_mand_atts(AttrList,ValList,Attr) is true if ValList 
% does not contain a value for the mandatory attribute Attr

check_mand_atts([],List,X) :-
    !,
    fail.

check_mand_atts([Attr|L],ValList,Attr) :-
    not(member([Attr,X],ValList)),
    !.

check_mand_atts([Attr|L],ValList,X) :-
    check_mand_atts(L,ValList,X).

% check_atts(AttList,ValList,CheckedList) 
% CheckedList = ValList + [Attr,"-"] for missing attribute Attr

check_atts([],List,[]) :- !.

check_atts([Attr|AList],ValList,[Val|CheckedList]) :-
    member([Attr,Val],ValList),
    !,
    check_atts(AList,ValList,CheckedList).

check_atts([Attr|AList],ValList,["-"|CheckedList]) :-
    check_atts(AList,ValList,CheckedList).

collect(cop,ValList,Copies) :-
    bagof([Own,Loc],member([cop,Own,Loc],ValList),Copies),
    !.

collect(kyw,ValList,KywList) :-
    bagof(Kyw,member([kyw,Kyw],ValList),KywList),
    !.

collect(X,Y,[]).

% first_double

first_double([DocType|ValList],f) :-
    member([ide,Ide],ValList),
    double(DoubleList),
    not(member(Ide,DoubleList)),
    !,
    retract(double(DoubleList)),
    conc([Ide],DoubleList,NewList),
    assert(double(NewList)).

first_double(Entry,m).

% ide_ending computes the character that has to be concatenated to
% the Document_Id of the actual entry to make it different from Similars

ide_ending([SingleItem],"b") :- !.

ide_ending(Similars,[End]) :-
    max(Similars,Max),
    End is Max + 1.

max([],0).

max([[[ide,Ide]|L]|List],Max) :-
    max(List,M),
    last(Ide,X,Last),
    (    M >= Last,
             !,
             Max = M
    ;
             Max = Last
    ).

% strings_to_atoms_list transforms a list of strings into a list of atoms

strings_to_atoms_list([],[]).

strings_to_atoms_list([String|StringList],[Atom|AtomList]) :-
    name(Atom,String),
    strings_to_atoms_list(StringList,AtomList).

% strings_to_atoms_result transforms the strings of a result list into atoms

strings_to_atoms_result([],[]).

strings_to_atoms_result([[Attr,String]|List],[[Attr,Atom]|AtomList]) :-
    name(Atom,String),
    strings_to_atoms_result(List,AtomList).

% usable(Input) is true if Input can be interpreted as a document entry

usable([Atom|List]) :-
    doctype(Atom),
    not(empty(List)),
    atts_and_values(List).

atts_and_values([]).
atts_and_values([[Atom,Val]|L]) :-
    attribute(Atom),
    is_string(Val),
    atts_and_values(L).

atts_and_values([[cop,Val1,Val2]|L]) :-
    is_string(Val1),
    is_string(Val2),
    atts_and_values(L).

% clean(Entry,CleanEntry):   * ? [ ] are removed, ' changed to ` in CleanEntry

clean([DocType|ValList],[DocType|CleanList]) :-
    clean_vallist(ValList,CleanList).

clean_vallist([],[]).

clean_vallist([[Attr,Val]|Tail],[[Attr,CleanVal]|CleanTail]) :-
    clean_val(Val,CleanVal),
    clean_vallist(Tail,CleanTail).

% fix: clean_val([],[]).
clean_val([42|Tail],CTail) :-
    !,
    clean_val(Tail,CTail).

clean_val([63|Tail],CTail) :-
    !,
    clean_val(Tail,CTail).

clean_val([91|Tail],CTail) :-
    !,
    clean_val(Tail,CTail).

clean_val([93|Tail],CTail) :-
    !,
    clean_val(Tail,CTail).

clean_val([39|Tail],[96|CTail]) :-
    !,
    clean_val(Tail,CTail).

clean_val([X|Tail],[X|CTail]) :-
    clean_val(Tail,CTail).


/* ------------------

D)  TRANSLATION OF SCRIBE BIBLIOGRAPHY FILES INTO LOB FILES

*/

translate_file :-
    clearpage,
    get_answer('Please enter name of Scribe file :  ',Inp,4,10),
    complete_filename(Inp,ScribeFile),
    open(ScribeFile,read,ReadStream3),
    setval(read_stream_3, ReadStream3),
    get_answer('Please enter name of LOB file :  ',Inp2,6,10),
    complete_filename(Inp2,LobFile),
    open(LobFile,write,WriteStream3),
    setval(write_stream_3, WriteStream3),
    transl_file,
    close(ReadStream3),
    close(WriteStream3).

transl_file :-
    repeat,
        read_reference(Reference),
        transl_reference(Reference).

transl_reference(end_of_file) :- !.

transl_reference(Reference) :-
    transl_ref(Reference,Entry),
    getval(write_stream_3, WriteStream3),
    write(WriteStream3,Entry),
    writeln(WriteStream3,'. '),
    fail.

transl_ref(Reference,[DocType|ValList]) :-
    retrieve_doctype(Reference,DocType,Rest),
    retrieve_vallist(Rest,ValList).

retrieve_doctype([64|List],DocType,Rest) :-
    !,
    head_3(List,Type),
    transl_case(Type,DocType),
    up_to_char(44,List,X,Rest).

retrieve_doctype([X|List],DocType,Rest) :-
    retrieve_doctype(List,DocType,Rest).

retrieve_vallist(")",[]).

retrieve_vallist(List,[[Attr,Val]|ValL]) :-
    retrieve_attrval(List,Attr,Val,Rest),
    retrieve_vallist(Rest,ValL).

retrieve_attrval([X|List],Attr,Val,Rest) :-
   (   X < 65
   ;
       X > 90,
       X < 97
   ;
       X > 122
   ),
   !,
   retrieve_attrval(List,Attr,Val,Rest).

retrieve_attrval(List,Attr,Value,Rest) :-
    head_3(List,Head),
    transl_attr(Head,Attr),
    up_to_char(34,List,X,Tail),
    up_to_char(34,Tail,Value,Rest).

transl_attr("Not",nte) :- !.
transl_attr(X,A) :- transl_case(X,A).

transl_case([C1,C2,C3],A) :-
    97 =< C1,
    C1 =< 122,
    !,
    name(A,[C1,C2,C3]).

transl_case([C1,C2,C3],A) :-
    N1 is C1 + 32,
    name(A,[N1,C2,C3]).

read_reference(Reference) :-
    read_ref(0,Reference).

read_ref(X,Reference) :-
    getval(read_stream_3, ReadStream3),
    getc(ReadStream3,Y),
    read_ref_rest(X,Y,Reference).

read_ref_rest(0,10,Reference) :-
    !,
    read_ref(0,Reference).

read_ref_rest(41,10,[]) :- !.                 % end of reference

read_ref_rest(X,Y,end_of_file) :-                 % EOF
    Y < 0,
    !.

read_ref_rest(X,39,[96|Reference]) :-         % translates ' into `
    !,
    read_ref(96,Reference).    

read_ref_rest(X,42,Reference) :-              % deletes *
    !,
    read_ref(42,Reference).

read_ref_rest(X,63,Reference) :-              % deletes ?
    !,
    read_ref(63,Reference).

read_ref_rest(X,91,Reference) :-              % deletes [
    !,
    read_ref(91,Reference).    

read_ref_rest(X,93,Reference) :-              % deletes ]
    !,
    read_ref(93,Reference).

read_ref_rest(X,Y,[Y|Reference]) :-
    !,
    read_ref(Y,Reference).


:- set_back_strings_to_strings.
