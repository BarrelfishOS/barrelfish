% LOOP57
:- module(yoyo3).

:- dynamic already_seen/1.

:- lib(porting, yoyo3).
:- set_all_strings_to_lists.	% this has to be done before compiling the files!

:- lib(basic, yoyo3).
:- lib(facts, yoyo3).
:- lib(screen, yoyo3).
:- set_flag(print_depth, 10000).

:- op(400, xfy, '.').
:- op(400, xfy, and).


getbug :-	
	writeln('\nCheck that you are in "module(yoyo3).", then'),
	writeln('to start the program type "check_input."\n and enter "Foo" (once) and "1986" (many times)\n').

bug :- 
	nl,
	explanation.

explanation :-	
writeln('There is no clause treating the case that there is no similar entry, \n\
causing backtracking to the place where an input is requested. \n\
\n\
GOAL:	 check_input, filename "bib4_list" \n\
CORRECT: ask for the missing key, display complete entry \n\
BUGGY:   endless loop (failure-driven). \n').


% ============================================================================

% --- test --- 5.7.89

insert(Entry) :-
	nl, writeln('Entry:'), writeln(Entry), nl.

double(_,_) :- !, fail.
double([DocType|ValList],Similars) :-
    member([ide,Ide],ValList),
    bagof(Result,
          general_search([[ide,m,Ide]],[ide,aut,tit],[DocType],Result),
          Similars).

/*                         UPDATE OF THE DATABASE                


A)  interactive input help
B)  input checker
C)  erase/modify/add (Key)
D)  transformation of findecrc files into LOB files

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

% ---------------
% test procedures
% ---------------

existing_name(Filename) :- !,fail.

% =============================================== main program ================

build_file :-
    clearpage,
%    get_answer('Please enter filename :  ',Input,4,10),
%    complete_filename(Input,Filename),
    lob_build_lib_name(bib4_list, Filename), % for testing
    open(Filename,write,bibfile),
    build_file(Filename),
    close(bibfile).

build_file(Filename) :-
    existing_name(Filename),                                    
    !,
    error_message('Filename already exists!',2,5,5),
    readln(X),
    clearpage,
    build_file.
build_file(Filename) :-
    clearpage,
    process_date,
    process_file(Filename).

process_file(Filename) :-
    cursor(2,5),
    write('Document type :  '),
    readln(Input),
    checkinp_1(Input,CheckedInp),
    !,
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
    process_file(Filename).
process_doc(DocType,Filename,Line) :-
    prompted_atts(DocType,AttrList),                                 
    askfor_atts(AttrList,ValueList,Line,XLine),
    write_entry([DocType|ValueList]),
    clearpage,
    process_file(Filename).

% askfor_atts will prompt the user to enter the values of the attributes

askfor_atts([Attr],Res,ActLine,NewLine) :-
    askfor_att(Attr,Res,ActLine,NewLine).
askfor_atts([Attr|AttrList],ValueList,ActLine,NewLine) :-
    askfor_att(Attr,Res,ActLine,ActL),
    askfor_atts(AttrList,ResList,ActL,NewLine),
    conc(Res,ResList,ValueList).

askfor_att([kyw,o],ResList,ActLine,NewLine) :-
    Line is ActLine + 1,
    askfor_att_k([kyw,o],ResList,Line,NewLine).
askfor_att([cop,o],ResList,ActLine,NewLine) :-
    Line is ActLine + 1,
    askfor_att_c([cop,o],ResList,Line,NewLine).
askfor_att([Attr,R],Res,ActLine,NewLine) :-
    cap_attr_abbr(Attribute,Attr),
    clearline(ActLine,5),
    write(Attribute),
    fill_blanks(Attribute,13),
    write(' :  '),
    receive(Attr,R,Res,ActLine,NewLine,22).

% Entry of (several) keywords: ResList = [[kyw,"logic"],[kyw,"Prolog"]]

askfor_att_k([kyw,o],ResList,ActLine,NewLine) :-
    askfor_kyw(Res,ActLine,ActL),
    process_kyw(Res,ResList,ActL,NewLine).

process_kyw([],[],ActLine,ActLine) :-
    !.
process_kyw(Res,ResList,ActLine,NewLine) :-
    askfor_att_k([kyw,o],Rlist,ActLine,NewLine),
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

askfor_att_c([cop,o],ResList,ActLine,NewLine) :-
    askfor_att([own,o],Res,ActLine,ActL),
    process_cop(Res,ResList,ActL,NewLine).

process_cop([],[],ActLine,ActLine).
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


% ---------------------
% input check functions
% ---------------------

checkinp_1("",no).
checkinp_1("no",no).
checkinp_1("n",no).
checkinp_1("e",no).
checkinp_1("x",no).
checkinp_1("exit",no).
checkinp_1(X,Y) :-
    name(Z,X),
    type_abbr(Z,Y).
checkinp_1(X,Y) :-
    name(Y,X), 
    type_abbr(_,Y).
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
% error messages
% --------------

error_message(Message,Line,Col,BackCol) :-
    clearline(Line,Col),
    write('    ** '),
%   write('*** error: '),
    write(Message),
    write(' **'),
    readln(X),
    clearline(Line,BackCol).


% --------------
% subprocedures
% --------------

process_date :-
    repeat,
      (    get_answer('Please enter today`s date (DD.MM.YY) : ',Date,2,5),
               check_date(Date),       
               write(bibfile,Date),
               writeln(bibfile,'. '),
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
    checkinp_2(Attr,R,Value,Res,ActLine,NewLine).
receive(Attr,R,Res,ActLine,NewLine,FromCol) :-
    receive(Attr,R,Res,ActLine,NewLine,FromCol).

write_entry(Entity) :-
    write(bibfile,Entity),
    writeln(bibfile,'. ').

put_dot(Attr,Line,Col) :-
    attr_len(Attr,Length),
    Col2 is Length + Col + 2,
    dot(Line,Col2),
    cursor(Line,Col).

dot(Line,Col) :-
    Col =< 80,
    !,
    cursor(Line,Col),
    write('.').
dot(Line,Col).


/*

------------------

B)  INPUT CHECKER

check_input will read new references from a file (written in a format like
the output of build_file) and check whether there are values for all the 
mandatory attributes, whether the values are not too long, and whether there 
are entries in the database that are similar to the new one (i.e. have the
same key). Correct references are directly inserted into the database.
Incorrect references and those which are suspected to be already contained
in the database are collected in a  file 'documents.err'. These references
are presented to the moderator after the first check, s/he has the possibility
to correct/reject them.

*/

check_input :-
    clearpage,
%    get_answer('Please enter filename :  ',Input,4,10),
%    complete_filename(Input,Filename),
    lob_build_lib_name(bib4_list, Filename), % for testing
    open(Filename,read,bibinput),
    check_input(Filename),
    close(bibinput).

% check_input(Filename) :-
%     not(existing_name(Filename)),                                    
%     !,
%     error_message('Filename does not exists!',2,5,5),
%     readln(X),
%     clearpage,
%     check_input.    

check_input(Filename) :-
    lob_build_lib_name('documents.err', Filename),
    open(Filename, write, errout),
    read(bibinput,Date),
    assert(dat(Date)),
    check_file,
    close(errout),
    !,
    open(Filename,read,errin),
    correct_entries,
    close(errin),
    retract(dat(Date)).


% --------------------------------
%        C H E C K   F I L E
% --------------------------------

check_file :-
    read(bibinput,Entry),
    process_entry(Entry).

process_entry(end_of_file) :- !.
process_entry(E) :-
    add_ide(E,Entry),
    !,
    proc_entry(Entry).
process_entry(Entry) :-
    write_errdoc(noide,Entry,[[key,m],[yea,m]]),
    check_file.

proc_entry(Entry) :-
    double(Entry,Similars),
    write_errdoc(double,Entry,Similars),
    check_file.
proc_entry(Entry) :-
    val_too_long(Entry,Attr),
    write_errdoc(toolong,Entry,Attr),
    check_file.
proc_entry(Entry) :-
    val_missing(Entry,Attr),
    write_errdoc(missing,Entry,Attr),
    check_file.
proc_entry(Entry) :-
    enter(Entry),
    check_file.

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

enter([]).
enter(Entry) :-
    add_date(Entry,ComplEntry),
    enter_doc(ComplEntry).
%    enter_copies(ComplEntry),
%    enter_kyws(ComplEntry).

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

insert_copies(DocType,Ide,Date,[]).
insert_copies(DocType,Ide,Date,[[Own,Loc]|CopList]) :-
    name(DocType,DTString),
    strings_to_atoms_list([Ide,DTString,Own,Loc,"-",Date],ValList),
    InsertProcess =.. [insert_cop|[ValList]],
    call(InsertProcess),
    insert_copies(DocType,Ide,Date,CopList).

insert_kyws(DocType,Ide,[]).
insert_kyws(DocType,Ide,[Kyw|KywList]) :-
    name(DocType,DTString),
    strings_to_atoms_list([Ide,DTString,Kyw],ValList),
    InsertProcess =.. [insert_kyw|[ValList]],
    call(InsertProcess),
    insert_kyws(DocType,Ide,KywList).

% ------ test procedures -----

general_search([[ide,m,Ide]],L1,L2,Result) :-
   nl,nl,
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
     
insert_art(List) :- write(List),nl,assert(ins(List)).
insert_bok(List) :- write(List),nl,assert(ins(List)).
insert_bol(List) :- write(List),nl,assert(ins(List)).
insert_con(List) :- write(List),nl,assert(ins(List)).
insert_inb(List) :- write(List),nl,assert(ins(List)).
insert_inc(List) :- write(List),nl,assert(ins(List)).
insert_inp(List) :- write(List),nl,assert(ins(List)).
insert_man(List) :- write(List),nl,assert(ins(List)).
insert_mas(List) :- write(List),nl,assert(ins(List)).
insert_mis(List) :- write(List),nl,assert(ins(List)).
insert_phd(List) :- write(List),nl,assert(ins(List)).
insert_pro(List) :- write(List),nl,assert(ins(List)).
insert_tec(List) :- write(List),nl,assert(ins(List)).
insert_unp(List) :- write(List),nl,assert(ins(List)).
insert_cop(List) :- write(List),nl,assert(ins(List)).
insert_kyw(List) :- write(List),nl,assert(ins(List)).

% ---------- end test procedures ------

/*

insert_art([Ide,Aut,Tit,Jou,Vol,Num,Pag,Yea,Mon,Nte,Dat]) :-
    insert(art(ide = ide, aut = Aut, tit = Tit, jou = Jou, vol = Vol,
               num = Num, pag = Pag, yea = Yea, mon = Mon, nte = Nte, 
               dat = Dat)).

insert_bok([Ide,Aut,Tit,Edr,Pub,Add,Edi,Ser,Vol,Num,How,Yea,Nte,Dat]) :-
    insert(bok(ide = Ide, aut = Aut, tit = Tit, edr = Edr, pub = Pub,
               add = Add, edi = Edi, ser = Ser, vol = Vol, num = Num,
               how = How, yea = Yea, nte = Nte, dat = Dat)).

insert_bol([Ide,Aut,Tit,Pub,Add,How,Yea,Mon,Nte,Dat]) :-
    insert(bol(ide = ide, aut = Aut, tit = Tit, pub = Pub, add = Add,
               how = How, yea = Yea, mon = Mon, nte = Nte, dat = Dat)).
               
insert_con([Ide,Aut,Tit,Org,Mee,Yea,Mon,Nte,Dat]) :-
    insert(con(ide = Ide, aut = Aut, tit = tit, org = Org, mee = Mee,
               yea = Yea, mon = Mon, nte = Nte, dat = Dat)).

insert_inb([Ide,Aut,Tit,Bot,Cha,Pag,Edr,Pub,Add,Edi,Ser,Vol,Num,How,Yea,
                  Nte,Dat]) :-
    insert(inb(ide = Ide, aut = Aut, tit = Tit, bot = Bot, cha = Cha,
               pag = Pag, edr = Edr, pub = Pub, add = Add, edi = Edi,
               ser = Ser, vol = Vol, num = Num, how = How, yea = Yea,
               nte = Nte, dat = Dat)).

insert_inc([Ide,Aut,Tit,Bot,Cha,Pag,Edr,Pub,Add,Ser,Vol,Num,Yea,Nte,Dat]) :-
    insert(inc(ide = Ide, aut = Aut, tit = Tit, bot = Bot, cha = Cha,
               pag = Pag, edr = Edr, pub = Pub, add = Add, ser = Ser,
               vol = Vol, num = Num, yea = Yea, nte = Nte, dat = Dat)).

insert_inp([Ide,Aut,Tit,Bot,Org,Pag,Edr,Pub,Add,Yea,Mon,Nte,Dat]) :-
    insert(inp(ide = Ide, aut = Aut, tit = Tit, bot = Bot, org = Org,
               pag = Pag, edr = Edr, pub = Pub, add = Add, yea = Yea,
               mon = Mon, nte = Nte, dat = Dat)).

insert_man([Ide,Aut,Tit,Org,Add,Edi,Yea,Mon,Nte,Dat]) :-
    insert(man(ide = Ide, aut = Aut, tit = Tit, org = Org, add = Add,
               yea = Yea, mon = Mon, nte = Nte, dat = Dat)).

insert_mas([Ide,Aut,Tit,Sch,Add,Yea,Mon,Nte,Dat]) :-
    insert(mas(ide = Ide, aut = Aut, tit = Tit, sch = Sch, add = Add,
               yea = Yea, mon = Mon, nte = Nte, dat = Dat)).

insert_mis([Ide,Aut,Tit,How,Yea,Mon,Nte,Dat]) :-
    insert(mis(ide = Ide, aut = Aut, tit = Tit, how = How, yea = Yea,
               mon = Mon, nte = Nte, dat = Dat)).

insert_phd([Ide,Aut,Tit,Sch,Add,Yea,Mon,Nte,Dat]) :-
    insert(phd(ide = Ide, aut = Aut, tit = Tit, sch = Sch, add = Add,
               yea = Yea, mon = Mon, nte = Nte, dat = Dat)).

insert_pro([Ide,org,Tit,Bot,Edr,Pub,Add,Yea,Mon,Nte,Dat]) :-
    insert(pro(ide = Ide, org = Org, tit = Tit, bot = Bot, edr = Edr,
               pub = Pub, add = Add, yea = yea, mon = Mon, nte = Nte,
               dat = Dat)).

insert_tec([Ide,Aut,Tit,Ins,Org,Add,Typ,Num,Yea,Mon,Nte,Dat]) :-
    insert(tec(ide = Ide, aut = Aut, tit = Tit, ins = Ins, org = Org,
               add = Add, typ = Typ, num = Num, yea = Yea, mon = Mon,
               nte = Nte, dat = Dat)).

insert_unp([Ide,Aut,Tit,Yea,Mon,Nte,Dat]) :-
    insert(unp(ide = Ide, aut = Aut, tit = Tit, yea = Yea, mon = Mon,
               nte = Nte, dat = Dat)).

insert_cop([Ide,DocType,Own,Loc,Llo,Dat]) :-
    insert(cop(ide = Ide, doc = DocType, own = Own, loc = Loc,
               llo = Llo, dat = Dat)).

insert_kyw([Ide,DocType,Kyw]) :-
    insert(kyw(ide = Ide, doc = DocType, kyw = Kyw)).

*/

% -------------------------
% process incorrect entries
% -------------------------

write_errdoc(Symptom,Entry,Bug) :-
    write(errout,Symptom),
    writeln(errout,'. '),
    write(errout,Entry),
    writeln(errout,'. '),
    write(errout,Bug),
    writeln(errout,'. ').


% -----------------------------------------
%       C O R R E C T   E N T R I E S
% -----------------------------------------

correct_entries :-
    read_comp(Complaint),
    process_comp(Complaint).
    
process_comp(end_of_file) :- !.
process_comp([Symptom,Entry,Bug]) :-
    correct_entry(Symptom,Entry,Bug,CorrectEntry),
    enter(CorrectEntry),
    correct_entries.

correct_entry(noide,[DocType|ValList],MissingL,CorrectEntry) :-
    entry_display([DocType|ValList],Line),
    NewLine is Line + 2,
    correction(noide,MissingL,ResultL,NewLine),
    conc(ResultL,ValList,CList),
    generate_ide(CList,Ide),
    conc([[ide,Ide]],CList,CorrList),
    check_double([DocType|CorrList],ResEntry),
    (    ResEntry = [],
             CorrectEntry = []
    ;
             not_too_long(ResEntry,NTLEntry),
             complete(NTLEntry,CorrectEntry)
    ).
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
    replace(Result,ValList,CList1),
    not_too_long([DocType|CList1],[DocType|CList2]),
    complete([DocType|CList2],[DocType|CorrectList]).
correct_entry(double,Entry,Similars,CorrectEntry) :-
    entry_display(Entry,Line),
    ide_ending(Similars,End),
    check_double_4(Entry,Similars,CorrectEntry,End).

check_double(Entry,CorrectEntry) :-
    double(Entry,Similars),
    correct_entry(double,Entry,Similars,CorrectEntry).
% fix: add second clause for check_double treating the case
%      that there is no similar entry:
%      check_doubleEn(try, Entry).

check_double_4([DocType|ValList],[],[DocType|CorrectList],End) :-
    clear_below(20),
    cursor(22,5),
    write('Reference will be entered into the database'),
    correct_ide(ValList,End,CList),
    not_too_long([DocType|CList],[DocType|CList1]),
    complete([DocType|Clist1],[DocType|CorrectList]).
check_double_4(Entry,[Similar|L],C,E) :-
    clear_below(20),
    write('-----------------------------------------------------------------'),
    m_display(Similar),
    get_answer('Next similar entry (N) / reject entry (R) : ',Answer,24,5),
    process_answer(Answer,Entry,L,C,E).

process_answer("R",Entry,Similars,[],End).
process_answer(Answer,Entry,Similars,CorrectEntry,End) :-
    check_double_4(Entry,Similars,CorrectEntry,End).

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
    read(errin,Line1),
    (    Line1 = end_of_file,
            Input = Line1
    ;
            read(errin,Line2),
            read(errin,Line3),
            Input = [Line1,Line2,Line3]
    ).

correction(noide,MissingL,ResultL,Line) :-
    print_line('Key/Year were incorrect!',Line,5),
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
    non_abbr(Str1,Str2,NonAbbr).
first_nonabbr(String,String) :-
    non_abbr(String,[],String).

m_display([],L).
m_display([[Attr|Val]|L],Line) :-
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
    first_nonabbr(String,NonAbbr).
non_abbr(Str,X,Str).

too_long([],X) :-
    !,
    fail.
too_long([[cop,Own,Loc]|L],own) :-
    len(Own,Length),
    attr_len(own,Len),
    Length > Len.
too_long([[cop,Own,Loc]|L],loc) :-
    len(Loc,Length),
    attr_len(loc,Len),
    Length > Len.
too_long([[Attr,Val]|L],Attr) :-
    len(Val,Length),
    attr_len(Attr,Len),
    Length > Len.
too_long([X|L],Attr) :-
    too_long(L,Attr).

% check_mand_atts(AttrList,ValList,Attr) is true if ValList 
% does not contain a value for the mandatory attribute Attr

check_mand_atts([],List,X) :-
    !,
    fail.
check_mand_atts([Attr|L],ValList,Attr) :-
    not(member([Attr,X],ValList)).
check_mand_atts([Attr|L],ValList,X) :-
    check_mand_atts(L,ValList,X).

% check_atts(AttList,ValList,CheckedList) 
% CheckedList = ValList + [Attr,"-"] for missing attribute Attr

check_atts([],List,[]).
check_atts([Attr|AList],ValList,[Val|CheckedList]) :-
    member([Attr,Val],ValList),
    !,
    check_atts(AList,ValList,CheckedList).
check_atts([Attr|AList],ValList,["-"|CheckedList]) :-
    check_atts(AList,ValList,CheckedList).

collect(cop,ValList,Copies) :-
    bagof([Own,Loc],member([cop,Own,Loc],ValList),Copies).
collect(kyw,ValList,KywList) :-
    bagof(Kyw,member([kyw,Kyw],ValList),KywList).
collect(X,Y,[]).

complete_filename(Str,CompName) :-
    atom_string(Atom, Str),
    lob_build_lib_name(Atom, CompName).
%    conc("/home/lp/ode/demo/loop/",Str,CompStr),
%    name(CompName,CompStr).


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

:- set_back_strings_to_strings.

