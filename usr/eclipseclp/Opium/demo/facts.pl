/*
       LOB'S INTERNAL DATABASE
*/
:- dynamic old_assertions/1.

%  ATTRIBUTES
%  add,aut,bot,cha,dat,edi,edr,eds,fla,flo,how,ins,jou,key,
%  mee,mon,nte,num,org,pag,pub,sch,ser,tit,typ,vol,yea, 
%  own,loc,llo,kyw,cop,ide  

attribute(X) :- all_attributes(List),member(X,List).
all_attributes([add,aut,bot,cha,dat,edi,edr,eds,fla,flo,how,ins,jou,key,
                mee,mon,nte,num,org,pag,pub,sch,ser,tit,typ,vol,yea, 
                own,loc,llo,kyw,cop,ide]).

%  DOCUMENT TYPES
%  art,bok,bol,con,inb,inc,inp,man,mas,mis,phd,pro,tec,unp

doctype(X) :- all_doctypes(List),member(X,List). 
all_doctypes([art,bok,bol,con,inb,inc,inp,man,mas,mis,phd,pro,tec,unp]).
order_doctypes([tec,bok,art,inp,pro,con,phd,inb,bol,inc,man,mas,mis,unp]).

%  -----------------------------------------------------------
%  translation of abbreviations into extensions, strings, etc.
%  (used by UPDATE, SEARCH)
%  -----------------------------------------------------------

%  internal abbreviations of the document types

type_abbr(article,art).
type_abbr(book,bok).
type_abbr(booklet,bol).
type_abbr(conference,con).
type_abbr(inbook,inb).
type_abbr(incollection,inc).
type_abbr(inproceedings,inp).
type_abbr(manual,man).
type_abbr(masterthesis,mas).
type_abbr(misc,mis).
type_abbr(phdthesis,phd).
type_abbr(proceeding,pro).
type_abbr(techreport,tec).
type_abbr(unpublished,unp).

%  internal abbreviations of Scribe attributes 

cap_attr_abbr('Address',add).
cap_attr_abbr('Author',aut).
cap_attr_abbr('Booktitle',bot).
cap_attr_abbr('Chapter',cha).
cap_attr_abbr('Date',dat).
cap_attr_abbr('Edition',edi).
cap_attr_abbr('Editor',edr).
cap_attr_abbr('Editors',eds).
cap_attr_abbr('Fullauthor',fla).
cap_attr_abbr('Fullorganization',flo).
cap_attr_abbr('Howpublished',how).
cap_attr_abbr('Institution',ins).
cap_attr_abbr('Journal',jou).
cap_attr_abbr('Key',key).
cap_attr_abbr('Meeting',mee).
cap_attr_abbr('Month',mon).
cap_attr_abbr('Note',nte).
cap_attr_abbr('Number',num).
cap_attr_abbr('Organization',org).
cap_attr_abbr('Pages',pag).
cap_attr_abbr('Publisher',pub).
cap_attr_abbr('School',sch).
cap_attr_abbr('Series',ser).
cap_attr_abbr('Title',tit).
cap_attr_abbr('Type',typ).
cap_attr_abbr('Volume',vol).
cap_attr_abbr('Year',yea).

%  internal abbreviations of LOB specific attributes

cap_attr_abbr('Copy',cop).
cap_attr_abbr('Owner',own).
cap_attr_abbr('Location',loc).
cap_attr_abbr('Lastlocation',llo).
cap_attr_abbr('Keyword',kyw).
cap_attr_abbr('Document-Id',ide).

%  translation of attributes to variable names

att_to_var(add,"Add").
att_to_var(aut,"Aut").
att_to_var(bot,"Bot").
att_to_var(cha,"Cha").
att_to_var(dat,"Dat").
att_to_var(edi,"Edi").
att_to_var(edr,"Edr").
att_to_var(eds,"Eds").
att_to_var(fla,"Fla").
att_to_var(flo,"Flo").
att_to_var(how,"How").
att_to_var(ins,"Ins").
att_to_var(jou,"Jou").
att_to_var(key,"Key").
att_to_var(mee,"Mee").
att_to_var(mon,"Mon").
att_to_var(nte,"Nte").
att_to_var(num,"Num").
att_to_var(org,"Org").
att_to_var(pag,"Pag").
att_to_var(pub,"Pub").
att_to_var(sch,"Sch").
att_to_var(ser,"Ser").
att_to_var(tit,"Tit").
att_to_var(typ,"Typ").
att_to_var(vol,"Vol").
att_to_var(yea,"Yea").

att_to_var(cop,"Cop").
att_to_var(kyw,"Kyw").
att_to_var(ide,"Ide").
att_to_var(doc,"Doc").


%  translation of attributes into strings

att_to_string(add,"add").
att_to_string(aut,"aut").
att_to_string(bot,"bot").
att_to_string(cha,"cha").
att_to_string(dat,"dat").
att_to_string(edi,"edi").
att_to_string(edr,"edr").
att_to_string(eds,"eds").
att_to_string(fla,"fla").
att_to_string(flo,"flo").
att_to_string(how,"how").
att_to_string(ins,"ins").
att_to_string(jou,"jou").
att_to_string(key,"key").
att_to_string(mee,"mee").
att_to_string(mon,"mon").
att_to_string(nte,"nte").
att_to_string(num,"num").
att_to_string(org,"org").
att_to_string(pag,"pag").
att_to_string(pub,"pub").
att_to_string(sch,"sch").
att_to_string(ser,"ser").
att_to_string(tit,"tit").
att_to_string(typ,"typ").
att_to_string(vol,"vol").
att_to_string(yea,"yea").

att_to_string(cop,"cop").
att_to_string(kyw,"kyw").
att_to_string(ide,"ide").
att_to_string(doc,"doc").
           
%  translation of type abbreviations into capitalized atoms 

cap_type_abbr('Article',art).
cap_type_abbr('Book',bok).
cap_type_abbr('Booklet',bol).
cap_type_abbr('Conference',con).
cap_type_abbr('Inbook',inb).
cap_type_abbr('Incollection',inc).
cap_type_abbr('Inproceedings',inp).
cap_type_abbr('Manual',man).
cap_type_abbr('Masterthesis',mas).
cap_type_abbr('Misc',mis).
cap_type_abbr('Phdthesis',phd).
cap_type_abbr('Proceeding',pro).
cap_type_abbr('Techreport',tec).
cap_type_abbr('Unpublished',unp).



%  ------------------------------------------------------------------------
%  relations between attributes and document types (used by UPDATE, SEARCH)
%  ------------------------------------------------------------------------

%  lists of attributes the interactive input will prompt
     
prompted_atts(art,[[aut,m],[key,m],[tit,m],[jou,m],[vol,m],[num,o],
                   [pag,m],[yea,m],[mon,o],
                   [nte,o],[kyw,o],[cop,o]]).
prompted_atts(bok,[[aut,m],[key,m],[tit,m],[edr,o],[pub,m],[add,o],
                   [edi,o],[ser,o],[vol,o],[num,o],[how,o],[yea,m],
                   [nte,o],[kyw,o],[cop,o]]).
prompted_atts(bol,[[aut,m],[key,m],[tit,m],[pub,m],[add,o],[how,o],[yea,m],
                   [mon,o],[nte,o],[kyw,o],[cop,o]]).
prompted_atts(con,[[aut,m],[key,m],[tit,m],[org,m],[mee,o],[yea,m],[mon,o],
                   [nte,o],[kyw,o],[cop,o]]).
prompted_atts(inb,[[aut,m],[key,m],[tit,m],[bot,m],[cha,o],[pag,m],[edr,o],
                   [pub,m],[add,o],[edi,o],[ser,o],[vol,o],[num,o],[how,o],
                   [yea,m],[nte,o],[kyw,o],[cop,o]]).
prompted_atts(inc,[[aut,m],[key,m],[tit,m],[bot,m],[cha,o],[pag,m],[edr,o],
                   [pub,m],[add,o],[ser,o],[vol,o],[num,o],[yea,m],
                   [nte,o],[kyw,o],[cop,o]]).
prompted_atts(inp,[[aut,m],[key,m],[tit,m],[bot,m],[org,m],[pag,m],[edr,o],
                   [pub,o],[add,o],[yea,m],[mon,o],
                   [nte,o],[kyw,o],[cop,o]]).
prompted_atts(man,[[aut,o],[key,m],[tit,m],[org,o],[add,o],[edi,o],
                   [yea,m],[mon,o],[nte,o],[kyw,o],[cop,o]]).
prompted_atts(mas,[[aut,m],[key,m],[tit,m],[sch,m],[add,o],[yea,m],[mon,o],
                   [nte,o],[kyw,o],[cop,o]]).
prompted_atts(mis,[[aut,m],[key,m],[tit,m],[how,m],[yea,o],[mon,o],
                   [nte,o],[kyw,o],[cop,o]]).
prompted_atts(phd,[[aut,m],[key,m],[tit,m],[sch,m],[add,o],[yea,m],[mon,o],
                   [nte,o],[kyw,o],[cop,o]]).
prompted_atts(pro,[[org,m],[key,m],[tit,m],[bot,o],[edr,o],[pub,m],[add,o],
                   [yea,m],[mon,o],[nte,o],[kyw,o],[cop,o]]).
prompted_atts(tec,[[aut,m],[key,m],[tit,m],[ins,m],[org,o],[add,o],
                   [typ,o],[num,o],[yea,m],[mon,o],
                   [nte,o],[kyw,o],[cop,o]]).
prompted_atts(unp,[[aut,m],[key,m],[tit,m],[yea,o],[mon,o],          
                   [nte,o],[kyw,o],[cop,o]]).


%  attributes of educe relations 
     
rel_has_atts(art,[ide,aut,key,tit,jou,vol,num,pag,yea,mon,nte,dat]).
rel_has_atts(bok,[ide,aut,key,tit,edr,pub,add,edi,ser,vol,num,how,yea,nte,
                  dat]).
rel_has_atts(bol,[ide,aut,key,tit,pub,add,how,yea,mon,nte,dat]).
rel_has_atts(con,[ide,aut,key,tit,org,mee,yea,mon,nte,dat]).
rel_has_atts(inb,[ide,aut,key,tit,bot,cha,pag,edr,pub,add,edi,ser,vol,num,how,
                  yea,nte,dat]).
rel_has_atts(inc,[ide,aut,key,tit,bot,cha,pag,edr,pub,add,ser,vol,num,yea,nte,
                  dat]).
rel_has_atts(inp,[ide,aut,key,tit,bot,org,pag,edr,pub,add,yea,mon,nte,
                  dat]).
rel_has_atts(man,[ide,aut,key,tit,org,add,edi,yea,mon,nte,dat]).
rel_has_atts(mas,[ide,aut,key,tit,sch,add,yea,mon,nte,dat]).
rel_has_atts(mis,[ide,aut,key,tit,how,yea,mon,nte,dat]).
rel_has_atts(phd,[ide,aut,key,tit,sch,add,yea,mon,nte,dat]).
rel_has_atts(pro,[ide,org,key,tit,bot,edr,pub,add,yea,mon,nte,dat]).
rel_has_atts(tec,[ide,aut,key,tit,ins,org,add,typ,num,yea,mon,nte,dat]).
rel_has_atts(unp,[ide,aut,key,tit,yea,mon,nte,dat]).

rel_has_atts(cop,[ide,doc,own,loc,llo,dat]).
rel_has_atts(kyw,[ide,doc,kyw]).

% attributes of document types

doc_has_atts(DocType,AttList) :-
    rel_has_atts(DocType,AList),
    conc(AList,[kyw,own,loc,llo,cop],AttList).

% mandatory attributes

mandatory_atts(art,[aut,key,tit,jou,pag,yea]).
mandatory_atts(bok,[aut,key,tit,pub,yea]).
mandatory_atts(bol,[aut,key,tit,pub,yea]).
mandatory_atts(con,[aut,key,tit,org,yea]).
mandatory_atts(inb,[aut,key,tit,bot,pag,pub,yea]).
mandatory_atts(inc,[aut,key,tit,bot,pag,pub,yea]).
mandatory_atts(inp,[aut,key,tit,bot,org,pag,yea]).
mandatory_atts(man,[key,tit,yea]).
mandatory_atts(mas,[aut,key,tit,sch,yea]).
mandatory_atts(mis,[aut,key,tit,how]).
mandatory_atts(phd,[aut,key,tit,sch,yea]).
mandatory_atts(pro,[org,key,tit,pub,yea]).
mandatory_atts(tec,[aut,key,tit,ins,yea]).
mandatory_atts(unp,[aut,key,tit]).


% lists of document types related to an attribute

att_of_types(add,[bok,bol,inb,inc,inp,man,mas,phd,pro,tec]).
att_of_types(aut,[art,bok,bol,con,inb,inc,inp,man,mas,mis,phd,tec,unp]).
att_of_types(bot,[inb,inc,inp,pro]).
att_of_types(cha,[inb,inc]).
att_of_types(dat,[art,bok,bol,con,inb,inc,inp,man,mas,mis,phd,pro,tec,unp]).
att_of_types(edi,[bok,inb,man]).
att_of_types(edr,[art,bok,inb,inc,inp,pro]).
att_of_types(eds,[]).
att_of_types(fla,[]).
att_of_types(flo,[]).
att_of_types(how,[bok,bol,inb,mis]).
att_of_types(ins,[tec]).
att_of_types(jou,[art]).
att_of_types(key,[art,bok,bol,con,inb,inc,inp,man,mas,mis,phd,pro,tec,unp]).
att_of_types(mee,[con]).
att_of_types(mon,[art,bol,con,inp,man,mas,mis,phd,pro,tec,unp]).
att_of_types(nte,[art,bok,bol,con,inb,inc,inp,man,mas,mis,phd,pro,tec,unp]).
att_of_types(num,[art,bok,inb,inc,tec]).
att_of_types(org,[inp,man,pro,tec]).
att_of_types(pag,[art,inb,inc,inp]).
att_of_types(pub,[bok,bol,inb,inc,inp,pro]).
att_of_types(sch,[mas,phd]).
att_of_types(ser,[bok,inb,inc]).
att_of_types(tit,[art,bok,bol,con,inb,inc,inp,man,mas,mis,phd,pro,tec,unp]).
att_of_types(typ,[tec]).
att_of_types(vol,[art,bok,inb,inc]).
att_of_types(yea,[art,bok,bol,con,inb,inc,inp,man,mas,mis,phd,pro,tec,unp]).

att_of_types(cop,[art,bok,bol,con,inb,inc,inp,man,mas,mis,phd,pro,tec,unp]).
att_of_types(kyw,[art,bok,bol,con,inb,inc,inp,man,mas,mis,phd,pro,tec,unp]).
att_of_types(ide,[art,bok,bol,con,inb,inc,inp,man,mas,mis,phd,pro,tec,unp]).
att_of_types(doc,[art,bok,bol,con,inb,inc,inp,man,mas,mis,phd,pro,tec,unp]).


%  attributes for the standard retrieval of a document type
     
stand_docattrs(art,[ide,aut,tit,jou,vol,num,pag,yea,mon,nte]).
stand_docattrs(bok,[ide,aut,tit,pub,yea,nte]).
stand_docattrs(bol,[ide,aut,tit,pub,yea,nte]).
stand_docattrs(con,[ide,aut,tit,org,mee,yea,mon,nte]).
stand_docattrs(inb,[ide,aut,tit,bot,cha,pag,pub,yea,nte]).
stand_docattrs(inc,[ide,aut,tit,bot,cha,pag,yea,nte]).
stand_docattrs(inp,[ide,aut,tit,bot,org,pag,yea,nte]).
stand_docattrs(man,[ide,aut,tit,org,yea,mon,nte]).
stand_docattrs(mas,[ide,aut,tit,sch,add,yea,nte]).
stand_docattrs(mis,[ide,aut,tit,how,yea,mon,nte]).
stand_docattrs(phd,[ide,aut,tit,sch,add,yea,nte]).
stand_docattrs(pro,[ide,org,tit,bot,edr,yea,nte]).
stand_docattrs(tec,[ide,aut,tit,ins,org,add,typ,num,yea,nte]).
stand_docattrs(unp,[ide,aut,tit,yea,mon,nte]).


%  ------------------------------------
%  length of attributes in the database
%  ------------------------------------

attr_len(add,40).
attr_len(aut,70).
attr_len(bot,80).
attr_len(cha,6).
attr_len(dat,10).
attr_len(edi,40).
attr_len(edr,40).
attr_len(eds,0).
attr_len(fla,0).
attr_len(flo,0).
attr_len(how,40).
attr_len(ins,80).
attr_len(jou,80).
attr_len(key,25).
attr_len(mee,80).
attr_len(mon,9).
attr_len(nte,57).
attr_len(num,20).
attr_len(org,80).
attr_len(pag,12).
attr_len(pub,40).
attr_len(sch,30).
attr_len(ser,40).
attr_len(tit,90).
attr_len(typ,30).
attr_len(vol,10).
attr_len(yea,4).

attr_len(ide,28).
attr_len(kyw,40).
attr_len(own,10).
attr_len(loc,10).
attr_len(llo,10).


% initialisation of old_assertions and already_seen.

old_assertions([]).

already_seen([]).

% ---- used by old versions only

% b_call 

b_call(add,"add(Add)").
b_call(aut,"aut(Aut)").
b_call(bot,"bot(Bot)").
b_call(cha,"cha(Cha)").
b_call(dat,"dat(Dat)").
b_call(edi,"edi(Edi)").
b_call(edr,"edr(Edr)").
b_call(eds,"eds(Eds)").
b_call(fla,"fla(Fla)").
b_call(flo,"flo(Flo)").
b_call(how,"how(How)").
b_call(ins,"ins(Ins)").
b_call(jou,"jou(Jou)").
b_call(key,"key(Key)").
b_call(mee,"mee(Mee)").
b_call(mon,"mon(Mon)").
b_call(nte,"nte(Nte)").
b_call(num,"num(Num)").
b_call(org,"org(Org)").
b_call(pag,"pag(Pag)").
b_call(pub,"pub(Pub)").
b_call(sch,"sch(Sch)").
b_call(ser,"ser(Ser)").
b_call(tit,"tit(Tit)").
b_call(typ,"typ(Typ)").
b_call(vol,"vol(Vol)").
b_call(yea,"yea(Yea)").

b_call(cop,"cop(Cop)").
b_call(kyw,"kyw(Kyw)").
b_call(ide,"ide(Ide)").
b_call(doc,"doc(Doc)").






