add_DRAM(Id) :- 
    is_list(Id),
    (ID_GDDR0,INKIND_GDDR0,OUTKIND_GDDR0) = (["GDDR0" | Id],memory,memory),
    (ID_RAMOUT,INKIND_RAMOUT,OUTKIND_RAMOUT) = (["RAMOUT" | Id],memory,memory),
    (ID_DRAMMAP,INKIND_DRAMMAP,OUTKIND_DRAMMAP) = (["DRAMMAP" | Id],memory,memory),
    assert(node_accept(ID_GDDR0,([block{base:0,limit:4276092927}]))),
    assert(node_translate(ID_DRAMMAP,([block{base:0,limit:4276092927}]),ID_RAMOUT,([block{base:0,limit:4276092927}]))).

add_SOCKET(Id) :- 
    is_list(Id),
    (ID_LOCAL,INKIND_LOCAL,OUTKIND_LOCAL) = (["LOCAL" | Id],memory,memory),
    (ID_LOCAL_SRC,INKIND_LOCAL_SRC,OUTKIND_LOCAL_SRC) = (["LOCAL_SRC" | Id],memory,memory),
    ID_RAM = ["RAM" | Id],
    assert(node_accept(ID_LOCAL,([block{base:0,limit:4276092927}]))),
    (block_values([block{base:1,limit:2},block{base:10,limit:11}],IDL_x),(foreach(IDT_x,IDL_x),param(Id),param(ID_LOCAL),param(ID_LOCAL_SRC),param(ID_RAM) do 

    (block_values([block{base:10,limit:11}],IDL_y),(foreach(IDT_y,IDL_y),param(Id),param(ID_LOCAL),param(ID_LOCAL_SRC),param(ID_RAM),param(IDT_x) do 
        add_DRAM([[IDT_x,IDT_y] | ID_RAM]),
        assert(node_overlay(["RAMOUT" | [[IDT_x,IDT_y] | ID_RAM]],ID_LOCAL)),
        assert(node_translate(ID_LOCAL_SRC,([block{base:4096,limit:8192}]),["GDDR0" | [[IDT_x,IDT_y] | ID_RAM]],([block{base:4096,limit:8192}])))
    ))
)).

