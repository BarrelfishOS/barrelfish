:-module(node_cnt).
:-comment(author,"Helmut Simonis").
:-comment(status,"experimental").
:-comment(copyright,"2010, Helmut Simonis").
:-comment(categories,["Development Tools","Visualization"]).
:-comment(summary,"Primitives to exchange node count information between visualization.ecl and visualize_tree.ecl").
:-comment(description,"Internal use only").

:-comment(current_node_cnt/1,[summary:"Synchronisation between variable and tree node numbers",
                  args:["Id":"returns the current node id number"],
                  amode:current_node_cnt(-),
                  desc:html("Internal use only, needed to exchange information between visualization.ecl and visualize_tree.ecl"),
                  see_also:[set_node_cnt/1,
                            new_node_cnt/4]]).


:-export(current_node_cnt/1).

:-comment(set_node_cnt/1,[summary:"Synchronisation between variable and tree node numbers",
                  args:["Id":"set the node count to this id"],
                  amode:set_node_cnt(++),
                  desc:html("Internal use only, needed to exchange information between visualization.ecl and visualize_tree.ecl"),
                  see_also:[current_node_cnt/1,
                            new_node_cnt/4]]).
:-export(set_node_cnt/1).

:-comment(new_node_cnt/4,[summary:"Synchronisation between variable and tree node numbers",
                  args:["Handle":"the opaque visualization handle",
                        "Id":"returns the current node id number",
                        "Parent":"returns the id of the parent node",
                        "Stream":"returns the file descriptor for the tree stream"],
                  amode:new_node_cnt(+,-,-,-),
                  desc:html("Internal use only, needed to exchange information between visualization.ecl and visualize_tree.ecl"),
                  see_also:[current_node_cnt/1,
                            set_node_cnt/1]]).
:-export(new_node_cnt/4).

:-use_module(vis_structures).
:-use_module(vis_options).


% counting nodes in tree
:-local variable(node_cnt).

new_node_cnt(Handle,Id,Parent,Stream):-
        Handle = visualization{parent:Parent,tree_stream:Stream},
        incval(node_cnt),
        getval(node_cnt,Id),
        setarg(parent of visualization,Handle,Id).

current_node_cnt(Id):-
        getval(node_cnt,Id).

set_node_cnt(N):-
        setval(node_cnt,N).

