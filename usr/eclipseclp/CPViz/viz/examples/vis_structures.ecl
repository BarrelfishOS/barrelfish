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
:-module(vis_structures).
:-comment(author,"Helmut Simonis").
:-comment(status,"experimental").
:-comment(copyright,"2010, Helmut Simonis").
:-comment(categories,["Development Tools","Visualisation"]).
:-comment(summary,"Structure definition for visualization").
:-comment(description,"Provides the different structures required for visualization. Also defines default values for attributes.").

:-comment(argument_number/4,[summary:"Helper to access attributes of visualization and visualizer terms",
                  args:["Functor":"returns the current node id number",
                        "AttributeName":"atom, attribute name",
                        "AttributeNumber":"variable, will be instantiated to the argument number of the attribute name searched for",
                        "Default":"default value used for attribute; value system(_) states that value is set by system not user,"],
                  amode:argument_number(++,++,-,-),
                  desc:html("Internal use only"),
                  see_also:[]]).

:-export(argument_number/4).

:-comment(struct(visualization),[summary:"Defines the data structure for the visualization",
                                 fields:["root":"atom (default vis), rootname of the visualization files",
                                         "tree_root":"atom (default tree)",
                                         "output":"atom/string (default OUTPUT), name of directory where log files will be placed ",
                                         "ignore_fixed":"yes/no (default yes), states if fixed assignments will be ignored and not create tree nodes",
                                         "var_arg":"integer, argument number used to retrieve variables to be assigned",
                                         "name_arg":"integer, argument number used to retrieve name of variable to be assigned",
                                         "focus_arg":"integer, argument number used to retrieve index number of variables to be assigned",
                                         "parent":"integer, current parent node id",
                                         "stream":"file descriptor of visualization log file",
                                         "schema_path":"Path to the schema description, can be URI or path relative to output directory",
                                         "range_from":"integer, only start visualization from this step on",
                                         "range_to":"integer, stop visualization when this step is reached",
                                         "tree_stream":"file descriptor of tree log file",
                                         "visualizers":"open list of visualizer terms attached to visualization"
                                         ],
                                 desc:html("This structure is used to contain all information about the visualization. The visualizers list is an open list, e.g. it ends with a variable, not the empty list. This allows to add more visualizers at any time point, but requires care when iterating over the visualizers."),
                                 see_also:[argument_number/4]
                                ]).

:-export struct(visualization(root,
                              tree_root,
                              output,
                              ignore_fixed,
                              var_arg,
                              name_arg,
                              focus_arg,
                              parent,
                              stream,
                              schema_path,
                              range_from,
                              range_to,
                              tree_stream,
                              visualizers)).

:-comment(struct(visualizer),[summary:"describes the data structures for the visualizers",
                              fields:["id":"integer number of visualizer",
                                      "type":"term, contains call pattern to be visualized",
                                      "type_name":"atom, name of item to be visualized",
                                      "display":"atom (default minimal), influences how the visualizer will be shown",
                                      "x":"integer (default 0), x position in visualization where visualizer will be placed",
                                      "y":"integer (default 0),y position in visualiation where visualizer will be placed",
                                      "group":"group id number used for this visualizer",
                                      "width":"optional width of visualizer",
                                      "height":"optional height of visualizer",
                                      "min":"optional minimal value of variables",
                                      "max":"optional maximal value of variable"
                                     ],
                              desc:html("This structure is used to store information about each visualizer. Some attributes can be set by the user (defined through argument_number/4"),
                              see_also:[argument_number/4]
                             ]).

:-export struct(visualizer(id,
                           type,
                           type_name,
                           display,
                           x,
                           y,
                           group,
                           width,
                           height,
                           min,
                           max)).

argument_number(visualization,root,root of visualization,vis).
argument_number(visualization,tree_root,tree_root of visualization,tree).
argument_number(visualization,output,output of visualization,"OUTPUT").
argument_number(visualization,schema_path,schema_path of visualization,"../../../documentation").
argument_number(visualization,ignore_fixed,ignore_fixed of visualization,yes).
argument_number(visualization,visualizers,visualizers of visualization,
                system(_)).
argument_number(visualization,var_arg,var_arg of visualization,
                system(_)).
argument_number(visualization,name_arg,name_arg of visualization,
                system(_)).
argument_number(visualization,focus_arg,focus_arg of visualization,
                system(_)).
argument_number(visualization,parent,parent of visualization,0).
argument_number(visualization,stream,stream of visualization,
                system(_)).
argument_number(visualization,range_from,range_from of visualization,
                0).
argument_number(visualization,range_to,range_to of visualization,
                3000).
argument_number(visualization,tree_stream,tree_stream of visualization,
                system(_)).

argument_number(visualizer,display,display of visualizer,minimal).
argument_number(visualizer,x,x of visualizer,0).
argument_number(visualizer,y,y of visualizer,0).
argument_number(visualizer,group,group of visualizer,system(_)).
argument_number(visualizer,width,width of visualizer,system(_)).
argument_number(visualizer,height,height of visualizer,system(_)).
argument_number(visualizer,min,min of visualizer,system(_)).
argument_number(visualizer,max,max of visualizer,system(_)).

