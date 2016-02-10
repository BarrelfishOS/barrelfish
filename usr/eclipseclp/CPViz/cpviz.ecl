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
% The Original Code is  ECLiPSe link to CPViz Constraint Visualization System
% The Initial Developer of the Original Code is  Joachim Schimpf
% Portions created by the Initial Developer are
% Copyright (C) 2013 Joachim Schimpf
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------

:-module(cpviz).

:-comment(author,"Joachim Schimpf").
:-comment(status,"experimental").
:-comment(copyright,"2013, Joachim Schimpf").
:-comment(categories,["Development Tools","Visualisation"]).
:-comment(summary, "Wrapper library, subsuming all of CP-Viz").
:-comment(desc, html("<P>
    This library loads and reexports library(visualization) and
    library(visualize_tree), making all features of CP-Viz available.
    In addition, it provides convenience predicates viz/2 and viztool/0
    for executing CP-Viz's Java-based tools from within ECLiPSe.
</P><P>
    Usage examples can be found under doc/examples/cpviz/ in your ECLiPSe
    installation.  The document 'CP Visualizer Format' is in the doc/
    directory.  Further background documents on CP-Viz are
    http://4c.ucc.ie/~hsimonis/cpviz-cp2010-paper.pdf,
    http://4c.ucc.ie/~hsimonis/cpviz-cp2010-slides.pdf
    and the project web site is http://sourceforge.net/projects/cpviz .
</P><P>
    The following is a code example.  After running sudoku(M), launch viztool
    and open the file aaa.idx in the OUTPUT subdirectory.
<PRE>
:-lib(ic).
:-import alldifferent/1 from ic_global_gac.
:-lib(cpviz).

sudoku(Matrix):-
        problem(Matrix),
        Matrix :: 1..9,
        create_visualization([],Handle),
        add_visualizer(Handle, domain_matrix(Matrix), [display:expanded]),
        draw_visualization(Handle),
        ( for(I,1,9), param(Matrix,Handle) do
            alldifferent(Matrix[I,1..9]),
            draw_visualization(Handle,[focus(1,row(I))]),
            alldifferent(Matrix[1..9,I]),
            draw_visualization(Handle,[focus(1,col(I))])
        ),
        ( multifor([I,J],[1,1],[7,7],[3,3]), param(Matrix,Handle) do
            alldifferent(flatten(Matrix[I..I+2,J..J+2])),
            draw_visualization(Handle,[focus(1,block(I,J,3,3))])
        ),
        extract_array(Handle,row,Matrix,NamedList),
        root(Handle),
        search(NamedList,1,input_order,tree_indomain(Handle,Handle), complete,[]),
        solution(Handle),
        close_visualization(Handle),
        viz(Handle, _).
        
problem([]([](4, _, 8, _, _, _, _, _, _), 
           [](_, _, _, 1, 7, _, _, _, _), 
           [](_, _, _, _, 8, _, _, 3, 2), 
           [](_, _, 6, _, _, 8, 2, 5, _), 
           [](_, 9, _, _, _, _, _, 8, _), 
           [](_, 3, 7, 6, _, _, 9, _, _), 
           [](2, 7, _, _, 5, _, _, _, _), 
           [](_, _, _, _, 1, 4, _, _, _), 
           [](_, _, _, _, _, _, 6, _, 4))).
</PRE>
</P>"
)).

:- reexport visualize_tree.
:- lib(vis_structures).

:- tool(mylibdir/1,mylibdir_/2).


:- comment(viz/2, [
	summary:"Run the CP-Viz viz program to convert xml traces to graphics",
	amode:(viz(+,?) is det),
	args:["Handle":"A (just closed) visualisation handle",
	    "ToolSpecs":"Configuration options for the output to produce"
	],
        desc:html("<P>
    This predicate provides a convenient interface to CP-Viz's 'viz' program,
    which converts XML trace log files into various drawing formats.  It is
    meant to be invoked just after the trace files have been produced.
    Afterwards, the resulting output may be viewed using viztool.
</P><P>
    The Handle argument should be the handle of a finished tracing session,
    which must have been closed already via close_visualization/1.
</P><P>
    The 'viz' program needs a configuration.xml file with options.  If such
    a file is present in the directory where the trace log files are located,
    and you want to use it, then set ToolSpecs to [].
    Otherwise, a configuration file will be created from the ToolSpecs.
    ToolSpecs is a list of tool-structures with the following arguments
    (all arguments except 'show' are optional):
<DL>
    <DT>show</DT><DD>
        tree or viz
</DD>
    <DT>type</DT><DD>
        layout (default), distribution, treemap, graph, values
</DD>
    <DT>display</DT><DD>
        compact (default) or expanded.  This controls whether failed
	subtrees are displayed in compact form.
</DD>
    <DT>repeat</DT><DD>
        all, final, or a positive or negative integer
</DD>
    <DT>width</DT><DD>
        positive integer (width of SVG canvas in pixels, default 700)
</DD>
    <DT>height</DT><DD>
        positive integer (height of SVG canvas in pixels, default 700)
</DD>
</DL>
    Alternatively, ToolSpecs can remain completely uninstantiated,
    in which case it defaults to the following two-element list:
<PRE>
        [tool{show:tree},tool{show:viz}]
</PRE>
    For the meaning of the options refer to the CP-Viz documentation.
</P><P>
    After processing the log files with viz/2, you can start viztool/0 and
    view the results, which are located in the directory specified by Handle
    (default 'OUTPUT'). Open the aaa.idx file.
</P>"),
        eg:"
    % for using default configuration settings:
    ...
    close_visualization(Handle),
    viz(Handle, _).

    % with specific configuration settings:
    ...
    close_visualization(Handle),
    viz(Handle, [
            tool{show:tree,display:expanded},
            tool{show:viz,width:1000,height:1000}
        ]).
",
	see_also:[create_visualization/2,close_visualisation/1,viztool/0]
    ]).

:- export viz/2.
viz(Handle, ToolSpecs) :-
	make_config(Handle, ToolSpecs, Files),
	exec_viz(Files).


% Make a configuration file for viz

:- export struct(tool(show,type,display,repeat,width,height,fileroot)).

make_config(Handle, Tools, [ConfigFile,TreeLog,VizLog]) :-

	check_handle(Handle, Dir, TreeLog, VizLog),

	( nonvar(Tools) -> true ;
	    Tools = [tool{show:tree},tool{show:viz}]
	),
	( is_list(Tools) ->
	    ( foreach(Tool,Tools), param(Handle) do
		proper_tool_spec(Tool, Handle)
	    )
	;
	    printf(error, "Illegal CP-Viz tool configuration (list expected):%n%w%n", [Tools]),
	    abort
	),

	concat_string([Dir,"/configuration.xml"], ConfigFile),
	( Tools==[] ->
	    % Check for existing configuration.xml file
	    ( existing_file(ConfigFile, [""], [readable], _) -> true ;
		printf(error,
		    "Cannot find readable config file %w%n", [ConfigFile]),
		abort
	    )
	;
	    % Create configuration.xml file
	    open(ConfigFile, write, S),
	    printf(S,
		    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>%n"
		    "<configuration version=\"1.0\" directory=\"%s\"%n"
		    "        xsi:noNamespaceSchemaLocation=\"configuration.xsd\"%n"
		    "        xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">%n",
		[Dir]),
	    ( foreach(Tool,Tools), param(S) do
		Tool = tool{show:Show,type:Type,display:Display,repeat:Repeat,
			    width:W,height:H,fileroot:Root},
		printf(S, 
		    "    <tool show=\"%w\" type=\"%w\" display=\"%w\" repeat=\"%w\"%n"
		    "          width=\"%w\" height=\"%w\" fileroot=\"%w\"/>%n",
		    [Show,Type,Display,Repeat,W,H,Root])
	    ),
	    printf(S, "</configuration>%n", []),
	    close(S)
	).


% Check visualisation handle and extract directory and file names
check_handle(Handle, Dir, TreeLog, VizLog) :-
	nonvar(Handle),
	Handle = visualization{root:VizBase,tree_root:TreeBase,
		output:Dir,stream:VizStream,tree_stream:TreeStream},
	( \+current_stream(VizStream), \+current_stream(TreeStream) ->
	    true
	;
	    printf(error,
	    	"Please call close_visualization/1 before running viz/2%n", []),
	    abort
	),
	concat_string([Dir,"/",VizBase,".viz"], VizLog),
	concat_string([Dir,"/",TreeBase,".tre"], TreeLog),
	( existing_file(VizLog, [""], [readable], _),
	  existing_file(TreeLog, [""], [readable], _) ->
	    true
	;
	    printf(error,
		"Cannot find readable log files %w and %w%n", [TreeLog,VizLog]),
	    abort
	).


% Check tool options and fill in defaults
proper_tool_spec(tool{show:Show,type:Type,display:Display,repeat:Repeat,
	    width:W,height:H,fileroot:Root},
	    visualization{root:VizRoot,tree_root:TreeRoot}) :-
	( Show==tree
	; Show==viz
	),
	( Type=layout -> true	% default
	; Type==distribution	% documented (but not implemented?)
	; Type==treemap		% documented (but not implemented?)
	; Type==graph		% implemented
	; Type==values		% implemented
	),
	( Display=compact -> true
	; Display==expanded
	),
	( var(Repeat) -> (Show==tree,Repeat=all ; Show==viz,Repeat=final)
	; Repeat==all
	; Repeat==final
	; integer(Repeat)
	),
	( var(W) -> W=700
	; integer(W), W>=50
	),
	( var(H) -> H=700
	; integer(H), H>=50
	),
	( var(Root) -> (Show==tree,Root=TreeRoot ; Show==viz,Root=VizRoot)
	; atom(Root)
	; string(Root)
	),
	!.
proper_tool_spec(Tool, _) :-
	printf(error, "Illegal CP-Viz tool configuration option:%n%w%n", [Tool]),
	abort.


%:- comment(exec_viz/1, [
%	summary:"Launch the CP-Viz viz program to convert xml traces to graphics",
%	args:["Args":"Command line arguments"]
%    ]).
:- export exec_viz/1.
exec_viz(Args) :-
	is_list(Args), ground(Args),
	% set up command according to architecture
	mylibdir(LibDir),
	concat_string([LibDir, "viz.jar"], VizJar),
	get_java_command("ie.ucc.cccc.viz.Viz", [VizJar], Args, Command),
	% execute command 
	exec(Command, [null,Out,Err], Pid),
	echo_blocking(Out, output),
	close(Out), flush(output),
	echo_blocking(Err, error),
	close(Err), flush(error),
	wait(Pid, Status),
	( Status == 0 -> true ;
	    printf(error, "CP-Viz: viz-command returned with status %w%n", [Status]),
	    abort
	).


%--------------------------------------------------
% Run viztool
%--------------------------------------------------

:- comment(viztool/0, [
	summary:"Launch the CP-Viz viztool for viewing trace log visualisations."
	" In TkECLiPSe, this can be done from the Tools menu."
    ]).
:- export viztool/0.
viztool :-
	mylibdir(LibDir),
	viztool(LibDir, LibDir, LibDir, _Pid).

viztool(LibDir, BatikLibDir, JavaHelpDir, Pid) :-

	% Work around viztool bug: create a history.hist file
	( get_flag(cwd_scope, OldCwdScope) ->
	    set_flag(cwd_scope, process),
	    getcwd(ProcessCwd),
	    set_flag(cwd_scope, OldCwdScope)
	;
	    getcwd(ProcessCwd)	% for pre-6.1 ECLiPSe
	),
	concat_strings(ProcessCwd, "history.hist", HistFile),
	catch((open(HistFile, append, HS), close(HS)), _, true),

	% Build classpath list
	findall(Jar, (
		concat_string([LibDir, "viztool.jar"], Jar)
	    ;
	    	batik_jar(Base),
		concat_string([BatikLibDir,Base,".jar"], Jar)
	    ;
		javahelp_jar(Base),
		concat_string([JavaHelpDir,Base,".jar"], Jar)
	    ), Jars),

	get_java_command("components.InternalFrame", Jars, [], Command),

	% execute command 
	exec(Command, [], Pid).


    javahelp_jar("jhall").

    batik_jar("batik-anim").
    batik_jar("batik-awt-util").
    batik_jar("batik-bridge").
    batik_jar("batik-css").
    batik_jar("batik-dom").
    batik_jar("batik-ext").
    batik_jar("batik-gvt").
    batik_jar("batik-gui-util").
    batik_jar("batik-parser").
    batik_jar("batik-script").
    batik_jar("batik-svg-dom").
    batik_jar("batik-swing").
    batik_jar("batik-util").
    batik_jar("batik-xml").
    batik_jar("xml-apis-ext").

	
%--------------------------------------------------
% Auxiliaries
%--------------------------------------------------

get_java_command(MainClass, ClassPathList, Args,
		[Java,
		%"-Djava.compiler=",	% turn off jit compiler
		%"-Xrunhprof:cpu=times,heap=all,file=/tmp/java.hprof.txt", % turn on profiler
		Size,
		"-Djava.net.preferIPv4Stack=true",
		"-classpath", ClassPath,
		%"-Xdebug",
		%"-Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=n",
		MainClass|Args]
	    ) :-

	get_flag(hostarch, ARCH), 
	( getenv("JRE_HOME", JRE_HOME_OS)  ->
	    os_file_name(JRE_HOME, JRE_HOME_OS)
        ;
             writeln(error, "Can't find JRE: Environment variable JRE_HOME, specifying location of the Java"),
             writeln(error, "Runtime Environment, has not been set."), 
	    abort
        ),

	concat_string([JRE_HOME, "/bin/"], JavaBin),
	( existing_file(JavaBin, ['javaw.exe','java.exe',java], [executable], Java) ->
	    true
	;
             printf(error, "Can't find Java executable in %w%n", [JavaBin]), 
	     abort
	),

        java_vm_size(JVM_SIZE),
	concat_string(["-Xmx", JVM_SIZE, "m"], Size),

	concat_string([JRE_HOME, "/lib/rt.jar"], RtJar),
	append(ClassPathList, [RtJar], ClassPathList1),
	( foreach(CP,ClassPathList1), foreach(CPOS,CPOSs) do
	    os_file_name(CP, CPOS)
	),
	java_path_sep(ARCH, Sep),
	join_string(CPOSs, Sep, ClassPath).

java_path_sep("i386_nt", (;)) :- !.
java_path_sep("x86_64_nt", (;)) :- !.
java_path_sep(_, (:)).

% max size of the virtual machine in megabytes
java_vm_size(256).


% Copy InStream to OutStream until end of stream
echo_blocking(InStream, OutStream) :-
	get(InStream, Byte),
	( Byte == -1 ->
	    true
	;
	    put(OutStream, Byte),
	    ( Byte == 10 -> flush(OutStream) ; true ),
	    echo_blocking(InStream, OutStream)
	).


% Find this module's source (or eco) file location
mylibdir_(Dir, Module) :-
	atom_string(Module, BaseName),
	current_compiled_file(Path, _Time, _Module),
	pathname(Path, Dir, BaseName, _Suf),
	!.

