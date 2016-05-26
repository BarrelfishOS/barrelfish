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
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 2001 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Josh Singer, Parc Technologies
% 
% END LICENSE BLOCK

:- module(java_vc).
:- use_module(vc_support).
:- use_module(graphviz_license).

:- export start_vc/1, stop_vc/1, find_java/1.

:- comment(categories, ["Interfacing","Visualisation"]).
:- comment(summary, 
	   "Module for the Java Visualisation Client").

:- comment(author, "Josh Singer").

:- comment(copyright, "Cisco Systems, Inc.").

:- comment(desc, html("<P> Module for the management of the Java Visualisation Client (VC) from ECLiPSe. The Java VC is an extensible, customisable, graphical visualisation tool for ECLiPSe implemented in Java. A new Java VC can be started using the predicate <code>start_vc/1</code>. While the Java VC is running, it will react automatically to the creation of <i>viewables</i> during ECLiPSe execution. For more information on how to declare and use viewables see the documentation for <code>lib(viewable)</code>. The Java VC can be terminated from ECLiPSe using the predicate <code>stop_vc/1</code>.
           
")).


:- comment(start_vc/1, 
	   [amode : start_vc(-), 
	    args : ["VisClientName" : 
		 "An atom; the name of the new visualisation client."],
	    summary : "Start a new Java Visualisation Client",
	    desc : html("This predicates starts a new Java Visualisation Client (VC). The VC survives backtracking over the <code>start_vc/1</code> goal, and persists until it is explicitly terminated. <i>VisClientName</i> becomes instantiated to an atom which can be used when the VC is to be stopped. <p>For <code>start_vc</code> to work correctly, ECLiPSe needs to know where to find the Java installation. To check whether this is so, use <code>find_java/1</code>. If the result of <code>find_java/1</code> is not the root directory of a Java installation (at least version 1.2.2) then the behaviour of <code>start_vc/1</code> prints an error message and fails.    

	"), 
	    see_also : [stop_vc/1, find_java/1],
            resat: no]).
start_vc(VisClientName):-
	Host = localhost,	% avoid firewall problems
	remote_connect_setup(Host/Port, Peer, Socket), 
	start_RemoteVisClient(Host, Port, StdOut), 
        (remote_connect_accept(Peer, Socket, 60, _, "", _) ->
             true
        ;
             writeln(error, "Unable to connect to Remote Java process...."),
             abort
        ),
	read_exdr(StdOut, VisClientName). 
	

:- comment(stop_vc/1, 
	   [amode : stop_vc(+), 
	    args : ["VisClientName" : 
		 "An atom; the name of the client to be terminated."],
	    summary : "Terminate a running Java visualisation client.", 
	    desc: html("Terminate a running Java visualisation client which was started using <code>start_vc/1</code>. The name atom is displayed at the top of the client's window."),
	    see_also : [start_vc/1],
            resat: no
]).
stop_vc(VisClientName):-
	vc_support:vis_client_request_termination(VisClientName).

start_RemoteVisClient(Host, Port, VisClientName):-
	% set up command according to architecture
	get_java_command(
	    "com.parctechnologies.eclipse.visualisation.RemoteVisClient",
	    [Host, Port],
	    Command),
	% execute command 
	run_java_command(Command, VisClientName).

:- comment(find_java/1, 
	   [amode : find_java(-), 
	    args : ["JavaLocation" : 
		 "A string: the path of the known Java Runtime Environment installation."],
	    summary : "Determine the path of the known Java installation.", 
	    desc: html("If this predicate succeeds, JavaLocation is instantiated to the root directory of the Java Runtime Environment which ECLiPSe will use to run a new Java VC. If the predicate fails, this means that to use the you need to install a Java Runtime Environment (JRE) on your computer (at least version 1.2.2) and then re-run the installation script. "),
            resat: no
]).
find_java(JRE_HOME):-
	getenv("JRE_HOME", JRE_HOME), 
	!.


get_java_command(MainClass, Args,
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
	get_flag(installation_directory, ECLIPSEDIR),
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

	concat_string([ECLIPSEDIR, "/lib/eclipse.jar"], EclipseJar),
	os_file_name(EclipseJar, EclipseJarOS),
	concat_string([ECLIPSEDIR, "/lib/grappa1_2.jar"], GrappaJar),
	os_file_name(GrappaJar, GrappaJarOS),
	concat_string([ECLIPSEDIR, "/lib/visualisation.jar"], VizJar),
	os_file_name(VizJar, VizJarOS),
	concat_string([JRE_HOME, "/lib/rt.jar"], RtJar),
	os_file_name(RtJar, RtJarOS),

	java_path_sep(ARCH, Sep),
	concat_string([EclipseJarOS,Sep,GrappaJarOS,Sep,VizJarOS,Sep,RtJarOS,Sep], ClassPath).


    java_path_sep("i386_nt", (;)) :- !.
    java_path_sep("x86_64_nt", (;)) :- !.
    java_path_sep(_, (:)).

    % max size of the virtual machine in megabytes
    java_vm_size(256).


% The NT case has to be run from <eclipse>/lib/i386_nt so that the DLLs
% are available to the embedded Eclipse. 

run_java_command(TestCommand, StdOut):-
	get_flag(hostarch, ARCH),
	( (ARCH = "i386_nt" ; ARCH = "x86_64_nt") ->
	    getcwd(CWD),
	    get_flag(installation_directory, ECLIPSEDIR),
	    concat_string([ECLIPSEDIR, "/lib/", ARCH], LIBDIR),
	    cd(LIBDIR), 	
	    exec(TestCommand, [null, StdOut, null], _),
	    cd(CWD)
	;
	    % assume can run Unix-like for other archs.
	    exec_group(TestCommand, [null, StdOut, null], _)
	).

