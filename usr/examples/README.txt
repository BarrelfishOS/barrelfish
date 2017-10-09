This directory contains some simple example programs that show how to
do specific things in barrelfish. 

The example programs are now available as modules_xmpl in
platforms/Hakefile.  The X86_64_Full target builds the example programs
by default now.

For other architectures you may have to add either the modules_xmpl list
or just the individual example you wish to build to the architecture's
build target.  For example to build the hello example for the
PandaboardES:

pandaModules = [ "/sbin/" ++ f | f <- [
    "cpu_omap44xx",
    "init"
    ...
    "examples/xmpl-hello" ]]

(Note there is an exception for xmpl-thc-lang.  See the README.txt file 
in that directory for more information).

For each example we also include a sample menu.lst for running that
example.

For the examples that use message interfaces we also include the .if
file in the xample directory.  The source file should already be in
the /if directory and appropriate entries made in the /if/Hakefile.  If
they aren't then you will have to copy them over and edit the Hakefile
appropriately.

For some examples there is also a README.txt file that explains a bit
more about it.
