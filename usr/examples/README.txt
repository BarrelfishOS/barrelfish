This directory contains some simple example programs that show how to
do specific things in barrelfish. 

In order to build an example you add it to platforms/Hakefile under
modules_common.  For example for the hello example:


modules_common = [ "/sbin/" ++ f | f <- [
    "init"
	...
	"examples/xmpl-hello"
    ... ]]

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
