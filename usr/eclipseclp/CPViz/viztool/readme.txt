
			Constraint Visualiser               
                
The visualiser uses Batik (http://xmlgraphics.apache.org/batik/index.html)
the open source SVG toolkit for displaying and manipulating the SVG 
files. The svgbrowser sample, which is included in the Batik source 
distribution (under the apps directory)was very useful in designing 
the visualiser. Some code from this application has been included in 
the visualiser. 

Notes


1. To import the project into eclipse by selecting 
file->import->Existing projects into workspace.

2. To execute within eclipse:
When you right click on the ConstraintVisualiser in the Package Explorer  and select run as java application you will 
get a dialog with several run time options - select InternalFrame - components.

3. To create an executable jar from the sources
Select the project in the Project Explorer. Select File->Export and select runnable jar under java. Under configuration select 
Internalframe - ConstraintVisualiser  and any directory for the destination.

4. Bug in file open dialog
There is a bug in the (JFileChooser) java file open dialog on windows 
when it tries to expand zip directories. This has been fixed 
 http://bugs.sun.com/view_bug.do?bug_id=6578753
 
But it seemed to reappear in other java releases. There is the
following work around
 //get rid of zip file expandsion
                // regsvr32 /u %windir%\system32\zipfldr.dll 
                // regsvr32 %windir%\system32\zipfldr.dll 
                
5.	Menu system
The framework code for building an extensible menu system in the 
svgbrowser was initialled used in the Visualiser (see GUI.properties 
under src/components/resources for details).  But this was replaced by 
a simpler hard-code system, which easily allows the insertion of the 
four most recently opened SVG files under the File menu. 

6. The .idx file
The Visualiser opens an .idx file e.g.
tree_expanded_1.svg frame1.svg
tree_expanded_2.svg frame2.svg
tree_expanded_3.svg frame3.svg

Where each line has the name of the search tree followed by the name 
of file showing the state of the variables.


7.  The Help system.
The help system is based on javahelp see 
http://java.sun.com/javase/technologies/desktop/javahelp/download_binary.html#userguide
for more details.

The main Javahelp table of contents file is under src/javahelp.docs included below for convenience

<toc version="2.0">
	<tocitem text="About" target="About" />
	<tocitem text="Introduction" target="Introduction" />
	<tocitem text="Main Menu">
		<tocitem text="File" target="File" />
		<tocitem text="Help" target="Help" />
	</tocitem>
	<tocitem text="ToolBar" target="Toolbar" />
	<tocitem text="Image manipulation">
		<tocitem text="Buttons" target="buttons" />
		<tocitem text="Mouse Actions" target="Mouse Actions" />
	</tocitem>
</toc>

8. history.hist

This file is created in the directory where the executable jar file is 
run from. This is brain-dead as we may not be able to write there. This file needs 
to be created with File.createTempFile("history",".hist") if it does
not exist already. If it does exist in the temp directory then we use this one.
