// BEGIN LICENSE BLOCK
// Version: CMPL 1.1
//
// The contents of this file are subject to the Cisco-style Mozilla Public
// License Version 1.1 (the "License"); you may not use this file except
// in compliance with the License.  You may obtain a copy of the License
// at www.eclipse-clp.org/license.
// 
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
// the License for the specific language governing rights and limitations
// under the License. 
// 
// The Original Code is  The ECLiPSe Constraint Logic Programming System. 
// The Initial Developer of the Original Code is  Cisco Systems, Inc. 
// Portions created by the Initial Developer are
// Copyright (C) 2002 - 2006 Cisco Systems, Inc.  All Rights Reserved.
// 
// Contributor(s): Andrew Cheadle, IC-Parc
// 
// END LICENSE BLOCK

/*
 * EclipseMapColourer.java
 *
 * Program:   ECLiPSe Java Map Colouring Demo
 * System:    ECLiPSe Constraint Logic Programming System
 * Author/s    Andrew Cheadle, IC-Parc 
 */


// Class imports
import java.awt.*;
import java.awt.event.*;
import javax.swing.event.*;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JMenuBar;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JLabel;
import javax.swing.JSlider;
import javax.swing.border.BevelBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.ImageIcon;
import java.io.*;
import java.awt.image.*;
import java.util.Vector;
import java.awt.geom.Rectangle2D;
import javax.swing.filechooser.FileFilter;
import com.parctechnologies.eclipse.*;

// The application is implemented by extending the
// swing JFrame class.
public class EclipseMapColourer extends JFrame 
{
  // Class constructor
  public EclipseMapColourer()
  {
    // Construct a JFrame with the appropriate title
    super("Java ECLiPSe Map Colouring Demo");
    
    // Build and layout the main map colouring frame
    buildApplicationFrame();

    // Build and layout the map resizing dialog
    buildmapSizeDialog();

    // Create a Map file chooser
    mapFileChooser = new JFileChooser();

    // Add a file filter to the MAP file selector
    mapFileChooser.addChoosableFileFilter(new MapFileFilter());

    // Colouring thread is not running
    colouringThread = null;
  }
    
  // Build and layout the main map colouring frame
  private void buildApplicationFrame()
  {
    // Control Panel 
    controlButtonPanel = new JPanel();
    
    runButton = new JButton();
    // When the 'Run' button is clicked start map colouring
    runButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          startSearch();
        }
      });
    
    moreButton = new JButton();
    // When the 'More' button is clicked continue map colouring
    moreButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          continueSearch();
        }
      });
    
    finishButton = new JButton();
    // When the 'Finish' button is clicked terminate map colouring
    finishButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          endSearch();
        }
      });
    
    // Default ImageBuffer - blank
    imageBuffer = new BufferedImage(100, 100,
                                    BufferedImage.TYPE_INT_ARGB);
    
    // Panel for displaying (coloured) map
    mapPanel = new MapPanel();
    
    // Panel and text are for status bar
    statusPanel = new JPanel();
    statusPanel.setLayout(new GridLayout(1, 1));
    statusPanel.setBorder(new BevelBorder(BevelBorder.LOWERED));
    statusTextArea = new JTextArea();
    statusTextArea.setBackground(new Color(204, 204, 204));
    statusTextArea.setEditable(false);
    statusTextArea.setText(" Status: ");
    statusTextArea.setToolTipText("Status of Map colouring");
    statusPanel.add(statusTextArea);

    // Menus
    menuBar = new JMenuBar();

    // File, New Map, Map Size..., Exit
    fileMenu = new JMenu();
    newMapMenuItem = new JMenuItem();
    mapSizeMenuItem = new JMenuItem();
    exitMenuItem = new JMenuItem();

    // Method
    methodMenu = new JMenu();

    // Solver
    solverMenu = new JMenu();
    solverButtonGroup = new ButtonGroup();
    fdMenuItem = new JRadioButtonMenuItem();
    icMenuItem = new JRadioButtonMenuItem();
    delayTilGrndMenuItem = new JRadioButtonMenuItem();
    prologMenuItem = new JRadioButtonMenuItem();

    // Value Choice
    valueChoiceMenu = new JMenu();
    valueChoiceButtonGroup = new ButtonGroup();
    indomainMenuItem = new JRadioButtonMenuItem();
    indomainRandomMenuItem = new JRadioButtonMenuItem();
    rotateColoursMenuItem = new JRadioButtonMenuItem();

    // Variable Selection
    variableSelectionMenu = new JMenu();
    variableSelectionButtonGroup = new ButtonGroup();
    inputOrderMenuItem = new JRadioButtonMenuItem();
    firstFailMenuItem = new JRadioButtonMenuItem();
    occurrenceMenuItem = new JRadioButtonMenuItem();
    mostConstrainedMenuItem = new JRadioButtonMenuItem();
    antiFirstFailMenuItem = new JRadioButtonMenuItem();

    // Help
    helpMenu = new JMenu();
    aboutMenuItem = new JMenuItem();
    aboutMenuItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          JOptionPane.showMessageDialog(null, "JAVA ECLiPSe Map Colouring " +
                                        "Demo\nCopyright: Cisco Systems Inc, 2002\n",
                                        "About ECLiPSe Map Colourer...", 
                                        JOptionPane.PLAIN_MESSAGE, 
                                        new ImageIcon("ic-parc.gif")); 
        }
      });
    
    // Configure menus
    setJMenuBar(menuBar);

    // File, New Map, Map Size..., Exit
    fileMenu.setText("File");
    newMapMenuItem.setText("New Map...");
    newMapMenuItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          chooseMapFile();
        }
      });
    fileMenu.add(newMapMenuItem);
    mapSizeMenuItem.setText("Map Size...");
    mapSizeMenuItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          mapSizeDialog.show();
        }
      });
    fileMenu.add(mapSizeMenuItem);
    fileMenu.addSeparator();
    exitMenuItem.setText("Exit");
    exitMenuItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          exitForm();
        }
      });
    fileMenu.add(exitMenuItem);
    menuBar.add(fileMenu);


    // Method
    methodMenu.setText("Method");
    menuBar.add(methodMenu);

    // Solver
    solverMenu.setText("Solver");
    methodMenu.add(solverMenu);

    // FD
    fdMenuItem.setSelected(true);
    fdMenuItem.setText("FD");
    fdMenuItem.setActionCommand("fd");
    solverMenu.add(fdMenuItem);
    solverButtonGroup.add(fdMenuItem);

    // IC
    icMenuItem.setText("IC");
    icMenuItem.setActionCommand("ic");
    solverMenu.add(icMenuItem);
    solverButtonGroup.add(icMenuItem);

    // Delay Until Ground
    delayTilGrndMenuItem.setText("Delay Until Ground");
    delayTilGrndMenuItem.setActionCommand("delay");
    solverMenu.add(delayTilGrndMenuItem);
    solverButtonGroup.add(delayTilGrndMenuItem);

    // Prolog
    prologMenuItem.setText("Prolog (Generate & Test)");
    prologMenuItem.setActionCommand("prolog");
    solverMenu.add(prologMenuItem);
    solverButtonGroup.add(prologMenuItem);

    // Value Choice
    valueChoiceMenu.setText("Value Choice");
    methodMenu.add(valueChoiceMenu);

    // Indomain
    indomainMenuItem.setText("In-Domain");
    indomainMenuItem.setSelected(true);
    indomainMenuItem.setActionCommand("indomain");
    valueChoiceMenu.add(indomainMenuItem);
    valueChoiceButtonGroup.add(indomainMenuItem);

    // Indomain_random
    indomainRandomMenuItem.setText("Random-In-Domain");
    indomainRandomMenuItem.setActionCommand("indomain_random");
    valueChoiceMenu.add(indomainRandomMenuItem);
    valueChoiceButtonGroup.add(indomainRandomMenuItem);

    // Rotate Colours
    rotateColoursMenuItem.setText("Rotate Colours");
    rotateColoursMenuItem.setActionCommand("rotate");
    valueChoiceMenu.add(rotateColoursMenuItem);
    valueChoiceButtonGroup.add(rotateColoursMenuItem);

    // Variable Selection
    variableSelectionMenu.setText("Variable Selection");
    methodMenu.add(variableSelectionMenu);

    // Input Order
    inputOrderMenuItem.setText("Input Order");
    inputOrderMenuItem.setSelected(true);
    inputOrderMenuItem.setActionCommand("input_order");
    variableSelectionMenu.add(inputOrderMenuItem);
    variableSelectionButtonGroup.add(inputOrderMenuItem);

    // First-Fail
    firstFailMenuItem.setText("First-Fail");
    firstFailMenuItem.setActionCommand("first_fail");
    variableSelectionMenu.add(firstFailMenuItem);
    variableSelectionButtonGroup.add(firstFailMenuItem);

    // Occurrence
    occurrenceMenuItem.setText("Occurrence");
    occurrenceMenuItem.setActionCommand("occurrence");
    variableSelectionMenu.add(occurrenceMenuItem);
    variableSelectionButtonGroup.add(occurrenceMenuItem);

    // Most Contrained
    mostConstrainedMenuItem.setText("Most Constrained");
    mostConstrainedMenuItem.setActionCommand("most_constrained");
    variableSelectionMenu.add(mostConstrainedMenuItem);
    variableSelectionButtonGroup.add(mostConstrainedMenuItem);

    // Anti-First-Fail
    antiFirstFailMenuItem.setText("Anti-First-Fail");
    antiFirstFailMenuItem.setActionCommand("anti_first_fail");
    variableSelectionMenu.add(antiFirstFailMenuItem);
    variableSelectionButtonGroup.add(antiFirstFailMenuItem);

    // Help
    helpMenu.setText("Help");
    menuBar.add(helpMenu);

    // About
    aboutMenuItem.setText("About");
    helpMenu.add(aboutMenuItem);

    // Configure window

    // Listen for the window close event
    addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent evt) {
          exitForm();
        }
      });
    
    // Set a grid layout for the control panel
    controlButtonPanel.setLayout(new GridLayout(1, 3));
    
    // Add the Run button
    runButton.setText("Run");
    runButton.setToolTipText("Start map colouring search");
    runButton.setBorder(new BevelBorder(BevelBorder.RAISED));
    controlButtonPanel.add(runButton);
    
    // Add the More button
    moreButton.setText("More");
    moreButton.setToolTipText("Find next map colouring solution");
    moreButton.setBorder(new BevelBorder(BevelBorder.RAISED));
    moreButton.setEnabled(false);
    controlButtonPanel.add(moreButton);
    
    // Add the Finish button
    finishButton.setText("Finish");
    finishButton.setToolTipText("Terminate map colouring search");
    finishButton.setBorder(new BevelBorder(BevelBorder.RAISED));
    finishButton.setEnabled(false);
    controlButtonPanel.add(finishButton);
    
    // Place the control panel at the top of the window
    getContentPane().add(controlButtonPanel, BorderLayout.NORTH);
  
    // Place the map panel in the center of the window
    getContentPane().add(mapPanel, BorderLayout.CENTER);
        
    // Place the status panel at the bottom of the window
    getContentPane().add(statusPanel, BorderLayout.SOUTH);

    // Lay everything out neatly
    pack();
  }

  // Build and layout the map resizing dialog
  private void buildmapSizeDialog()
  {
    // Map size and scaling modal dialog box
    mapSizeDialog = new JDialog(this, "Map Size", true);
    mapSizeDialog.setResizable(false);
    mapSizeDialog.getContentPane().setLayout(new GridLayout(1,1));
    mapSizeDialog.setSize(332, 94);
    
    // An etched frame to enclose the widgets
    mapSizePanel = new JPanel();
    mapSizePanel.setLayout(null);
    mapSizePanel.setBorder(new EtchedBorder());

    // Map sizing widgets
    mapSizeLabel = new JLabel();
    mapSizeLabel.setText("Map size:");
    mapSizeLabel.setBounds(10, 10, 58, 16);
    mapSizePanel.add(mapSizeLabel);

    mapSizeSlider = new JSlider();
    mapSizeSlider.setMinorTickSpacing(1);
    mapSizeSlider.setBounds(73, 10, 192, 16);
    mapSizeSlider.addChangeListener(new ChangeListener() {
        public void stateChanged(ChangeEvent e) {
          JSlider source = (JSlider)e.getSource();
          if (!source.getValueIsAdjusting()) {
            mapSizeTextField.setText("" + source.getValue());
          }
        }
      });
    mapSizePanel.add(mapSizeSlider);

    mapSizeTextField = new JTextField();
    mapSizeTextField.setBorder(new BevelBorder(BevelBorder.LOWERED));
    mapSizeTextField.setHorizontalAlignment(JTextField.CENTER);
    mapSizeTextField.setBounds(270, 8, 40, 20);
    mapSizePanel.add(mapSizeTextField);

    // Map scaling widgets
    mapScalingLabel = new JLabel();
    mapScalingLabel.setText("Map scale:");
    mapScalingLabel.setBounds(10, 40, 64, 16);
    mapSizePanel.add(mapScalingLabel);

    mapScalingTextField = new JTextField();
    mapScalingTextField.setBorder(new BevelBorder(BevelBorder.LOWERED));
    mapScalingTextField.setHorizontalAlignment(JTextField.CENTER);
    mapScalingTextField.setText("" + scalingFactor);
    mapScalingTextField.setBounds(80, 38, 110, 20);
    mapSizePanel.add(mapScalingTextField);

    // 'Ok' button closes window and re-sizes/re-scales the map
    mapSizeOkButton = new JButton();
    mapSizeOkButton.setText("Ok");
    mapSizeOkButton.setBorder(new BevelBorder(BevelBorder.RAISED));
    mapSizeOkButton.setBounds(206, 40, 50, 20);
    mapSizeOkButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          int newScalingFactor, newMapSize;
          
          newScalingFactor = (new Integer(mapScalingTextField.getText())).intValue();
          newMapSize = (new Integer(mapSizeTextField.getText())).intValue();
          resizeMap(newMapSize, newScalingFactor);
          mapSizeDialog.hide();
        }
      });
    mapSizePanel.add(mapSizeOkButton);

    // 'Close' button just closes the window
    mapSizeCancelButton = new JButton();
    mapSizeCancelButton.setText("Cancel");
    mapSizeCancelButton.setBorder(new BevelBorder(BevelBorder.RAISED));
    mapSizeCancelButton.setBounds(262, 40, 50, 20);
    mapSizeCancelButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          mapSizeDialog.hide();
        }
      });
    mapSizePanel.add(mapSizeCancelButton);
    
    mapSizeDialog.getContentPane().add(mapSizePanel);
  }

  // Exit the application - the window is closed or 'Exit' is selected
  private void exitForm()
  {
    // Stop the thread - otherwise System.exit() will wait for thread
    // completion, i.e. map colouring to complete
    if (colouringThread != null) {
      // Thread.stop() is deprecated, however, the thread is
      // only ever stopped at termination - the reasons cited
      // for not using Thread.stop() do not apply. Because of 
      // the Java - ECLiPSe control model, it is better to do this
      // than litter the code with checks for an 'exit variable'
      // becoming 'true' and the exit not being actioned until
      // control returns from ECLiPSe to one of these cancellation
      // points.
      colouringThread.stop();
    }
    
    try {
      // Destroy the Eclipse
      ((EmbeddedEclipse) eclipse).destroy();
    }
    catch(Exception expn) {
      System.out.println("Exception caught whilst " +
                         "destroying ECLiPSe engine: " +
                         expn.getMessage());
    }

    // Exit!!
    System.exit(0);
  }
  
  // Open the map file selector dialog and on selection
  // of a file compile the map data
  private void chooseMapFile() 
  {
    int returnVal = mapFileChooser.showOpenDialog(this);
    if(returnVal == JFileChooser.APPROVE_OPTION) {
      compileMapData(mapFileChooser.getSelectedFile().getAbsolutePath());
    }
  }
  
  // The 'Run' button has been clicked, initiate map 
  // colouring solution search
  private void startSearch() 
  {
    // Set the status
    statusTextArea.setText(" Status: Colouring map - searching for a solution");

    // Kick off a new thread that passes control to the ECLiPSe engine
    // and waits for completion of the 'colouring' predicate. This must 
    // be done in a new thread otherwise the Java AWT event handling thread
    // will be used and whilst ECLiPSe has control. As a result the user won't 
    // be able to perform any interaction and the map will not be updated with 
    // colours in real-time.
    (colouringThread = new Thread() {
        public void run() {

          CompoundTerm result;
          solutionCount = 0;

          // Disable operations that are not allowed whilst colouring is 
          // in progress.
          runButton.setEnabled(false);
          methodMenu.setEnabled(false);
          newMapMenuItem.setEnabled(false);
          mapSizeMenuItem.setEnabled(false);
          
          // Pass control to the embedded ECLiPSe engine and execute 
          // the 'colouring' predicate
          try {
	    Object[] colouringArgs = {new Atom(solverButtonGroup.getSelection().getActionCommand()),

                                      new Atom(variableSelectionButtonGroup.getSelection().getActionCommand()),
				      new Atom(valueChoiceButtonGroup.getSelection().getActionCommand()),
                                      new Integer(mapSize), null, null};

            result = eclipse.rpc(new CompoundTermImpl("colouring", colouringArgs));
            
            // The top-level functor of the goal term is 'colouring'. 
            // The fifth and sixth arguments are the return arguments 
            // with the number of backtracks performed and the execution time. The 
            // remaining arguments are populated from the radio button menu items.
            Integer backtracks = (Integer)result.arg(5);
            Double time = (Double)result.arg(6);
            
            if (backtracks != null) {
              statusTextArea.setText(" Status: " + solutionCount + " solution(s) found " +
                                     "in " + time.doubleValue() + " seconds with " + 
                                     backtracks.intValue() + " backtracks");
            }
            else {
              statusTextArea.setText(" Status: " + solutionCount + " solution(s) found " +
                                     "in " + time.doubleValue() + " seconds");
            }
          }
          catch(Exception msqExpn) {
            System.out.println( "Exception caught whilst invoking " +
                                "remote predicate 'colouring': " + 
                                msqExpn.getMessage());
          }
          
          // Colouring completed - enable all operations
          runButton.setEnabled(true);
          methodMenu.setEnabled(true);
          newMapMenuItem.setEnabled(true);
          mapSizeMenuItem.setEnabled(true);

          // Colouring thread is not running
          colouringThread = null;
        }
      }).start();
  }
  
  // The 'Continue' button has been clicked, continue map 
  // colouring solution search
  private void continueSearch() 
  {
    // Set the status
    statusTextArea.setText(" Status: Colouring map - searching for a solution");

    // Disable operations that are not allowed whilst colouring is 
    // in progress.
    moreButton.setEnabled(false);
    finishButton.setEnabled(false);

    // Indicate (another) solution has been found
    solutionCount ++;

    // Pass control back to ECLiPSe indicating it should
    // continue the search
    controlDataProducer.writeContinueAction("yes");
  }
  
  // The 'Finish' button has been clicked, terminate map 
  // colouring solution search
  private void endSearch() 
  {
    // Enable/disable the appropriate operations
    runButton.setEnabled(true);
    moreButton.setEnabled(false);
    finishButton.setEnabled(false);
    methodMenu.setEnabled(true);
    newMapMenuItem.setEnabled(true);
    mapSizeMenuItem.setEnabled(true);

    // Indicate (another) solution has been found
    solutionCount ++;

    // Pass control back to ECLiPSe indicating it should
    // terminate the search
    controlDataProducer.writeContinueAction("no");
  }
  
  // Draw the map on the image buffer and set the size of the
  // application frame appropriately
  private void setupMap()
  {
    int cnt, rectCnt;
    Rectangle rect;
    Graphics2D gbi = imageBuffer.createGraphics();

    gbi.setColor(Color.gray);
    
    // Draw and fill every rectangle for every country
    for(cnt = 0; cnt < mapSize; cnt++) {
      for(rectCnt = 0; rectCnt < countries[cnt].size(); rectCnt++) {
        rect = (Rectangle)countries[cnt].elementAt(rectCnt);
        gbi.draw(rect);
        gbi.fill(rect);
      }
    }

    // Set the size of the application frame appropriately
    this.setSize(maxWidth + 20, maxHeight + 35 + 
                 controlButtonPanel.getHeight() + 
                 menuBar.getHeight() + statusPanel.getHeight());
  }
    
  // The map size and scale has been updated, reflect this on the
  // map 
  private void resizeMap(int newMapSize, int newScalingFactor)
  {
    int cnt, rectCnt, x, y, width, height;
    Rectangle rect, newRect;
    Vector[] newCountries;

    // A new vector for the new country dimensions
    newCountries = new Vector[maxMapSize];

    // No longer no the maximum width and height of the image buffer
    maxWidth = 0;
    maxHeight = 0;

    // Apply scaling to every rectangle of every country 
    for(cnt = 0; cnt < maxMapSize; cnt++) {
      newCountries[cnt] = new Vector();
      for(rectCnt = 0; rectCnt < countries[cnt].size(); rectCnt++) {
        rect = (Rectangle)countries[cnt].elementAt(rectCnt);
        x = (((rect.x - 1) / scalingFactor) * newScalingFactor) + 1;
        y = (((rect.y - 1) / scalingFactor) * newScalingFactor) + 1;
        width = (((rect.width + 2) / scalingFactor) * newScalingFactor) - 2;
        height = (((rect.height + 2) / scalingFactor) * newScalingFactor) - 2;
        
        newRect = new Rectangle(x, y, width, height);
            
          newCountries[cnt].addElement(newRect);

          // Update the maximum width and height of the image buffer
          if ( width > maxWidth ) maxWidth = width;
          if ( height > maxHeight ) maxHeight = height;
      }
    }

    // Update the scaling and sizing factors
    mapSize = newMapSize;
    scalingFactor = newScalingFactor;

    // Update the countries data structure
    countries = newCountries;

    // Create a new image buffer
    imageBuffer = new BufferedImage(maxWidth, maxHeight,
                                    BufferedImage.TYPE_INT_ARGB);

    // Draw the map on the image buffer and set the size of the
    // application frame appropriately
    setupMap();
    
    // Make a request for components to be laid out by 
    // the layout manmager
    validate();

    // Redraw the window
    repaint();
  }

  // A colour translation function translating the prolog map colours
  // to appropriate Java colours
  private Color convertColour(String colour)
  {
    if (colour.equals("green")) return Color.green;
    if (colour.equals("purple")) return Color.magenta;
    if (colour.equals("red")) return Color.red;
    if (colour.equals("yellow")) return Color.yellow;
    if (colour.equals("darkgray")) return Color.gray;

    System.out.println("Colour '" + colour + "' is unmapped.");
    return Color.white;
  }

  // A country has been update with a new colour - reflect this 
  // on the map
  private void updateMap(int countryIndex, String colour)
  {
    int rectCnt;
    Rectangle rect;
    Graphics2D gbi = (Graphics2D)imageBuffer.getGraphics();

    gbi.setColor(convertColour(colour));

    // Draw and fill every rectangle of the country in the appropriate
    // colour
    for(rectCnt = 0; rectCnt < countries[countryIndex].size(); rectCnt++) {
      rect = (Rectangle)countries[countryIndex].elementAt(rectCnt);
      gbi.draw(rect);
      gbi.fill(rect);

      // Redraw the window
      repaint();
    }
  }
    
  // When initialising, compile a default map file
  private void compileDefaultMapData() 
  {
    // Get the pathname separator, e.g. '/' (UNIX) or a '\'
    // (Windows)
    String seperator = System.getProperty("file.separator");
    
    // Locate the default map file
    String defaultMapFile = 
      System.getProperty("eclipse.directory") +
      seperator + "lib_tcl" + seperator + "map_data.map";
    
    // Compile the default map data
    compileMapData(defaultMapFile);
  }
  
  // Instruct the embedded ECLiPSe engine to compile a 
  // new map file and pass it control to do so the 
  // country data will be received on the 'setup_map' queue
  private void compileMapData(String filename) 
  {
    CompoundTerm result;
    
    // Make an embedded ECLiPSe call to 'init_map' predicate
    try {

      result = eclipse.rpc( new CompoundTermImpl("init_map", filename, null));

      // The top-level functor of the goal term is 'init_map'. 
      // The first and second arguments of the goal term are the
      // filename and the maximum size of the map. It is the second
      // argument that we're interested in.
      Integer retMaxMapSize = (Integer)result.arg(2);
      
      // Reflect the map size in the appropriate variables and 
      // window widgets
      mapSize = maxMapSize = retMaxMapSize.intValue();
      mapSizeSlider.setMaximum(maxMapSize);
      mapSizeSlider.setValue(maxMapSize);
      mapSizeTextField.setText("" + mapSize);
    }
    catch(Exception msqExpn) {
      System.out.println( "Exception caught whilst invoking " +
                          "remote predicate 'init_map': " + 
                          msqExpn.getMessage());
      return;
    }

    // Create a fixed size array containing dynamic arrays of 
    // rectangles to store the country data. Each country is represented
    // as a dynamic array of rectangles
    countries = new Vector[maxMapSize];

    // Make an embedded ECLiPSe call to 'get_map_data' predicate
    try {
      result = eclipse.rpc( new CompoundTermImpl("get_map_data", 
                                new Integer(mapSize)));
    }
    catch(Exception msqExpn) {
      System.out.println( "Exception caught whilst invoking " +
                          "remote predicate 'get_map_data': " + 
                          msqExpn.getMessage());
    }

    repaint();
    statusTextArea.setText(" Status: Map file '" + filename
                           + "' loaded");
  }
  
  // Embed an ECLiPSe engine, initialise it, compile in the 
  // map colouring program and setup the 'setup_map', 
  // 'update_map' and 'continue' Java - ECLiPSe EXDR 
  // interface queues
  private void loadECLiPSeMapColouringEngine()
  {
    String seperator;
    
    eclipseEngineOptions = new EclipseEngineOptions();
    
    // Connect the Eclipse's standard streams to the JVM's
    eclipseEngineOptions.setUseQueues(false);

    try {
      // Initialise Eclipse
      eclipse = EmbeddedEclipse.getInstance(eclipseEngineOptions);
    }
    catch(Exception expn) {
      System.out.println("Exception caught whilst " +
                         "initialising ECLiPSe engine: " +
                         expn.getMessage());
      System.exit(0);
    }

    seperator = System.getProperty("file.separator");
    
    // Locate the map colouring program
    mapColouringProgram = new File(System.getProperty("eclipse.directory") +
                                   seperator + "lib_tcl" + seperator + 
                                   "mapcolour.ecl");
    
    // Compile the map colouring solver
    try {
      eclipse.compile(mapColouringProgram);
    }
    catch(Exception miscExpn) {
      System.out.println( "Exception caught whilst compiling " +
                          "'mapcolour.ecl': " + 
                          miscExpn.getMessage());
      System.exit(0);
    }
    
    // Set up the java representation of the queue streams
    try {
      continueQueue = eclipse.getToEclipseQueue("continue");
    }
    catch(Exception cqExpn) {
      System.out.println( "Exception caught whilst creating " +
                          "continue queue: " + 
                          cqExpn.getMessage());
      System.exit(0);
    }

    // Add a producer to the continueQueue ToEclipseQueue
    try {
      controlDataProducer = new ControlDataProducer();
      continueQueue.setListener(controlDataProducer);
    }
    catch(IOException cqIOExpn) {
      System.out.println( "I/O Exception caught whilst creating " +
                          "control queue listener: " + 
                          cqIOExpn.getMessage());
      System.exit(0);
    }

    try {
      setupMapQueue = eclipse.getFromEclipseQueue("setup_map");
    }
    catch(Exception msqExpn) {
      System.out.println( "Exception caught whilst creating " +
                          "map setup queue: " + 
                          msqExpn.getMessage());
      System.exit(0);
    }

    // Add a listener to the setupMapQueue FromEclipseQueue
    try {
      setupMapQueue.setListener(new SetupMapDataListener());
    }
    catch(IOException msqIOExpn) {
      System.out.println( "I/O Exception caught whilst creating " +
                          "map setup queue listener: " + 
                          msqIOExpn.getMessage());
      System.exit(0);
    }

    try {
      updateMapQueue = eclipse.getFromEclipseQueue("update_map");
    }
    catch(Exception muqExpn) {
      System.out.println( "Exception caught whilst creating " +
                          "map update queue: " + 
                          muqExpn.getMessage());
      System.exit(0);
    }

    // Add a listener to the updateMapQueue FromEclipseQueue
    try {
      updateMapQueue.setListener(new UpdateMapDataListener());
    }
    catch(IOException muqIOExpn) {
      System.out.println( "I/O Exception caught whilst creating " +
                          "map update queue listener: " + 
                          muqIOExpn.getMessage());
      System.exit(0);
    }
  }

  // Static 'main' function defining the application - 
  // load the default map data and show the application frame
  public static void main(String args[])
  {
    EclipseMapColourer mapColourerApp;
    
    // Construct the application frame
    mapColourerApp = new EclipseMapColourer();

    // Initialise and configure ECLiPSe engine
    mapColourerApp.loadECLiPSeMapColouringEngine();
    
    // Create the EclipseMapColourer

    // Load the default map data
    mapColourerApp.compileDefaultMapData();

    // Open the EclipseMapColourer frame
    mapColourerApp.setResizable(false);
    mapColourerApp.show();
  }
    
  // Private class variable declarations

  // GUI objects
  private JMenuItem aboutMenuItem;
  private JRadioButtonMenuItem rotateColoursMenuItem;
  private JRadioButtonMenuItem indomainRandomMenuItem;
  private JMenu fileMenu;
  private JPanel controlButtonPanel;
  private MapPanel mapPanel;
  private JPanel statusPanel;
  private JTextArea statusTextArea;
  private JMenuBar menuBar;
  private JMenu valueChoiceMenu;
  private JRadioButtonMenuItem fdMenuItem;
  private JMenu variableSelectionMenu;
  private JMenuItem mapSizeMenuItem;
  private JButton moreButton;
  private JRadioButtonMenuItem delayTilGrndMenuItem;
  private JButton finishButton;
  private JRadioButtonMenuItem firstFailMenuItem;
  private JMenu methodMenu;
  private JRadioButtonMenuItem antiFirstFailMenuItem;
  private JButton runButton;
  private JRadioButtonMenuItem indomainMenuItem;
  private JRadioButtonMenuItem inputOrderMenuItem;
  private JMenu solverMenu;
  private JMenuItem exitMenuItem;
  private JMenuItem newMapMenuItem;
  private JRadioButtonMenuItem icMenuItem;
  private JRadioButtonMenuItem prologMenuItem;
  private JRadioButtonMenuItem mostConstrainedMenuItem;
  private JRadioButtonMenuItem occurrenceMenuItem;
  private JMenu helpMenu;
  private ButtonGroup solverButtonGroup;
  private ButtonGroup valueChoiceButtonGroup;
  private ButtonGroup variableSelectionButtonGroup;
  private JFileChooser mapFileChooser;
  private JDialog mapSizeDialog;
  private JPanel mapSizePanel;
  private JLabel mapSizeLabel;
  private JSlider mapSizeSlider;
  private JTextField mapSizeTextField;
  private JLabel mapScalingLabel;
  private JTextField mapScalingTextField;
  private JButton mapSizeOkButton;
  private JButton mapSizeCancelButton;

  // Map related objects
  private Vector[] countries;
  private int scalingFactor = 20;
  private BufferedImage imageBuffer;
  private int maxWidth;
  private int maxHeight;
  private Thread colouringThread;
  private int maxMapSize;
  private int mapSize;
  private int solutionCount;

  // ECLiPSe Objects

  // Create some default Eclipse options
  private EclipseEngineOptions eclipseEngineOptions;

  // Object representing the Eclipse process
  private EclipseEngine eclipse;

  // Path of the Eclipse program
  private File mapColouringProgram;

  // Data going out from java
  private ToEclipseQueue continueQueue;
  private ControlDataProducer controlDataProducer;

  // Data coming in from eclipse
  private FromEclipseQueue setupMapQueue;
  private FromEclipseQueue updateMapQueue;

  // Define an inner class that extends a JPanel
  // allowing us to display an image buffer on it
  class MapPanel extends JPanel
  {
    // Override the update() and paint()
    // methods to draw our image buffer
    // on the appropriate graphics context

    public void update(Graphics g)
    {
      super.update(g);
      paint(g);
    }
    
    public void paint(Graphics g)
    {
      super.paint(g);
      g.drawImage(imageBuffer,0,0, this);
    }
  }

  // Define inner class that allows the JFileChooser
  // to filter possible file selections to those with 
  // '.map' extensions.
  class MapFileFilter extends FileFilter 
  {
    // Whether a file should be filtered or not
    public boolean accept(File f) {
      if (f.isDirectory()) {
        return true;
      }
      
      String extension = getExtension(f);
      if (extension != null) {
        if (extension.equals("map") ||
            extension.equals("MAP")) {
          return true;
        } else {
          return false;
        }
      }
      
      return false;
    }
    
    // The description of this filter
    public String getDescription() {
      return "Map files";
    }
    
    // Locate the file extension
    private String getExtension(File f)
    {
      String s = f.getName();
      int i = s.lastIndexOf('.');
      if (i > 0 &&  i < s.length() - 1)
        return s.substring(i+1).toLowerCase();
      return "";
    }
  }

  // Define an inner class that reads map country data
  // from the 'setup_map' queue populated by ECLiPSe
  // engine during 'get_map_data' predicate call. The 
  // uncoloured map is then drawn.
  class SetupMapDataListener implements QueueListener
  {
    FromEclipseQueue input_queue_stream = null;
    EXDRInputStream input_queue_stream_formatted = null;

    // Called when Eclipse flushes source
    public void dataAvailable(Object source)
    {
      CompoundTerm data;
      int x1, y1, x2, y2, countryIndex;
      Rectangle rect;

      // Reset the maximum size of the map
      maxWidth = 0;
      maxHeight = 0;

      if(input_queue_stream == null)
      {
	input_queue_stream = (FromEclipseQueue) source;
	input_queue_stream_formatted =
	  new EXDRInputStream(input_queue_stream);
      }

      try
      {
        // Repeatedly read terms from stream until 'end' atom is read
        for ( ;; ) {
          data = (CompoundTerm)input_queue_stream_formatted.readTerm();
          if (data.functor().equals("end")) break;

          // Add a rectangle for the appropriate country - note the
          // index in the data structure is the country name - 1
          countryIndex = ((Integer)(data.arg(1))).intValue() - 1;
          x1 = (((Integer)(data.arg(2))).intValue() * scalingFactor) + 1;
          y1 = (((Integer)(data.arg(3))).intValue() * scalingFactor) + 1;
          x2 = (((Integer)(data.arg(4))).intValue() * scalingFactor) - 1;
          y2 = (((Integer)(data.arg(5))).intValue() * scalingFactor) - 1;
          
          rect = new Rectangle(x1, y1, x2-x1, y2-y1);
          if (countries[countryIndex] == null) {
            countries[countryIndex] = new Vector();
          }
            
          countries[countryIndex].addElement(rect);

          // Update the maximum width and height of the image buffer
          if ( x2 > maxWidth ) maxWidth = x2;
          if ( y2 > maxHeight ) maxHeight = y2;
        }
      } catch(IOException ioe){
        System.out.println( "I/O Exception caught whilst reading data on" +
                            "'setup_map' queue: " + 
                            ioe.getMessage());
        return;
      }

      // Calculate the size of the image buffer
      imageBuffer = new BufferedImage(maxWidth, maxHeight,
                                      BufferedImage.TYPE_INT_ARGB);

      // Draw the uncoloured map
      setupMap();
    }

    // Required to implement QueueListener
    public void dataRequest(Object source)
    {
    }
  }

  // Define an inner class that reads map country data
  // from the 'update_map' queue populated by ECLiPSe
  // engine during 'colouring' predicate call. The 
  // map is then drawn with the appropriate country (re)coloured.
  class UpdateMapDataListener implements QueueListener
  {
    FromEclipseQueue input_queue_stream = null;
    EXDRInputStream input_queue_stream_formatted = null;

    // Called when Eclipse flushes source
    public void dataAvailable(Object source)
    {
      CompoundTerm data;

      if(input_queue_stream == null)
      {
	input_queue_stream = (FromEclipseQueue) source;
	input_queue_stream_formatted =
	  new EXDRInputStream(input_queue_stream);
      }

      try
      {
        // Repeatedly read terms from stream while data is available
        // The first argument is the map name/number, the second is an
        // atom indicating the new colour of the country
        while(((InputStream) source).available() > 0)
        {
          data = (CompoundTerm)input_queue_stream_formatted.readTerm();
          updateMap(((Integer)(data.arg(1))).intValue() - 1, 
            ((Atom)(data.arg(2))).functor());
        }
      } catch(IOException ioe){
        System.out.println( "I/O Exception caught whilst reading data on" +
                            "'update_map' queue: " + 
                            ioe.getMessage());
      }
    }

    // Required to implement QueueListener
    public void dataRequest(Object source)
    {
    }
  }

  // Define an inner class that writes the search control data
  // to the 'control' queue that ECLiPSe engine reads from having 
  // found a solution in the 'colouring' predicate call.
  // The call will block the thread until the user presses the 'More'
  // or 'Finish' button. We use the Java wait()/notify() mechanism to
  // coordinate this.
  class ControlDataProducer implements QueueListener
  {
    private ToEclipseQueue output_queue_stream = null;
    private EXDROutputStream output_queue_stream_formatted = null;
    private Atom continueAction = null;

    // Required to implement QueueListener
    public void dataAvailable(Object source)
    {
    }

    // Called when Eclipse tries to read from source when it is empty.
    // A solution has been found, wait for user action
    public synchronized void dataRequest(Object source)
    {
      int tmpSolutionCount = solutionCount + 1;
      
      if(output_queue_stream == null)
      {
	output_queue_stream = (ToEclipseQueue) source;
	output_queue_stream_formatted =
	  new EXDROutputStream(output_queue_stream);
      }

      statusTextArea.setText(" Status: " + tmpSolutionCount +
                             " solution(s) found. Continue search?");
      runButton.setEnabled(false);
      moreButton.setEnabled(true);
      finishButton.setEnabled(true);

      while(continueAction == null) {
        try
        {
          wait();
        } catch(Exception expn){
          System.out.println( "Exception caught whilst waiting " +
                              "on user continue action: " + 
                              expn.getMessage());
        }
      }

      // Write the user action
      try
      {
        output_queue_stream_formatted.write(continueAction);
        output_queue_stream_formatted.flush();
      } catch(IOException ioe){
        System.out.println( "I/O Exception caught whilst writing " +
                            "continue action: " + 
                            ioe.getMessage());
      }
      
      continueAction = null;
    }

    // The user has clicked 'More' or 'Finish' button, indicate this
    // action to the paused colouring thread
    public synchronized void writeContinueAction(String action)
    {
      continueAction = new Atom(action);
      notify();
    }
  }
}
