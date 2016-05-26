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
// The Original Code is  CPViz Constraint Visualization System
// The Initial Developer of the Original Code is  Helmut Simonis
// Portions created by the Initial Developer are
// Copyright (C) 2009-2010 Helmut Simonis
// 
// Contributor(s): 	Helmut Simonis, 4C, Univerity College Cork, Cork
//			Paul Davern, 4C, Univerity College Cork, Cork
// 
// END LICENSE BLOCK
// ----------------------------------------------------------------------
package components;


import javax.help.HelpBroker;
import javax.help.HelpSet;
import javax.help.SwingHelpUtilities;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JInternalFrame;
import javax.swing.JDesktopPane;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JMenuBar;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.Timer;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;


import org.apache.batik.util.Service;
//import org.apache.batik.util.gui.resource.ActionMap;
//import org.apache.batik.util.gui.resource.JComponentModifier;
//import org.apache.batik.util.gui.resource.JToolbarButton;
//import org.apache.batik.util.gui.resource.MissingListenerException;
//import org.apache.batik.util.gui.resource.ToolBarFactory;
import org.apache.batik.util.resources.ResourceManager;

import java.awt.event.*;
import java.awt.geom.AffineTransform;
import java.awt.*;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.Vector;

/*
 * InternalFrameDemo.java requires:
 *   MyInternalFrame.java
 */
public class InternalFrame extends JFrame
                               implements ActionListener,
                                ActionMap{
	 public static final int FILESSTARTPOS = 2;
	 public static final int NOFILEITEMS = 4;
	 public static final String ABOUT_ACTION = "AboutAction";
	    public static final String OPEN_ACTION = "OpenAction";
	    public static final String OPEN_LOCATION_ACTION = "OpenLocationAction";
	    public static final String NEW_WINDOW_ACTION = "NewWindowAction";
	    public static final String RELOAD_ACTION = "ReloadAction";
	    public static final String SAVE_AS_ACTION = "SaveAsAction";
	    public static final String BACK_ACTION = "BackAction";
	    public static final String FORWARD_ACTION = "ForwardAction";
	    public static final String MOVIE_SETUP_ACTION = "MovieSetupAction";
	    public static final String END_BACK_ACTION = "EndBackAction";
	    public static final String END_FORWARD_ACTION = "EndForwardAction";
	    public static final String FAST_FORWARD_ACTION = "FastForwardAction";
	    public static final String REWIND_ACTION = "RewindAction";
	    public static final String FULL_SCREEN_ACTION = "FullScreenAction";
	    public static final String PRINT_ACTION = "PrintAction";
	    public static final String EXPORT_AS_JPG_ACTION = "ExportAsJPGAction";
	    public static final String EXPORT_AS_PNG_ACTION = "ExportAsPNGAction";
	    public static final String EXPORT_AS_TIFF_ACTION = "ExportAsTIFFAction";
	    public static final String PREFERENCES_ACTION = "PreferencesAction";
	    public static final String CLOSE_ACTION = "CloseAction";
	    public static final String VIEW_SOURCE_ACTION = "ViewSourceAction";
	    public static final String EXIT_ACTION = "ExitAction";
	    public static final String RESET_TRANSFORM_ACTION = "ResetTransformAction";
	    public static final String ZOOM_IN_ACTION = "ZoomInAction";
	    public static final String ZOOM_OUT_ACTION = "ZoomOutAction";
	    public static final String PREVIOUS_TRANSFORM_ACTION = "PreviousTransformAction";
	    public static final String NEXT_TRANSFORM_ACTION = "NextTransformAction";
	    public static final String USE_STYLESHEET_ACTION = "UseStylesheetAction";
	    public static final String PLAY_ACTION = "PlayAction";
	    public static final String PAUSE_ACTION = "PauseAction";
	    public static final String STOP_ACTION = "StopAction";
	    public static final String MONITOR_ACTION = "MonitorAction";
	    public static final String DOM_VIEWER_ACTION = "DOMViewerAction";
	    public static final String SET_TRANSFORM_ACTION = "SetTransformAction";
	    public static final String FIND_DIALOG_ACTION = "FindDialogAction";
	    public static final String THUMBNAIL_DIALOG_ACTION = "ThumbnailDialogAction";
	    public static final String FLUSH_ACTION = "FlushAction";
	    public static final String TOGGLE_DEBUGGER_ACTION = "ToggleDebuggerAction";
public static	String initfilename;
private HelpSet hs;
private HelpBroker hb;
private URL hsURL;
    JDesktopPane desktop;
    JSlider slide_images = new JSlider();
    int WhichIndex = 0;
    String LeftFileName;
    String ThePath;
    Dimension OldSize=null;
    MyInternalFrame left;
    MyInternalFrame right;
    JMenuBar TheMenuBar=null;
    boolean doSlideradjust = true;
    protected BackAction backAction = new BackAction();
    protected EndBackAction endbackAction = new EndBackAction();

    /**
     * The forward action
     */
    protected ForwardAction forwardAction = new ForwardAction();
    protected EndForwardAction endforwardAction = new EndForwardAction();
    protected FastForwardAction fastforwardAction = new FastForwardAction();
    protected RewindAction rewindAction = new RewindAction();
    protected StopAction stopAction = new StopAction();
    protected MovieSetupAction moviesetupAction = new MovieSetupAction();
    
    public static final String RESOURCES =
        "components.resources.GUI";
protected static ResourceBundle bundle;

/**
 * The resource manager
 */
protected static ResourceManager resources;
static {
    bundle = ResourceBundle.getBundle(RESOURCES, Locale.getDefault());
    resources = new ResourceManager(bundle);
}


protected Map listeners = new HashMap();

/**
 * Returns the action associated with the given string
 * or null on error
 * @param key the key mapped with the action to get
 * @throws MissingListenerException if the action is not found
 */
public Action getAction(String key) throws MissingListenerException {
    Action result = (Action)listeners.get(key);
   // System.out.println("The Action"+result);
    //if (result == null) {
    //result = canvas.getAction(key);
    //}
    if (result == null) {
        throw new MissingListenerException("Can't find action.", RESOURCES, key);
    }
    return result;
}
    public InternalFrame() {
        super("ECLiPSe Visualizer");
        try {
      	  UIManager.setLookAndFeel(
      	    UIManager.getSystemLookAndFeelClassName());
      	} catch (Exception e) {
      	}
        ThePath = System.getProperty("user.dir");
        //Make the big window be indented 50 pixels from each edge
        //of the screen.
        int inset = 50;
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        setBounds(inset, inset,
                  screenSize.width  - inset*2,
                  screenSize.height - inset*2);
        listeners.put(OPEN_ACTION, new OpenAction());
       // listeners.put(OPEN_LOCATION_ACTION, new OpenLocationAction());
       listeners.put(BACK_ACTION, backAction);
       listeners.put(END_BACK_ACTION, endbackAction);
        listeners.put(FORWARD_ACTION, forwardAction);
        listeners.put(END_FORWARD_ACTION, endforwardAction);
        listeners.put(MOVIE_SETUP_ACTION, moviesetupAction);
        listeners.put(REWIND_ACTION, rewindAction);
        listeners.put(FAST_FORWARD_ACTION, fastforwardAction);
        listeners.put(STOP_ACTION, stopAction);
       // listeners.put(EXIT_ACTION, application.createExitAction(this));
        listeners.put(THUMBNAIL_DIALOG_ACTION, new ThumbnailDialogAction());
      
        //Set up the GUI.
        desktop = new JDesktopPane(); //a specialized layered pane
        left = createFrame(); //create first "window"
        right = createFrame();
        left.setSize((screenSize.width  - inset*3)/2,screenSize.height - inset*4);
        //Set the window's location.
        left.setLocation(0, 0);
        right.setSize((screenSize.width  - inset*3)/2,screenSize.height - inset*4);
        JPanel p = new JPanel(new BorderLayout());
        //Set the window's location.
        right.setLocation((screenSize.width  - inset*3)/2, 30);
        ToolBarFactory tbf = new ToolBarFactory(bundle, this);
        JToolBar tb = tbf.createJToolBar("ToolBar");
        tb.setFloatable(false);
        p.add(tb, BorderLayout.NORTH);
        for (int iu=0;iu<10;iu++){
        	Component c = tb.getComponentAtIndex(iu);
        	//System.out.println("Compoent "+iu+" "+c);
        	if (c!=null){
        		try{
        		JToolbarButton b = (JToolbarButton)c;
        		//System.out.println("icon "+b.getIcon());
        		}catch (Exception ee){
        		//	System.out.println("exp");
        		}
        	}
        }
        p.add(slide_images,BorderLayout.SOUTH);
        getContentPane().add(p, BorderLayout.NORTH);
        getContentPane().add(desktop, BorderLayout.CENTER);
       // setContentPane(desktop);
        TheMenuBar = createMenuBar();
        setJMenuBar(TheMenuBar);
        
        //Make dragging a little faster but perhaps uglier.
        desktop.setDragMode(JDesktopPane.OUTLINE_DRAG_MODE);
        addComponentListener(new ComponentAdapter() {
            public void componentResized(ComponentEvent e) {
         	 Dimension d = desktop.getSize();
         	 if (OldSize != null){
         		 Dimension l = left.getSize();
         		 Dimension r = right.getSize();
         		 int x = (int)(d.getWidth()* l.getWidth()/OldSize.getWidth());
         		 int y = (int)(d.getHeight()* l.getHeight()/OldSize.getHeight());
         		 left.setSize(x,y);
         		 int rx = (int)(d.getWidth()* r.getWidth()/OldSize.getWidth());
         		 int ry = (int)(d.getHeight()* r.getHeight()/OldSize.getHeight());
         		 right.setSize(rx,ry);
         		 left.setLocation(0,0);
         		 right.setLocation(x,0);
         	 }
         	 else{
         	  	left.setSize((int)(d.getWidth()/2),(int)(d.getHeight()));
         	    right.setSize((int)(d.getWidth()/2),(int)(d.getHeight()));
         	    right.setLocation((int)(d.getWidth()/2),0);
         	 }
         	OldSize = d;
            	//System.out.println("JSVG canvas component resize listener");
            	
      	   }
             
           
            
        }); 
        slide_images.setValue(1);
        slide_images.setMaximum(100);
        slide_images.addChangeListener(new javax.swing.event.ChangeListener() {
            public void stateChanged(ChangeEvent e) {
            	JSlider source = (JSlider)e.getSource();
                //if (!source.getValueIsAdjusting()) 
                {
                if (doSlideradjust){
                	float fps = source.getValue();
                	float pos;
                	pos = fps/100;
                	Iterator ii = FileNames.iterator();
                	int size = FileNames.size();
                	if (size == 0) return;
                	int num = (int)(size * pos);
                	if (fps>0.0)
                	num = (int)fps-1;
                	else
                		num=0;
                	//if ( num %2 == 1) num--;
                //	System.out.println("num is"+num+"fps"+fps+" size "+size);
                	int i=0;
                	boolean didit=false;
                	if (source.getValueIsAdjusting()){//&&firstAdjust){
                		//AffineTransform at = activePane.svgCanvas.getRenderingTransform();
                		//if (activePane == leftpane)
                		   // leftoldTransform = leftpane.svgCanvas.getRenderingTransform();;
                		//else
                			//rightoldTransform = rightpane.svgCanvas.getRenderingTransform();;
                		//firstAdjust = false;
                	}
                	if (!source.getValueIsAdjusting()){
                		//firstAdjust = true;
                	}
                	
                	boolean found;
                	ii = FileNames.iterator();
             //   	while (ii.hasNext ()) {
                		String filenames[] = (String[])(FileNamesarr[num]);//(ii.next());
                	//	if (i==num ){//&& !filenames[0].equals(LeftFileName)){
                			WhichIndex = num;
                			/*if (!LeftFileName.equals("")){
                				didit = true;
                        		System.out.println("done transform possibly");
                        		Iterator iii = activePane.svgCanvas.getInteractors().iterator();
                        		while (iii.hasNext()){
                        			Object o = iii.next();
                        			if (o.getClass().getName().equals("org.apache.batik.swing.gvt.AbstractZoomInteractor")){
                        				AbstractZoomInteractor azi = (AbstractZoomInteractor)o;
                        				//azi.
                        				found = true;
                        			}
                        			System.out.println("Interactor "+o);
                        	//	listInteractors.add(o);
                        		}
                        	}*/
                			
                			//System.out.println("got the num");
                			LeftFileName = filenames[0];
                			String ss= "file:///"+ThePath+filenames[0];
                			String ss1 = "file:///"+ThePath+filenames[1];
                			//System.out.println("uri="+ss);
                			//leftpane.svgCanvas.setURI("file:///"+ThePath+filenames[0]);
                			//rightpane.svgCanvas.setURI("file:///"+ThePath+filenames[1]);
                			left.setURI(ss);
                			right.setURI(ss1);
                			//drawSVG(ss,ss1);
                		//	break;
                	//	}
                		i++;
                	//}
                }
               else
              	doSlideradjust=true;
                   
                }
            }
            });
        if (initfilename!=null){
        	fileopen(initfilename,true);
        }
    }

    protected JMenuBar createMenuBar() {
        JMenuBar menuBar = new JMenuBar();

        //Set up the lone menu.
        JMenu menu = new JMenu("File");
        menu.setMnemonic(KeyEvent.VK_D);
        menuBar.add(menu);

        //Set up the first menu item.
        JMenuItem menuItem = new JMenuItem("Open");
        menuItem.setMnemonic(KeyEvent.VK_O);
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_O, ActionEvent.ALT_MASK));
        menuItem.setActionCommand("open");
        menuItem.addActionListener(this);
        menu.add(menuItem);
        menu.addSeparator();
       

        //Set up the second menu item.
        try {
			FileReader fr = new FileReader("history.hist");
			BufferedReader br = new BufferedReader(fr);
			String s;
			try {
				int i=0;
				while((s = br.readLine()) != null && i<4) {
					if (s.length()<2)
						break;
					int pos = s.lastIndexOf("/");
		    		if (pos==-1)
		    			pos = s.lastIndexOf("\\");
					menuItem = new JMenuItem(s.substring(pos+1));
				    menuItem.setActionCommand("visited");
				    menuItem.setToolTipText(s);
				    menuItem.addActionListener(this);
				    menu.add(menuItem);
				    i++;
				}
				fr.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			menu.addSeparator();
			 //Set up the last menu item.
	        menuItem = new JMenuItem("Quit");
	        menuItem.setMnemonic(KeyEvent.VK_Q);
	        menuItem.setAccelerator(KeyStroke.getKeyStroke(
	                KeyEvent.VK_Q, ActionEvent.ALT_MASK));
	        menuItem.setActionCommand("quit");
	        menuItem.addActionListener(this);
	        menu.add(menuItem);
	        menu = new JMenu("Help");
	        menu.setMnemonic(KeyEvent.VK_D);
	        menuBar.add(menu);
	        menuItem = new JMenuItem("Contents");
	        menuItem.setMnemonic(KeyEvent.VK_H);
	        menuItem.setAccelerator(KeyStroke.getKeyStroke(
	                KeyEvent.VK_H, ActionEvent.ALT_MASK));
	        menuItem.setActionCommand("help");
	        menuItem.addActionListener(this);
	        menu.add(menuItem);
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
       

        return menuBar;
    }
    protected static Vector handlers;
    /**
     * Registers an input file handler by adding it to the handlers map.
     * @param handler the new input handler to register.
     */
    public static synchronized
        void registerHandler(SquiggleInputHandler handler) {
        Vector handlers = getHandlers();
        handlers.addElement(handler);
    }
    public class OpenAction extends AbstractAction {

        public OpenAction() {
        }
        public void actionPerformed(ActionEvent e) {
        	fileopen(null,true);
        }
    }
    Collection FileNames = new LinkedList();
    Object[] FileNamesarr = null;
    protected static Vector getHandlers() {
        if (handlers != null) {
            return handlers;
        }

        handlers = new Vector();
        registerHandler(new SVGInputHandler());

        Iterator iter = Service.providers(SquiggleInputHandler.class);
        while (iter.hasNext()) {
            SquiggleInputHandler handler
                = (SquiggleInputHandler)iter.next();

            registerHandler(handler);
        }

        return handlers;
    }
  
    public void fileopen(String fname, boolean addHistory){
    	JFileChooser fileChooser = null;
   	 File f =null;
    	if (fname == null){
    	 fileChooser = new JFileChooser(ThePath);
        fileChooser.setFileHidingEnabled(false);
        fileChooser.setFileSelectionMode
            (JFileChooser.FILES_ONLY);

        //
        // Add file filters from the handlers map
        //get rid of zip file expandsion
        // regsvr32 /u %windir%\system32\zipfldr.dll 
        // regsvr32 %windir%\system32\zipfldr.dll 



        Iterator iter = getHandlers().iterator();
        while (iter.hasNext()) {
            SquiggleInputHandler handler
                = (SquiggleInputHandler)iter.next();
            fileChooser.addChoosableFileFilter
                (new SquiggleInputHandlerFilter(handler));
        }
//System.out.println("do filechooser");
        int choice = fileChooser.showOpenDialog(this);
       
        if (choice == JFileChooser.APPROVE_OPTION) {
            f = fileChooser.getSelectedFile();
            //activePane.currentPath = f;
        }
    	}
    	else
    		f = new File(fname);
    if (f != null){
    	try {
    		//System.out.println("file name:"+f.getPath());
    		FileReader fr = new FileReader(f);
    		FileNames.clear();
    		BufferedReader br = new BufferedReader(fr); 
    		String s; 
    		try {
				while((s = br.readLine()) != null) { 
					int x = s.indexOf(' ');
					String left = s.substring(0, x);
					String right = s.substring(x+1,s.length());
					String p = f.getAbsolutePath();
					x = p.lastIndexOf('\\');
					if ( x == -1){
						x = p.lastIndexOf('/');
					}
					p = p.substring(0,x+1);
					ThePath = p;
					FileNames.add(new String[]{left,right});
				//System.out.println("x "+x+" left "+left+" right "+right+" path "+p + "oldpath= "+f.getAbsolutePath()); 
				}
			fr.close();
			FileNamesarr = FileNames.toArray();
			slide_images.setValue(1);
            slide_images.setMaximum(FileNames.size());
            left.SetVeryFirst(true);
            right.SetVeryFirst(true);
            if (addHistory){
            	JMenu jm = TheMenuBar.getMenu(0);
            	int max = jm.getItemCount();
            	int i=2;
            	int x=0;
            	if (max-NOFILEITEMS >=4){
            		x=1;
            		jm.remove(jm.getItem(FILESSTARTPOS+3));
            	}
            	s = f.getName();
            	int pos = s.lastIndexOf("/");
	    		if (pos==-1)
	    			pos = s.lastIndexOf("\\");
            	JMenuItem menuItem = new JMenuItem(f.getName());//(s.substring(pos+1));
			    menuItem.setActionCommand("visited");
			    menuItem.setToolTipText(f.getAbsolutePath());
			    menuItem.addActionListener(this);
			    jm.add(menuItem,FILESSTARTPOS);//-x+(max-NOFILEITEMS));
			    FileWriter fw = new FileWriter("history.hist");
				BufferedWriter bw = new BufferedWriter(fw);
				max = jm.getItemCount();
				for (i=FILESSTARTPOS;i<max-(NOFILEITEMS-FILESSTARTPOS);i++){
					menuItem = jm.getItem(i);
					bw.write(menuItem.getToolTipText());
					bw.newLine();
				}
				bw.close();
				
            }
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} 
    		try {
				fr.close();
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} 

            
           
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
    }
    }
    //React to menu selections.
    public void actionPerformed(ActionEvent e) {
        if ("open".equals(e.getActionCommand())) { //new
            fileopen(null,true);
        } 
        if ("visited".equals(e.getActionCommand())) { //new
        	JMenuItem ji = (JMenuItem)e.getSource();
        	String fname = ji.getToolTipText();
        	fileopen(fname,false);
        } 
        if ("help".equals(e.getActionCommand())) { //new
        	openHelp();
        } 
       // else { //quit
         //   quit();
        //}
    }
    /**
     * To go forward to the next document
     */
    public class ForwardAction extends    AbstractAction
                               implements JComponentModifier {
       List components = new LinkedList();
        public ForwardAction() {}
     
        /*public void actionPerformed(ActionEvent e) {
         //   if (localHistory.canGoForward()) {
                localHistory.forward();
            }
        }*/
        public void actionPerformed(ActionEvent arg0) {
	//		System.out.println("Right");
			if (FileNames.size()>0 && WhichIndex != FileNames.size()-1){
				//System.out.println("Right after if");
				WhichIndex++;
				float fps = slide_images.getValue();
            	float pos;
            	pos = fps/100;
            	int size = FileNames.size();
            	int num = (int)(size * pos);
            	doSlideradjust=false;
            	slide_images.setValue(WhichIndex);
				String filenames[] = (String[])((LinkedList)FileNames).get(WhichIndex);
				String ss= "file:///"+ThePath+filenames[0];
    			String ss1 = "file:///"+ThePath+filenames[1];
    		//	System.out.println("uri="+ss);
      			left.setURI(ss);
    			right.setURI(ss1);
			}
			
		}

        public void addJComponent(JComponent c) {
//            components.add(c);
//            c.setEnabled(false);
        }

        protected void update() {
            
        }
    }
    class MyActionListener implements ActionListener{

			public void actionPerformed(ActionEvent arg0) {
			// TODO Auto-generated method stub
			if ( GoBack )
				if ( WhichIndex == 0){
					timeractive=false;
					TheTimer.stop();
				}
				else
					backAction.actionPerformed(null);
			else{
				if ( WhichIndex == FileNames.size()-1){
					timeractive=false;
					TheTimer.stop();
				}
				else
				   forwardAction.actionPerformed(null);
			}
			}
    	
    }
    MyActionListener timeraction = new MyActionListener();
    boolean timeractive = false;
    int TheTimerDelay = 400; 
    Timer TheTimer = new Timer(TheTimerDelay, timeraction);
    boolean GoBack = false;
    /**
     * To go forward to the next document
     */
    public class RewindAction extends    AbstractAction
                               implements JComponentModifier {
       List components = new LinkedList();
        public RewindAction() {}
     
        /*public void actionPerformed(ActionEvent e) {
         //   if (localHistory.canGoForward()) {
                localHistory.forward();
            }
        }*/
        public void actionPerformed(ActionEvent arg0) {
        	if(FileNames.size()==0 ) return;
			//System.out.println("Left");
			if (!timeractive){
				TheTimer.start();
				timeractive = true;
			}
				GoBack = true;
			
			}

        public void addJComponent(JComponent c) {
//            components.add(c);
//            c.setEnabled(false);
        }

        protected void update() {
            
        }
    }
    /**
     * To go forward to the next document
     */
    public class FastForwardAction extends    AbstractAction
                               implements JComponentModifier {
       List components = new LinkedList();
        public FastForwardAction() {}
     
        /*public void actionPerformed(ActionEvent e) {
         //   if (localHistory.canGoForward()) {
                localHistory.forward();
            }
        }*/
        public void actionPerformed(ActionEvent arg0) {
		//	System.out.println("FAst Forward");
		//	System.out.println("Left");
			if(FileNames.size()==0 ) return;
			if (!timeractive){
				TheTimer.start();
				timeractive = true;
			}
				GoBack = false;
		
		}

        public void addJComponent(JComponent c) {
//            components.add(c);
//            c.setEnabled(false);
        }

        protected void update() {
            
        }
    }
    
    public class StopAction extends    AbstractAction{

		@Override
		public void actionPerformed(ActionEvent arg0) {
			// TODO Auto-generated method stub
			TheTimer.stop();
			timeractive = false;
		}
    	
    }
    public class EndBackAction extends    AbstractAction{

		@Override
		public void actionPerformed(ActionEvent arg0) {
			// TODO Auto-generated method stub
			if (timeractive){
			TheTimer.stop();
			timeractive = false;
			}
			if (FileNames.size()>0 ){
				//System.out.println("Right after if");
				WhichIndex = 0;
				doSlideradjust=false;
            	slide_images.setValue(WhichIndex);
				String filenames[] = (String[])((LinkedList)FileNames).get(WhichIndex);
				String ss= "file:///"+ThePath+filenames[0];
    			String ss1 = "file:///"+ThePath+filenames[1];
    		//	System.out.println("uri="+ss);
      			left.setURI(ss);
    			right.setURI(ss1);
			}
		}
    	
    }
    public class EndForwardAction extends    AbstractAction{

		@Override
		public void actionPerformed(ActionEvent arg0) {
			// TODO Auto-generated method stub
			if (timeractive){
			TheTimer.stop();
			timeractive = false;
			}
			if (FileNames.size()>0 ){
				//System.out.println("Right after if");
				WhichIndex = FileNames.size()-1;
				doSlideradjust=false;
            	slide_images.setValue(WhichIndex);
				String filenames[] = (String[])((LinkedList)FileNames).get(WhichIndex);
				String ss= "file:///"+ThePath+filenames[0];
    			String ss1 = "file:///"+ThePath+filenames[1];
    		//	System.out.println("uri="+ss);
      			left.setURI(ss);
    			right.setURI(ss1);
			}
		}
    	
    }
    public class MovieSetupAction extends    AbstractAction{

		@Override
		public void actionPerformed(ActionEvent arg0) {
			// TODO Auto-generated method stub
			Object[] possibilities = {"500", "1000","1500","2000","2500"};
			String s = (String)JOptionPane.showInputDialog(
			                    desktop,
			                    "Select the delay between documents in milliseconds\n",
			                    "Customized Dialog",
			                    JOptionPane.PLAIN_MESSAGE,
			                    null,
			                    possibilities,
			                    ""+TheTimerDelay );
//System.out.println("return from dialog "+s);
			//If a string was returned, say so.
			if ((s != null) && (s.length() > 2)) {
			    //setLabel("Green eggs and... " + s + "!");
				TheTimerDelay = Integer.valueOf(s).intValue();
			//	System.out.println("timer delay "+TheTimerDelay);
				TheTimer.setDelay(TheTimerDelay);
			    return;
			}

			//If you're here, the return value was null/empty.
			//setLabel("Come on, finish the sentence!");


		}
    	
    }
    
    public void openHelp() {
        // Identify the location of the .hs file 
        String pathToHS = "/javahelp/docs/helpset.hs";
        //Create a URL for the location of the help set
        try {
        	SwingHelpUtilities.setContentViewerUI("components.ExternalLinkContentViewerUI");

          URL hsURL = getClass().getResource(pathToHS);
            hs = new HelpSet(null, hsURL);
         // hs = new HelpSet(null,
        	//	  new URL("file:///c:/documents and settings/pdavern/my documents/firsthelp/helpset.hs"));
        } catch (Exception ee) {
            // Print info to the console if there is an exception
        //    System.out.println( "HelpSet " + ee.getMessage());
        //    System.out.println("Help Set "+ pathToHS +" not found");
            return;
        }
      
        // Create a HelpBroker object for manipulating the help set
        hb = hs.createHelpBroker();
        //Display help set
        hb.setDisplayed(true);
    }

    public class ExitAction extends    AbstractAction{

		@Override
		public void actionPerformed(ActionEvent arg0) {
			// TODO Auto-generated method stub
			
		}
    	
    }
    public class BackAction extends    AbstractAction
    implements JComponentModifier {
    		List components = new LinkedList();
    			public BackAction() {//System.out.println("left");
    				
    			}

    			public void actionPerformed(ActionEvent arg0) {
    			//	System.out.println("Left");
    				if (FileNames.size() >0 && WhichIndex != 0){
    					WhichIndex--;
    					float fps = slide_images.getValue();
    	            	float pos;
    	            	pos = fps/100;
    	            	int size = FileNames.size();
    	            	int num = (int)(size * pos);
    	            	doSlideradjust=false;
    	            	slide_images.setValue(WhichIndex);
    					String filenames[] = (String[])((LinkedList)FileNames).get(WhichIndex);
    					String ss= "file:///"+ThePath+filenames[0];
    	    			String ss1 = "file:///"+ThePath+filenames[1];
    	    		//	System.out.println("uri="+ss);
    	    			left.setURI(ss);
    	    			right.setURI(ss1);	 
    				}	
    			}
    			

    			public void addJComponent(JComponent c) {
//          components.add(c);
//c.setEnabled(false);
    			}

    			protected void update() {
/*boolean b = localHistory.canGoBack();
Iterator it = components.iterator();
while (it.hasNext()) {
((JComponent)it.next()).setEnabled(b);
}*/
    			}
}
    
    /**
     * To display the Thumbnail dialog
     */
    public class ThumbnailDialogAction extends AbstractAction {
        public ThumbnailDialogAction() {}
        public void actionPerformed(ActionEvent e) {
           
        }
    }

    //Create a new internal frame.
    protected  MyInternalFrame createFrame() {
        MyInternalFrame frame = new MyInternalFrame();
        frame.setVisible(true); //necessary as of 1.3
        desktop.add(frame);
        try {
            frame.setSelected(true);
        } catch (java.beans.PropertyVetoException e) {}
        return frame;
    }

    //Quit the application.
    protected void quit() {
        System.exit(0);
    }

    /**
     * Create the GUI and show it.  For thread safety,
     * this method should be invoked from the
     * event-dispatching thread.
     */
    private static void createAndShowGUI() {
        //Make sure we have nice window decorations.
        JFrame.setDefaultLookAndFeelDecorated(false);

        //Create and set up the window.
        InternalFrame frame = new InternalFrame();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        //Display the window.
        frame.setVisible(true);
    }

    public static void main(String[] args) {
        //Schedule a job for the event-dispatching thread:
        //creating and showing this application's GUI.
    	if (args.length !=0){
    		initfilename = args[0];
    	}
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                createAndShowGUI();
            }
        });
    }
}

