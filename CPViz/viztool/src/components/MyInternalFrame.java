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


import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JInternalFrame;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JToolBar;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;

import org.apache.batik.swing.JSVGCanvas;
import org.apache.batik.swing.JSVGScrollPane;
import org.apache.batik.swing.gvt.GVTTreeRenderer;
import org.apache.batik.swing.gvt.GVTTreeRendererAdapter;
import org.apache.batik.swing.gvt.GVTTreeRendererEvent;
import org.apache.batik.swing.gvt.GVTTreeRendererListener;
import org.apache.batik.swing.svg.GVTTreeBuilderAdapter;
import org.apache.batik.swing.svg.GVTTreeBuilderEvent;
import org.apache.batik.swing.svg.SVGDocumentLoaderAdapter;
import org.apache.batik.swing.svg.SVGDocumentLoaderEvent;
import org.apache.batik.swing.svg.SVGLoadEventDispatcherAdapter;
import org.apache.batik.swing.svg.SVGLoadEventDispatcherEvent;
import org.apache.batik.util.Platform;
import org.apache.batik.util.gui.resource.ToolBarFactory;
import org.apache.batik.util.resources.ResourceManager;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.events.Event;
import org.w3c.dom.events.EventListener;
import org.w3c.dom.events.EventTarget;
import org.apache.batik.script.Window;



import java.awt.event.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;
import java.awt.*;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

class MyActionListener implements ActionListener {
	
	MyInternalFrame Me;
	MyActionListener(MyInternalFrame me){
		super();
		Me = me;
	}
	public void actionPerformed(ActionEvent arg0) {
		// TODO Auto-generated method stub
	//	System.out.println("here");
		if (Me.FileName != null)
		Me.svgCanvas.setURI(Me.FileName);
    	
	}
	
}
/* Used by InternalFrameDemo.java. */
public class MyInternalFrame extends JInternalFrame {
	  

    static int openFrameCount = 0;
    static final int xOffset = 30, yOffset = 30;
    JSVGCanvas svgCanvas;
    JSVGScrollPane panelCanvas;
    JPanel panel;
    boolean first = true;
    boolean veryfirst=true;
    boolean ReRender = false;
    AffineTransform oldTransform = null;
    AffineTransform OriginalTransform = null;
    JButton zoomin = new JButton("Zoom In");
    JButton zoomout = new JButton("Zoom Out");
    JButton OriginalSize = new JButton("Original Size");
    String FileName = null;
    Document document;
    Window window;

 
    public void registerListeners() {
        // Gets an element from the loaded document.
        Element elt = document.getElementById("elt-id");
        EventTarget t = (EventTarget)elt;

        // Adds a 'onload' listener
        t.addEventListener("SVGLoad", new OnLoadAction(), false);

        // Adds a 'onclick' listener
        t.addEventListener("click", new OnClickAction(), false);
    }

    public class OnLoadAction implements EventListener {
        public void handleEvent(Event evt) {
            // Perform some actions here...
            
            // ...for example start an animation loop:
            window.setInterval(new Animation(), 50);
        }
    }

    public class OnClickAction implements EventListener {
        public void handleEvent(Event evt) {
            // Perform some actions here...
//System.out.println("element clicked");
            // ...for example schedule an action for later:
           // window.setTimeout(new DelayedTask(), 500);
        }
    }

    public class Animation implements Runnable {
        public void run() {
            // Insert animation code here...
        }
    }

    public class DelayedTask implements Runnable {
        public void run() {
            // Perform some actions here...

            // ...for example displays an alert dialog:
            window.alert("Delayed Action invoked!");
        }
    }

    public void setURI(String fname){
    	if (svgCanvas!=null){
    		if (!veryfirst){
    			ReRender = true;
    			oldTransform = svgCanvas.getRenderingTransform();
    		}
    		else{
    			OriginalTransform = svgCanvas.getRenderingTransform();
    			veryfirst = false;
    			ReRender = false;
    		}
    		svgCanvas.setURI(fname);
    		FileName = fname;
    		String s = fname.substring(8);
    		int pos = s.lastIndexOf("/");
    		if (pos==-1)
    			pos = s.lastIndexOf("\\");
    		if (pos !=-1)
    			setTitle(s.substring(pos+1));
    		else
    		   setTitle(s);
    		
    	}
    }
    public void setToOldTransform()
    {
    	ReRender = true;
    	svgCanvas.repaint();
    }
    public void SetVeryFirst(boolean b)
    {
    	veryfirst = b;
    }
    public MyInternalFrame() {
        super("Document #" + (++openFrameCount), 
              true, //resizable
              true, //closable
              true, //maximizable
              true);//iconifiable

        //...Create the GUI and put it in the window...

        //...Then set the window size or call pack...
        setSize(300,300);
       
        //Set the window's location.
        setLocation(xOffset*openFrameCount, yOffset*openFrameCount);
        svgCanvas = new JSVGCanvas() {
            Dimension screenSize;

            {
                screenSize = Toolkit.getDefaultToolkit().getScreenSize();
                setMaximumSize(screenSize);
            }

            public Dimension getPreferredSize(){
                Dimension s = super.getPreferredSize();
                if (s.width > screenSize.width) s.width =screenSize.width;
                if (s.height > screenSize.height) s.height = screenSize.height;
                return s;
            }


            /**
             * This method is called when the component knows the desired
             * size of the window (based on width/height of outermost SVG
             * element). We override it to immediately pack this frame.
             */
            public void setMySize(Dimension d) {
              //  setPreferredSize(d);
            	//System.out.println("setmysize"+d);
                invalidate();
                repaint();
                
            }
           	protected void renderGVTTree() {
                //System.out.println("renderGVRTRee");
           		Rectangle visRect = getRenderRect();
                   if (gvtRoot == null || visRect.width <= 0 || visRect.height <= 0) {
                       return;
                   }

                   // Renderer setup.
                   if (renderer == null || renderer.getTree() != gvtRoot) {
                       renderer = createImageRenderer();
                       renderer.setTree(gvtRoot);
                   }

                   // Area of interest computation.
                   AffineTransform inv;
                   try {
                       inv = renderingTransform.createInverse();
                   } catch (NoninvertibleTransformException e) {
                       throw new IllegalStateException( "NoninvertibleTransformEx:" + e.getMessage() );
                   }
                   Shape s = inv.createTransformedShape(visRect);
                   if (ReRender){
                   	 //  System.out.println("render ");
                       if (oldTransform != null){
                       	 
                       	svgCanvas.setRenderingTransform(oldTransform,false);
                   //    	System.out.println("render is set old");
                       
                       }
                       ReRender = false;
                      }
                   // Rendering thread setup.
                   gvtTreeRenderer = new GVTTreeRenderer(renderer, renderingTransform,
                                                         doubleBufferedRendering, s,
                                                         visRect.width, visRect.height);
                   gvtTreeRenderer.setPriority(Thread.MIN_PRIORITY);

                   Iterator it = gvtTreeRendererListeners.iterator();
                   while (it.hasNext()) {
                       gvtTreeRenderer.addGVTTreeRendererListener
                           ((GVTTreeRendererListener)it.next());
                   }

                   // Disable the dispatch during the rendering
                   // to avoid concurrent access to the GVT tree.
                   if (eventDispatcher != null) {
                       eventDispatcher.setEventDispatchEnabled(false);
                   }

                   gvtTreeRenderer.start();
               }

           
        };
        panelCanvas = new  JSVGScrollPane(svgCanvas);  
        panel = new JPanel(new BorderLayout());
    	SVGDocumentLoaderAdapter dla = new SVGDocumentLoaderAdapter() {
    		public void documentLoadingStarted(SVGDocumentLoaderEvent e) {
    			//updateStatus("Document Loading...");
    		}


    		public void documentLoadingCompleted(SVGDocumentLoaderEvent e) {
    			//updateStatus("Document Loaded.");
    		}
    	};
    	svgCanvas.addSVGDocumentLoaderListener(dla);
    	svgCanvas.addSVGLoadEventDispatcherListener
        (new SVGLoadEventDispatcherAdapter() {
                public void svgLoadEventDispatchStarted
                    (SVGLoadEventDispatcherEvent e) {
                    // At this time the document is available...
                    document = svgCanvas.getSVGDocument();
                    // ...and the window object too.
                    window = svgCanvas.getUpdateManager().
                        getScriptingEnvironment().createWindow();
                    // Registers the listeners on the document
                    // just before the SVGLoad event is
                    // dispatched.
                    registerListeners();
                    // It is time to pack the frame.
                    pack();
                }
            });

    	 svgCanvas.addMouseListener(new MouseAdapter() {
        	 public void mouseClicked(MouseEvent e) {
        		 
        		// System.out.println("view "+svgCanvas.getViewBoxTransform());
        		// System.out.println(" x "+e.getX()+" y "+e.getY());
        		 AffineTransform at;
                 at = svgCanvas.getViewBoxTransform();
                
                 if (at != null) {
       //          	 System.out.println("mouse move viewBoxTrans"+at+"mousepos "+
          //       	new Point2D.Float(e.getX(), e.getY()));
                     try {
						at = at.createInverse();
					} catch (NoninvertibleTransformException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
                     Point2D p2d =
                         at.transform(new Point2D.Float(e.getX(), e.getY()),
                                      null);
                   //  System.out.println("point in view "+p2d);
                 }
        		
        	    }

                public void mouseExited(MouseEvent e) {
                    Dimension dim = svgCanvas.getSize();
                   
                }
            });
    	
    	GVTTreeBuilderAdapter tba = new GVTTreeBuilderAdapter() {
    		public void gvtBuildStarted(GVTTreeBuilderEvent e) {
    		//	updateStatus("Build Started...");
    		}


    		public void gvtBuildCompleted(GVTTreeBuilderEvent e) {
    			//updateStatus("Build Done.");
    		}
    	};
    	svgCanvas.addGVTTreeBuilderListener(tba);
    	
    			GVTTreeRendererAdapter tra=		new GVTTreeRendererAdapter() {
    		public void gvtRenderingPrepare(GVTTreeRendererEvent e) {
    			//updateStatus("Rendering Started...");
    		}


    		public void gvtRenderingCompleted(GVTTreeRendererEvent e) {
    			//updateStatus("");
    		}
    	};
    	svgCanvas.addGVTTreeRendererListener(tra);
    	JPanel panel1 = new JPanel(new GridLayout(1,1));
    	panel1.add(zoomin);
    	panel1.add(zoomout);
    	panel1.add(OriginalSize);
    	panel.add(panel1,BorderLayout.NORTH);
    	panel.add(panelCanvas,BorderLayout.CENTER);
    	panelCanvas.setScrollbarsAlwaysVisible(true);
    	//svgCanvas.setURI("file:///C:/testsaros/result/tree_1.svg");
    	javax.swing.ActionMap cMap = svgCanvas.getActionMap();
    	zoomin.addActionListener(cMap.get(JSVGCanvas.ZOOM_IN_ACTION));
		zoomout.addActionListener(cMap.get(JSVGCanvas.ZOOM_OUT_ACTION));
		OriginalSize.addActionListener(new MyActionListener(this));
		
    	AffineTransform vb = svgCanvas.getViewBoxTransform();
     //	System.out.println("view box trans"+vb);
    	 getContentPane().add(panel, BorderLayout.CENTER);
    	 addComponentListener(new ComponentAdapter() {
             public void componentResized(ComponentEvent e) {
          	  
  //           	System.out.println("JSVG canvas component resize listener");
             	AffineTransform vb = svgCanvas.getViewBoxTransform();
    //         	System.out.println("view box trans"+vb);
             	 AffineTransform at = svgCanvas.getRenderingTransform();
      //        	System.out.println("new render"+at);
              Dimension sp = panel.getSize();
       	   Dimension c = svgCanvas.getSize();
       	//   System.out.println("sp "+sp+" c "+c);
       	   double top = sp.getHeight();///c.getHeight();s.getHeight()-t.getHeight()-sbar.getHeight()-sslide.getHeight();
       
       	   if (top > 40.0){
       	   double scale = (top-20.0)/(double)(c.getHeight());
       	   
       	  // System.out.println("scale "+scale);
       	  at= svgCanvas.getRenderingTransform();
             at.concatenate(AffineTransform.getScaleInstance(scale,scale)); 
           //  if (first){
            // at = new AffineTransform(scale,0.0,0.0,scale,-vb.getTranslateX(),-vb.getTranslateY());
            // first = false;
           //  svgCanvas.setRenderingTransform(at);
            // }
             vb = svgCanvas.getViewBoxTransform();
         // 	System.out.println("new view box trans"+vb);
          	 at = svgCanvas.getRenderingTransform();
        // 	System.out.println("new render"+at);
            // leftpane.AffineScale = scale;
         //   at.concatenate(AffineTransform.getTranslateInstance(c.getWidth()/2, 0));
         //  svgCanvas.setRenderingTransform(at);
        /* 	int tx=0,ty=0;
         	at = AffineTransform.getTranslateInstance(tx, ty);
         	AffineTransform rat = svgCanvas.getRenderingTransform();
         	Dimension dim = svgCanvas.getSize();
            int x = dim.width / 2;
            int y = dim.height / 2;
            AffineTransform t = AffineTransform.getTranslateInstance(x, y);
            t.concatenate(at);
            t.translate(-x, -y);
            t.concatenate(rat);
            svgCanvas.setRenderingTransform(t);*/
         	svgCanvas.setSize(sp);
         //	setURI(FileName);
         /*	javax.swing.SwingUtilities.invokeLater(new Runnable() {
                 public void run() {
                	 panel.validate();
                	 panel.repaint();
                 }
             });*/
         	
       	   }
              
            
             }
         }); 
    }
}
