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
// Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
// 
// Contributor(s): 
// 
// END LICENSE BLOCK

package com.parctechnologies.eclipse.visualisation;

import com.parctechnologies.eclipse.*;

import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.*;
import java.awt.Color;
import java.awt.Component;
import java.util.*;
import java.awt.event.ActionEvent;
import javax.swing.*;
import javax.swing.table.*;
import att.grappa.*;


/**
 * Meta-viewlet type used to treat many other viewlet types as one.
 *
 * <p> The multi-viewlet data holds both the original ViewletData and
 * the original ViewletTye.
 * */
public class MultiViewletType implements ViewletType, ViewletFactory {
    private SymRef symRef;

    private TableCellRenderer tableCellRenderer;

    /** The actual types which this meta-type wraps */
    private Collection viewletTypeCollection;

    public MultiViewletType(Collection viewletTypeCollection) {
        this.viewletTypeCollection = new LinkedList();
        for(Iterator it = viewletTypeCollection.iterator(); it.hasNext();) {
            addViewletType((ViewletType)it.next());
        }
    }

    public void addViewletType(ViewletType viewletType) {
        viewletTypeCollection.add(viewletType);
    }

    public Collection getViewletTypeCollection() {
        return viewletTypeCollection;
    }

    /* ViewletFactory methods */    
    public boolean canBuildFrom(ElementType elementType)
    {
	return(true);
    }
    
    public ViewletData build()
    {
        // by default build the first type in the list
        ViewletType type = (ViewletType)viewletTypeCollection.iterator().next();
        ViewletData subData = ((ViewletFactory)type).build();
	return new Data(type, subData);
    }
    
    public String getDescription()
    {
	return("Multi viewlet");
    }
    
    public void setSymRef(SymRef symRef)
    {
	this.symRef = symRef;
        for(Iterator it = viewletTypeCollection.iterator(); it.hasNext();) {
            ViewletType viewletType = (ViewletType) it.next();
            viewletType.setSymRef(new SymRef(viewletType,
                                             this.getSymRef(),
                                             viewletType.getClass().getName()));
        }

    }
    public SymRef getSymRef()
    {
	return(symRef);
    }



    /* ViewletType methods */
    public synchronized TableCellRenderer getTableCellRenderer() {
	if (tableCellRenderer == null) {
	    tableCellRenderer = new CellRenderer();
	}
	return tableCellRenderer;
    }

    synchronized Class getCustomRendererClass() {
//  	if (customRenderer == null) {
//  	    customRenderer = new CustRenderer();
//  	}
//  	return customRenderer;
      return com.parctechnologies.eclipse.visualisation.viewers.CustRenderer.class;
    }

    public void customizeElement(ViewletDataStore store,
                                 java.util.List index,
                                 Element element) {
        Data data = (Data)(store.getViewletDataAt(index));
        if (DebuggingSupport.logMessages) {
            DebuggingSupport.logMessage(this,"MultiViewletType customizeElement called with data="+data+" element="+element);
        }
        ViewletData realData = data.getViewletData();
        ViewletType realType = data.getViewletType();
        if (DebuggingSupport.logMessages) {
            DebuggingSupport.logMessage(this,"MultiViewletType customizeElement called with data="+data+" element="+element);
            DebuggingSupport.logMessage(this,"MultiViewletType customizeElement called with realData="+realData+" realType="+realType);
        }
        // pass the customization on to the actual type of this element
        MultiViewletDataStore multiStore=(MultiViewletDataStore)store;
        data.getViewletType().customizeElement(multiStore.getViewletDataStore(data.getViewletType()), index, element);
    }

    private Map rangeToTypeRangeMap(ViewletDataStore store,
                                    ViewletRange range) {
	Iterator indexListIterator = range.iterator();
	java.util.List currentIndex;
	Map typeRangeMap = new HashMap();
	while(indexListIterator.hasNext()) {
            currentIndex = (java.util.List) indexListIterator.next();
            Data data = (Data) store.getViewletDataAt(currentIndex);
            ViewletType type = data.getViewletType();
            ViewletRange typeRange = (ViewletRange) typeRangeMap.get(type);
            if (typeRange == null) {
                typeRange = new ViewletRangeCollection();
                typeRangeMap.put(type, typeRange);
            }
            typeRange.add(currentIndex);
        }
        if (DebuggingSupport.logMessages) {
            DebuggingSupport.logMessage(this,
                                        "Range="+range+
                                        "Range to type map="+typeRangeMap);
        }
        return typeRangeMap;
    }


    public BatchGoal collectPreBuildGoal(Viewer viewer,
                                         ViewletDataStore store,
                                         ViewletRange range)
    {
	BatchGoal result = new BatchGoal();
	Map typeRangeMap = rangeToTypeRangeMap(store, range);
        // now collect together all the type specific batch goals
        for(Iterator it = viewletTypeCollection.iterator(); it.hasNext(); ) {
            ViewletType type = (ViewletType) it.next();
            ViewletRange typeRange = (ViewletRange) typeRangeMap.get(type);
            ViewletDataStore typeStore = ((MultiViewletDataStore)store).getViewletDataStore(type);
            if (typeRange == null) {
                typeRange = new ViewletRangeCollection();
            }
            result.add(type.collectPreBuildGoal(viewer, typeStore, typeRange));
        }
	return result;
    }

    public void startBuild(Viewer viewer,
                           ViewletDataStore store,
                           ViewletRange range,
                           List results) {
	Map typeRangeMap = rangeToTypeRangeMap(store, range);
        Iterator resultIt = results.iterator();
        // now collect together all the type specific batch goals
        for(Iterator it = viewletTypeCollection.iterator(); it.hasNext(); ) {
            ViewletType type = (ViewletType) it.next();
            ViewletRange typeRange = (ViewletRange) typeRangeMap.get(type);
            ViewletDataStore typeStore = ((MultiViewletDataStore)store).getViewletDataStore(type);
            List typeResult = (List) resultIt.next();
            if (typeRange == null) {
                typeRange = new ViewletRangeCollection();
            }
            type.startBuild(viewer, typeStore, typeRange, typeResult);
        }
    }


    public void stopBuild()
    {}

    public BatchGoal collectPreUpdateGoal(Viewer viewer,
                                          ViewletDataStore store,
					  ViewletRange range,
					  UpdateEvent updateEvent)
    {
	BatchGoal result = new BatchGoal();
	Map typeRangeMap = rangeToTypeRangeMap(store, range);
        // now collect together all the type specific batch goals
        for(Iterator it = viewletTypeCollection.iterator(); it.hasNext(); ) {
            ViewletType type = (ViewletType) it.next();
            ViewletRange typeRange = (ViewletRange) typeRangeMap.get(type);
            ViewletDataStore typeStore = ((MultiViewletDataStore)store).getViewletDataStore(type);
            if (typeRange == null) {
                typeRange = new ViewletRangeCollection();
            }
            result.add(type.collectPreUpdateGoal(viewer, typeStore, typeRange, updateEvent));
        }
        if (DebuggingSupport.logMessages) {
            DebuggingSupport.logMessage(this,"Pre-update goals="+result);
        }
	return result;
    }

    public void startUpdate(Viewer viewer,
                            ViewletDataStore store,
			    ViewletRange range,
			    List results,
			    UpdateEvent updateEvent)
    {
        if (DebuggingSupport.logMessages) {
            DebuggingSupport.logMessage(this,"start-update results="+results);
        }
	Map typeRangeMap = rangeToTypeRangeMap(store, range);
        Iterator resultIt = results.iterator();
        // now collect together all the type specific batch goals
        for(Iterator it = viewletTypeCollection.iterator(); it.hasNext(); ) {
            ViewletType type = (ViewletType) it.next();
            ViewletRange typeRange = (ViewletRange) typeRangeMap.get(type);
            ViewletDataStore typeStore = ((MultiViewletDataStore)store).getViewletDataStore(type);
            List typeResult = (List) resultIt.next();
            if (typeRange == null) {
                typeRange = new ViewletRangeCollection();
            }
            type.startUpdate(viewer, typeStore, typeRange, typeResult, updateEvent);
        }
    }

    public void stopUpdate(Viewer viewer,
                           ViewletDataStore store,
			   ViewletRange range)
    {
      // unsure why this is parameterised.
	//setUpdating("no");
	//setUpdating(viewer, range, Data.UPDATING_NOT);
    }





    /*
     * Data is a viewlet which can monitor elements of any type. It is
     * responsible for:
     * <ul>
     * <li> Maintaining a record of the text representation of the term.
     * </ul>
     */
    public static class Data extends ViewletDataImpl
    {
        private ViewletType viewletType;
        private ViewletData viewletData;

	public Data(ViewletType viewletType, ViewletData viewletData)
	{
	    super();
            this.viewletType = viewletType;
            this.viewletData = viewletData;
	}

        public void setViewletData(ViewletData viewletData) {
            this.viewletData = viewletData;
        }

        public ViewletData getViewletData() {
            return viewletData;
        }

        public void setViewletType(ViewletType viewletType) {
            this.viewletType = viewletType;
        }

        public ViewletType getViewletType() {
            return viewletType;
        }

        public boolean getHoldsOnUpdates() {
            return viewletData.getHoldsOnUpdates();
        }
        
        public void setHoldsOnUpdates(boolean newValue) {
            viewletData.setHoldsOnUpdates(newValue);
        }

        public String toString() {
            return viewletData.toString();
        }
    }


    /**
     * Return a collection of actions which can be applied to viewlets
     * in this table
     */
    public Collection getActions(ViewletDataStore store,
                                 ViewletRange range) {
        Collection ll = new LinkedList();
        //ll.add(new ChangeTypeAction(viewletTypeCollection, store, range));
        for(Iterator it = viewletTypeCollection.iterator(); it.hasNext();) {
          ViewletType type = (ViewletType) it.next();
          ViewletDataStore typeStore =
            ((MultiViewletDataStore)store).getViewletDataStore(type);
          // filter the range to only those indices which are applicable to
          ViewletRange typeRange = typeStore.createRange(range);
          ll.addAll(type.getActions(typeStore, typeRange ));
        }
        // now combine any duplicated actions in the list by combining
        // them into a single MultiAction
        Map nameMultiActionMap = new TreeMap();
        for(Iterator it = ll.iterator(); it.hasNext(); ) {
            Action action = (Action)it.next();
            String name = (String)action.getValue(Action.NAME);
            MultiAction multiAction = (MultiAction)nameMultiActionMap.get(name);
            if (multiAction == null) {
                multiAction =
                    new MultiAction(name,
                                    (Icon)action.getValue(Action.SMALL_ICON));
                nameMultiActionMap.put(name, multiAction);
            }
            multiAction.actions.add(action);
        }
        return nameMultiActionMap.values();
    }

    protected class MultiAction extends AbstractAction {
        /** Holds the type specific sub-actions */
        Collection actions;

        public MultiAction(String name, Icon icon) {
            super(name);
            putValue(Action.SMALL_ICON, icon);
            actions = new LinkedList();
        }

        /**
         * Perform all the sub-actions contained within
         */
        public void actionPerformed(ActionEvent event) {
            for(Iterator it = actions.iterator(); it.hasNext(); ) {
                Action action = (Action)it.next();
                action.actionPerformed(event);
            }
        }
    }

    protected class ChangeTypeAction extends ViewletAction {
        protected ViewletDataStore viewletDataStore;
        protected ViewletRange viewletRange;
        protected Collection viewletTypeCollection;

        public ChangeTypeAction(Collection viewletTypeCollection,
                                ViewletDataStore store,
                                ViewletRange range) {
            super("Change type");
            putValue(Action.NAME, "Change type");
            putValue(Action.LONG_DESCRIPTION,
                     "Change the type of viewlet at this location");
            putValue(Action.SHORT_DESCRIPTION,
                     "Change type of viewlet");
            //putValue(Action.SMALL_ICON, new FadeIcon(20, 20));
            this.viewletTypeCollection = viewletTypeCollection;
            this.viewletRange = range;
            this.viewletDataStore = store;
            // temporariy disable
            setEnabled(false);
        }

        public void actionPerformed() {
            // do nothing for now
        }
    }

    /**
     * The default text cell render
     */
    private class CellRenderer extends DefaultTableCellRenderer {
        
	public CellRenderer() {
	    super();
	}

	public Component getTableCellRendererComponent(JTable table,
						       Object value,
						       boolean isSelected,
						       boolean hasFocus,
						       int row,
						       int column) {
            // pass the request up to the correct viewlet type
	    Data data = (Data)value;
            return 
                data.getViewletType().getTableCellRenderer().
                getTableCellRendererComponent(table,
                                              data.getViewletData(),
                                              isSelected,
                                              hasFocus,
                                              row,
                                              column);
	}
    }

}

