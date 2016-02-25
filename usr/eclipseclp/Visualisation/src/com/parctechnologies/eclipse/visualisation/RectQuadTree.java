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

import java.util.*;
import java.awt.*;

/**
 * Spatial, recursive data structure for storing a set of components S whose
 * bounds fall within a given rectangle R. Allows efficient addition of
 * components to S and removal of components from S. Also allows efficient
 * lookup of which components in S intersect an arbitrary given rectangle
 * contained by R. Assumes that the bounds of the component do not change while
 * it is in the RectQuadTree. If the bounds are to change, the component should
 * be removed and added again once the new bounds are set.
 */

class RectQuadTree
{
  // for type INTERIOR, sub nodes covering four quadrants.
  private RectQuadTree [] child;

  // kind of node:
  // NONEMPTY_LEAF: some components intersect bounds. Either this node has max
  // depth == 0 or all intersecting components contain this node's bounds.
  // EMPTY_LEAF: nuff said
  // INTERIOR: some components intersect bounds, max depth > 0 and node has
  // children.
  private int type;
  // values for type
  private static final int NONEMPTY_LEAF = 0;
  private static final int EMPTY_LEAF = 1;
  private static final int INTERIOR = 2;

  private static final int DEFAULT_MIN_SIDE = 20;

  // for type NONEMPTY_LEAF: collection of components which intersect this node's
  // bounds.
  private Collection intersectingComponents;
  private Rectangle bounds;

  // maximum nesting level.
  private int maxDepth;

  /**
   * create empty tree, with max splitting depth maxDepth.
   */
  private RectQuadTree(Rectangle bounds, int maxDepth)
  {
    this.bounds = bounds;
    this.maxDepth = maxDepth;
    type = EMPTY_LEAF;
  }

  private static int depthGivenBounds(Rectangle bounds)
  {
    int smallestDim = Math.min(bounds.width, bounds.height);
    int maxDepth =
     (int) (Math.log((double) smallestDim / (double) DEFAULT_MIN_SIDE) /
     Math.log(2.0)) - 1;
    return(maxDepth);
  }

  /**
   * Creates empty tree with maxDepth such that the side of any node is at most
   * DEFAULT_MIN_SIDE
   */
  public RectQuadTree(Rectangle bounds)
  {
    this(bounds, depthGivenBounds(bounds));
  }



  // public level methods

  /**
   * Add a component to this RectQuadTree.
   */
  public void addComponent(Component c)
  {
    addComponent(c, c.getBounds());
  }

  /**
   * Remove a component from this RectQuadTree.
   */
  public void removeComponent(Component c)
  {
    removeComponent(c, c.getBounds());
  }

  /**
   * Return the set of components within this RectQuadTree which intersect
   * rectangle.
   */
  public Collection getComponentsWithin(Rectangle rectangle)
  {
    HashSet result = new HashSet();
    addIntersectingComponentsWithin(rectangle, result);
    return(result);
  }

  /**
   * Assumes added component intersects bounds.
   *
     * type = empty leaf
     *
     * if(maxDepth == 0 OR c.bounds contains this.bounds)
     *    this.type -> NONEMPTY_LEAF
     *    initialise intersectingComponents.
     *    add c to self
     *
     * else
     *    this.type -> INTERIOR
     *    initialise child array
     *    add c to self
     *
     * ---
     *
     * type = nonempty leaf
     *
     * if(maxDepth == 0 OR c.bounds contains this.bounds)
     *    intersectingComponents.add(c)
     * else
     *    this.type -> INTERIOR
     *    initialise child array
     *    add all intersectingComponents to each child,
     *    intersectingComponents = null;
     *    add c to self.
     *
     * ---
     *
     * type = interior
     *
     * for each child:
     *    if(c.bounds intersects child.bounds)
     *      child.addComponent(c)
     *
     */
  private void addComponent(Component c, Rectangle cBounds)
  {
    if(type == EMPTY_LEAF)
    {
      if(maxDepth == 0 || cBounds.contains(bounds))
      {
        type = NONEMPTY_LEAF;
        initialiseIntersectingComponents();
        intersectingComponents.add(c);
      }
      else
      {
        type = INTERIOR;
        initialiseChild();
        addComponent(c, cBounds);
      }
      return;
    }
    if(type == NONEMPTY_LEAF)
    {
      if(maxDepth == 0 || cBounds.contains(bounds))
      {
        intersectingComponents.add(c);
      }
      else
      {
        type = INTERIOR;
        initialiseChild();
        for(int i = 0; i < child.length; i++)
        {
          child[i].type = NONEMPTY_LEAF;
          child[i].initialiseIntersectingComponents();
          child[i].intersectingComponents.addAll(intersectingComponents);
        }
        intersectingComponents = null;
        addComponent(c, cBounds);
      }
      return;
    }
    if(type == INTERIOR)
    {
      for(int i = 0; i < child.length; i++)
      {
        if(cBounds.intersects(child[i].bounds))
        {
          child[i].addComponent(c, cBounds);
        }
      }
    }
  }

  /**
   * type = empty leaf
   *    do nothing.
   *
   * type = nonempty leaf
   *    result.addAll(intersectingComponents)
   *
   * type = interior
   *   for i = 0 to 3
   *     child[i].addIntersectingComponents(result);
   */
  private void addIntersectingComponents(Collection result)
  {
    if(type == EMPTY_LEAF)
    {
      return;
    }
    if(type == NONEMPTY_LEAF)
    {
      result.addAll(intersectingComponents);
      return;
    }
    if(type == INTERIOR)
    {
      for(int i = 0; i < child.length; i++)
      {
        child[i].addIntersectingComponents(result);
      }
    }
  }

  /**
   * Add all components which are within this tree and which intersect rectangle
   * to result. Assumes rectangle intersects this.bounds.
   *
   * type = empty leaf
   *    do nothing
   *
   * type = nonempty leaf
   *    for each member of intersectingComponents x
   *       if x intersects rectangle
   *         result.add(x)
   *
   * type = interior
   *    for i = 0 to 3
   *      if rectangle contains child[i].bounds
   *          child[i].addIntersectingComponents(result))
   *          return
   *      else if rectangle intersects child[i].bounds
   *          child[i].addIntersectingComponents(rectangle, result)
   */

  private void addIntersectingComponentsWithin(Rectangle rectangle,
                                               Collection result)
  {
    if(type == EMPTY_LEAF)
    {
      return;
    }
    if(type == NONEMPTY_LEAF)
    {
      Iterator i = intersectingComponents.iterator();
      Component x;
      while(i.hasNext())
      {
        x = (Component) i.next();
        if(x.getBounds().intersects(rectangle))
        {
          result.add(x);
        }
      }
    }
    if(type == INTERIOR)
    {
      for(int i = 0; i < child.length; i++)
      {
        if(rectangle.contains(child[i].bounds))
        {
          child[i].addIntersectingComponents(result);
        }
        else
        {
          if(rectangle.intersects(child[i].bounds))
          {
            child[i].addIntersectingComponentsWithin(rectangle, result);
          }
        }
      }
    }
  }


  /**
   * Assumes that component is member of the set of intersecting components.
   *
   * type = emtpy leaf
   *    dp nothing.
   *
   * type = nonempty leaf
   *    intersectingComponents.remove(component).
   *    if intersectingComponents is empty
   *    intersectingComponents -> null
   *    type -> empty leaf
   *
   * type = interior
   *    int empty leaf children -> 0;
   *    int nonempty leaf children -> 0;
   *    for i = 0 to 3
   *       if component.bounds intersects child[i].bounds
   *          child[i].removeComponent(component)
   *       if child[i].type == empty leaf
   *          empty leaf children ++
   *       if child[i].type == nonempty leaf
   *          nonempty leaf children ++
   *    end for
   *    if empty leaf children == 4
   *      type -> empty
   *      child -> null
   *    if nonemtpy leaf children == 4 &&
   *      child[0].intersectingComponents.equals child[1].intersectingComponents &&
   *      child[1].intersectingComponents.equals child[2].intersectingComponents &&
   *      child[2].intersectingComponents.equals child[3].intersectingComponents &&
   *      foreach comp in child[0].intersectingComponents
   *        comp.bounds contains this.bounds.
   *       then
   *           type -> nonempty leaf
   *           intersectingComponents = child[0].intersectingComponents
   *           child -> null
   */
  private void removeComponent(Component c, Rectangle cBounds)
  {
    if(type == EMPTY_LEAF)
    {
      return;
    }
    if(type == NONEMPTY_LEAF)
    {
      intersectingComponents.remove(c);
      if(intersectingComponents.isEmpty())
      {
        intersectingComponents = null;
        type = EMPTY_LEAF;
      }
      return;
    }
    if(type == INTERIOR)
    {
      int emptyLeafChildren = 0;
      int nonEmptyLeafChildren = 0;
      for(int i = 0; i < child.length ; i++)
      {
        if(cBounds.intersects(child[i].bounds))
        {
          child[i].removeComponent(c, cBounds);
        }
        if(child[i].type == EMPTY_LEAF)
        {
          emptyLeafChildren++;
        }
        else
        {
          if(child[i].type == NONEMPTY_LEAF)
          {
            nonEmptyLeafChildren++;
          }
        }
      }
      if(emptyLeafChildren == child.length)
      {
        type = EMPTY_LEAF;
        child = null;
        return;
      }
      if(nonEmptyLeafChildren == child.length &&
         child[0].intersectingComponents.equals(child[1].intersectingComponents) &&
         child[1].intersectingComponents.equals(child[2].intersectingComponents) &&
         child[2].intersectingComponents.equals(child[3].intersectingComponents))
      {
        boolean allIntersectingComponentsContainBounds = true;
        Iterator i = child[0].intersectingComponents.iterator();
        Component c2;
        while(i.hasNext() && allIntersectingComponentsContainBounds)
        {
          c2 = (Component) i.next();
          if(!c2.getBounds().contains(bounds))
          {
            allIntersectingComponentsContainBounds = false;
          }
        }
        if(allIntersectingComponentsContainBounds)
        {
          type = NONEMPTY_LEAF;
          intersectingComponents = child[0].intersectingComponents;
          child = null;
        }
      }
    }
  }

  private void initialiseChild()
  {
    child = new RectQuadTree[4];
    int widthLeft = bounds.width/2;
    int widthRight = bounds.width - widthLeft;
    int heightTop = bounds.height/2;
    int heightBottom = bounds.height - heightTop;
    int newDepth = maxDepth - 1;
    child[0] =
      new RectQuadTree(new Rectangle(bounds.x, bounds.y,
                                     widthLeft, heightTop), newDepth);
    child[1] =
      new RectQuadTree(new Rectangle(bounds.x + widthLeft, bounds.y,
                                     widthRight, heightTop), newDepth);
    child[2] =
      new RectQuadTree(new Rectangle(bounds.x, bounds.y + heightTop,
                                     widthLeft, heightBottom), newDepth);
    child[3] =
      new RectQuadTree(new Rectangle(bounds.x + widthLeft, bounds.y + heightTop,
                                     widthRight, heightBottom), newDepth);
  }

  private void initialiseIntersectingComponents()
  {
    intersectingComponents = new HashSet();
  }


  // testing code
  private static void test()
  {
    int MAX_DEPTH = 7;
    int COMPONENTS = 50;
    int COMPONENTS_TO_REPLACE = 25;
    int TESTS = 2000;
    int width = 2000;
    int height = 2000;
    int cHeight, cWidth;
    int replacement_index;
    Random random = new Random(1);
    Component[] component = new Component[COMPONENTS];
    RectQuadTree rqt =
      new RectQuadTree(new Rectangle(0,0,width, height), MAX_DEPTH);

    for(int i = 0; i < COMPONENTS; i++)
    {
      component[i] = new Label("");
      cWidth = random.nextInt(width/2);
      cHeight = random.nextInt(height/2);
      component[i].
        setBounds(random.nextInt(width - cWidth),
                  random.nextInt(height - cHeight),
                  cWidth, cHeight);
      rqt.addComponent(component[i]);
      System.out.println("added component to rqt with bounds "+component[i].getBounds());
    }

    System.out.println("Tests for 20 x 20: "+tests(rqt, TESTS, component, 20, 20));
    System.out.println("Tests for 20 x 500: "+tests(rqt, TESTS, component, 20, 500));
    System.out.println("Tests for 500 x 20: "+tests(rqt, TESTS, component, 500, 20));
    System.out.println("Tests for 500 x 500: "+tests(rqt, TESTS, component, 500, 500));

    for(int i = 0; i < COMPONENTS_TO_REPLACE; i++)
    {
      replacement_index = random.nextInt(COMPONENTS);
      rqt.removeComponent(component[replacement_index]);
      cWidth = random.nextInt(width/2);
      cHeight = random.nextInt(height/2);
      component[replacement_index] = new Label("");
      component[replacement_index].
        setBounds(random.nextInt(width - cWidth),
                  random.nextInt(height - cHeight),
                  cWidth, cHeight);
      rqt.addComponent(component[replacement_index]);
      System.out.println("replaced component : new one has bounds "+
        component[replacement_index].getBounds());
    }
    System.out.println("Post-replacement tests for 20 x 20: "+tests(rqt, TESTS, component, 20, 20));
    System.out.println("Post-replacement tests for 20 x 500: "+tests(rqt, TESTS, component, 20, 500));
    System.out.println("Post-replacement tests for 500 x 20: "+tests(rqt, TESTS, component, 500, 20));
    System.out.println("Post-replacement tests for 500 x 500: "+tests(rqt, TESTS, component, 500, 500));
  }

  private static boolean tests(RectQuadTree rqt, int tests,
                               Component[] component, int testWidth, int testHeight)
  {
    Rectangle testRect;
    Random random = new Random(2);
    boolean allTestsSucceeded = true;

    for(int i = 0; i < tests; i++)
    {
      testRect = new Rectangle(random.nextInt(rqt.bounds.width - testWidth),
                               random.nextInt(rqt.bounds.height - testHeight),
                               testWidth, testHeight);
      if(!test(rqt, testRect, component))
      {
        System.out.println("test failed for testRect = "+testRect);
        allTestsSucceeded = false;
      }
    }
    return(allTestsSucceeded);
  }

  private static boolean test(RectQuadTree rqt, Rectangle testRect,
                              Component[] component)
  {
    Collection rqtResults = rqt.getComponentsWithin(testRect);
    Collection naiveResults = new HashSet();
    for(int i = 0 ; i < component.length; i++)
    {
      if(component[i].getBounds().intersects(testRect))
      {
        naiveResults.add(component[i]);
      }
    }
    //System.out.println("Rqt results: "+rqtResults);
    //System.out.println("Naive results: "+naiveResults);
    return(rqtResults.equals(naiveResults));
  }
}
