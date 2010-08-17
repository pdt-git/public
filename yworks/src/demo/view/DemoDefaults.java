/****************************************************************************
 **
 ** This file is part of yFiles-2.7.0.1. 
 ** 
 ** yWorks proprietary/confidential. Use is subject to license terms.
 **
 ** Redistribution of this file or of an unauthorized byte-code version
 ** of this file is strictly forbidden.
 **
 ** Copyright (c) 2000-2010 by yWorks GmbH, Vor dem Kreuzberg 28, 
 ** 72070 Tuebingen, Germany. All rights reserved.
 **
 ***************************************************************************/
package demo.view;

import java.awt.Color;
import java.util.Map;

import y.base.Node;
import y.base.NodeCursor;
import y.view.Arrow;
import y.view.GenericNodeRealizer;
import y.view.Graph2D;
import y.view.Graph2DView;
import y.view.NodeRealizer;
import y.view.PolyLineEdgeRealizer;
import y.view.ShinyPlateNodePainter;
import y.view.GenericNodeRealizer.Factory;

/**
 * Provides default node and edge realizer configurations used by most demos.
 */
public class DemoDefaults {
  
  /**
   * The Name of the GenericNodeRealizer configuration of the default node used by most demos.  
   */
  public static final String NODE_CONFIGURATION = "DemoDefaults#Node";           
  
  /**
   * The default node fill color used by most demos 
   */
  public static final Color DEFAULT_NODE_COLOR = new Color(255, 153, 0);

  /**
   * The default node line color used by most demos. This is set to <code>null</code> meaning no border is drawn.
   */
  public static final Color DEFAULT_NODE_LINE_COLOR = null;

  /**
   * The default secondary or contract color used by most demos 
   */
  public static final Color DEFAULT_CONTRAST_COLOR = new Color(202,227,255);
    
  static {
    Factory factory = GenericNodeRealizer.getFactory();
    Map configurationMap = factory.createDefaultConfigurationMap();
    
    ShinyPlateNodePainter painter = new ShinyPlateNodePainter();
    // ShinyPlateNodePainter has an option to draw a drop shadow that is more efficient
    // than wrapping it in a ShadowNodePainter.
    painter.setDrawShadow(true);
    
    configurationMap.put(GenericNodeRealizer.Painter.class, painter);
    configurationMap.put(GenericNodeRealizer.ContainsTest.class, painter);
    factory.addConfiguration(NODE_CONFIGURATION, configurationMap);       
  };

  /**
  * <p>
  * The default representation used for a node is provided by a {@link y.view.GenericNodeRealizer} 
  * that uses the configuration as returned by {@link #createDefaultNodeConfiguration} 
  * under the name <code>DemoDefaultNode</code>. 
  * The default colors (fill, border) for a node are set to {@link #DEFAULT_NODE_COLOR}, and
  * {@link #DEFAULT_NODE_LINE_COLOR}, respectively. 
  * </p>
  * <p>
  * The default representation for an edge is provided by a {@link y.view.PolyLineEdgeRealizer} 
  * with a standard arrow used on its target side.
  * </p>
` */  
  public static final void configureDefaultRealizers(Graph2DView view) {
    NodeRealizer nr = new GenericNodeRealizer(NODE_CONFIGURATION);
    nr.setFillColor(DEFAULT_NODE_COLOR);
    nr.setLineColor(DEFAULT_NODE_LINE_COLOR);
    nr.setWidth(60);
    nr.setHeight(30);    
    view.getGraph2D().setDefaultNodeRealizer(nr);    
        
    // By default, edges show their direction using the standard arrowhead.
    PolyLineEdgeRealizer er = new PolyLineEdgeRealizer();
    er.setTargetArrow(Arrow.STANDARD);
    view.getGraph2D().setDefaultEdgeRealizer(er);  
  }

  /**
   * Applies NodeRealizer defaults to all nodes. Properties no applied are location and size 
   */
  public static void applyRealizerDefaults(Graph2D graph) {
    applyRealizerDefaults(graph, false, true);
  }

  /**
   * Applies NodeRealizer defaults to all nodes. Properties not applied are location, and
   * depending on the given arguments, size and fillColor.      
   */
  public static void applyRealizerDefaults(Graph2D graph, boolean applyDefaultSize, boolean applyFillColor) {
    for(NodeCursor nc = graph.nodes(); nc.ok(); nc.next()) {
      GenericNodeRealizer gnr = new GenericNodeRealizer(graph.getRealizer(nc.node()));
      gnr.setConfiguration(DemoDefaults.NODE_CONFIGURATION);
      if(applyFillColor) {
        gnr.setFillColor(graph.getDefaultNodeRealizer().getFillColor());
      }
      gnr.setLineColor(null);      
      if(applyDefaultSize) {
        gnr.setSize(graph.getDefaultNodeRealizer().getWidth(), graph.getDefaultNodeRealizer().getHeight());
      }
      graph.setRealizer(nc.node(), gnr);      
    }        
  }

  /**
   * Applies the given fill color to all nodes
   */
  public static void applyFillColor(Graph2D graph, Color color) {
    for (NodeCursor nc = graph.nodes(); nc.ok(); nc.next()) {
      Node n = nc.node();
      graph.getRealizer(n).setFillColor(color);
    } 
  }

  /**
   * Applies the given fill color to all nodes
   */

  public static void applyLineColor(Graph2D graph, Color color) {
    for (NodeCursor nc = graph.nodes(); nc.ok(); nc.next()) {
      Node n = nc.node();
      graph.getRealizer(n).setLineColor(color);
    } 
  }
}
