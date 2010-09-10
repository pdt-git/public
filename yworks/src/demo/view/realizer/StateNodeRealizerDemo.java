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
package demo.view.realizer;

import java.awt.EventQueue;
import java.awt.event.MouseEvent;

import y.base.GraphEvent;
import y.base.GraphListener;
import y.base.Node;
import y.io.GraphMLIOHandler;
import y.view.Graph2D;
import y.view.NodeRealizer;
import y.view.ShapeNodeRealizer;
import y.view.ViewMode;
import demo.view.DemoBase;
import demo.view.DemoDefaults;

/**
 * This demo shows how the custom node realizer {@link StateNodeRealizer}
 * can be used within an application.
 * The demo allows to create nodes that have different state. 
 * Additionally it is possible to change the state of a node by right clicking
 * on it.
 * A graph with its custom node realizers can be saved and loaded using the GraphML
 * format.
 */
public class StateNodeRealizerDemo extends DemoBase
{

  public StateNodeRealizerDemo()
  {
    Graph2D graph = view.getGraph2D();

    view.addViewMode(new StateChangeViewMode());

    StateNodeRealizer svr = new StateNodeRealizer();
    svr.setSize(70,70);
    svr.setState(StateNodeRealizer.FINAL_STATE);
    svr.setFillColor(DemoDefaults.DEFAULT_NODE_COLOR);
    
    graph.setDefaultNodeRealizer(svr);

    //for each node that will be created use a reconfigured
    //default node realizer. 
    graph.addGraphListener(new GraphListener() {
      public void onGraphEvent(GraphEvent ev)
      {
        if(ev.getType() == GraphEvent.NODE_CREATION)
        {
          applyNextState(((Graph2D)ev.getGraph()).getDefaultNodeRealizer());
        }
      }
    });

    loadGraph( "resource/stateNodeRealizer.graphml" );
    DemoDefaults.applyFillColor(graph, DemoDefaults.DEFAULT_NODE_COLOR);
  }


  protected GraphMLIOHandler createGraphMLIOHandler() {
    GraphMLIOHandler ioHandler = super.createGraphMLIOHandler();
    ioHandler.addNodeRealizerSerializer(new StateNodeRealizer.StateNodeRealizerSerializer());
    return ioHandler;
  }

  /**
   * This method changes state and shape of a StateNodeRealizer.
   */
  private void applyNextState(NodeRealizer vr)
  {
    if(vr instanceof StateNodeRealizer)
    {
      StateNodeRealizer svr = (StateNodeRealizer)vr;
      switch(svr.getState()) {
        case StateNodeRealizer.INITIAL_STATE:
         svr.setState(StateNodeRealizer.TRANSITION_STATE);
         break;
        case StateNodeRealizer.TRANSITION_STATE:
         svr.setState(StateNodeRealizer.FINAL_STATE);
         break;
        case StateNodeRealizer.FINAL_STATE:
         svr.setState(StateNodeRealizer.INITIAL_STATE);
         break;
      }
      if(svr.getShapeType() == ShapeNodeRealizer.ELLIPSE)
      {
        svr.setShapeType(StateNodeRealizer.CUSTOM_SHAPE);
      }
      else
      {
        svr.setShapeType(ShapeNodeRealizer.ELLIPSE);
      }
    }
  }

  /**
   * ViewMode that changes state and shape of a node when it
   * gets right-clicked or double-clicked.
   */
  private class StateChangeViewMode extends ViewMode
  {

    public void mousePressedRight(double x, double y)
    {
      Node hitNode = getHitInfo(x,y).getHitNode();
      if(hitNode != null)
      {
        applyNextState(getGraph2D().getRealizer(hitNode));
        getGraph2D().updateViews();
      }
    }

    public void mouseClicked(MouseEvent ev)
    {
      if(ev.getClickCount() == 2)
      {
        double x = translateX(ev.getX());
        double y = translateY(ev.getY());
        Node hitNode = getHitInfo(x,y).getHitNode();
        if(hitNode != null)
        {
          applyNextState(getGraph2D().getRealizer(hitNode));
          getGraph2D().updateViews();
        }
      }
    }
  }


  public static void main(String[] args)
  {
    EventQueue.invokeLater(new Runnable() {
      public void run() {
        initLnF();
        (new StateNodeRealizerDemo()).start();
      }
    });
  }

}
