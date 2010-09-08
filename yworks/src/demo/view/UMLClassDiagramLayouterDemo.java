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

import demo.view.DemoBase;
import demo.view.DemoDefaults;

import y.base.DataProvider;
import y.base.Edge;
import y.base.EdgeCursor;
import y.base.Node;
import y.layout.LayoutOrientation;
import y.layout.Layouter;
import y.layout.PortConstraintKeys;
import y.layout.orthogonal.DirectedOrthogonalLayouter;
import y.layout.router.EdgeGroupRouterStage;
import y.layout.router.OrthogonalEdgeRouter;
import y.util.DataProviderAdapter;
import y.view.Arrow;
import y.view.BridgeCalculator;
import y.view.DefaultGraph2DRenderer;
import y.view.EdgeRealizer;
import y.view.Graph2D;
import y.view.LineType;
import y.view.NodeRealizer;
import y.view.PolyLineEdgeRealizer;

import javax.swing.AbstractAction;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JToolBar;
import javax.swing.ListCellRenderer;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

/**
 * <p>
 * This demo shows how {@link DirectedOrthogonalLayouter} and {@link OrthogonalEdgeRouter} can be used to
 * nicely layout UML Class Diagrams in an orthogonal layout style.
 * <p>
 * Usually, there are different kind of relationships between the classes of an UML diagram.
 * Some of the relationships are undirected (e.g. associations) while others are directed
 * (e.g. generalizations and realizations). This demo arranges a diagram in a way that
 * directed relationships point in a main layout direction(here bottom-to-top), while the
 * undirected relationships will be arranged without such a direction constraint.
 * Furthermore, all directed relationships of the same type sharing a common target node
 * will be routed in a bus-like style. For this special task
 * OrthogonalEdgeRouter will be used in combination with {@link EdgeGroupRouterStage}.
 * <p>
 * The toolbar of this demo offers four additional items:
 * <ol>
 * <li>A combobox that selects the type of relationship to be used: association (no arrow),
 * generalization (arrow and solid line), and realization (arrow and dashed line).
 * </li>
 * <li>Layout button - to layout the diagram</li>
 * <li>Layout From Sketch button - to layout the diagram, while obeying the layout of the given diagam</li>
 * <li>Route Edges button - to route all edges of the diagram, while preserving the coordinates of the nodes</li>
 * </ol>
 */
public class UMLClassDiagramLayouterDemo extends DemoBase {

  DirectedOrthogonalLayouter doLayouter;
  OrthogonalEdgeRouter oeRouter;
  Layouter layouter, router;

  public UMLClassDiagramLayouterDemo() {
    final Graph2D graph = view.getGraph2D();

    //configure default node realizer
    NodeRealizer nr = graph.getDefaultNodeRealizer();
    nr.setSize(80, 50);
    nr.setLabelText("<html><b>Class</b><br><hr>doit():void");
    nr.setFillColor(new Color(189, 185, 146));

    //activate grid mode
    view.setGridMode(true);
    view.setGridResolution(15);

    //activate bridge style painting of edge crossings
    DefaultGraph2DRenderer r = (DefaultGraph2DRenderer) view.getGraph2DRenderer();
    BridgeCalculator bc = new BridgeCalculator();
    bc.setCrossingMode(BridgeCalculator.CROSSING_MODE_VERTICAL_CROSSES_HORIZONTAL);
    r.setBridgeCalculator(bc);

    configureLayout();

    loadGraph(getClass(), "resource/classdiagram01.graphml");
    DemoDefaults.applyRealizerDefaults(graph, false, false);
  }


  /**
   * Configures layout algorithm and adds layout-specific data providers to the graph
   */
  private void configureLayout() {
    final Graph2D graph = view.getGraph2D();

    doLayouter = new DirectedOrthogonalLayouter();
    doLayouter.setLayoutOrientation(LayoutOrientation.BOTTOM_TO_TOP);
    doLayouter.setGrid(30);
    layouter = doLayouter;

    DataProvider directedDP = new DataProviderAdapter() {
      public boolean getBool(Object obj) {
        return graph.getRealizer((Edge) obj).getTargetArrow() != Arrow.NONE;
      }
    };
    graph.addDataProvider(DirectedOrthogonalLayouter.DIRECTED_EDGE_DPKEY, directedDP);

    DataProvider targetGroupDP = new DataProviderAdapter() {
      public Object get(Object obj) {
        EdgeRealizer er = graph.getRealizer((Edge) obj);
        if (er.getTargetArrow() == Arrow.NONE) {
          return null;
        } else {
          return er.getLineType();
        }
      }
    };
    graph.addDataProvider(PortConstraintKeys.TARGET_GROUPID_KEY, targetGroupDP);

    oeRouter = new OrthogonalEdgeRouter();
    oeRouter.setGridSpacing(10);
    oeRouter.setGridRoutingEnabled(true);
    oeRouter.setCrossingCost(2.0);
    oeRouter.setReroutingEnabled(true);
    router = new EdgeGroupRouterStage(oeRouter);
  }

  /**
   * Run a layout algorithm
   */
  private void runLayout(Layouter layouter) {
    Cursor oldCursor = view.getCanvasComponent().getCursor();
    try {
      view.getCanvasComponent().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
      view.applyLayoutAnimated(layouter);
    } finally {
      view.getCanvasComponent().setCursor(oldCursor);
    }
  }

  /**
   * Add a layout button and a combobox for edge realizer selection to the ToolBar
   */
  protected JToolBar createToolBar() {
    final JToolBar toolBar = super.createToolBar();

    toolBar.add(createEdgeRealizerComboBox());

    toolBar.addSeparator();
    toolBar.add(new AbstractAction("Layout") {
      public void actionPerformed(ActionEvent e) {
        doLayouter.setUseSketchDrawing(false);
        runLayout(doLayouter);
      }
    });
    toolBar.addSeparator();
    toolBar.add(new AbstractAction("Layout From Sketch") {
      public void actionPerformed(ActionEvent e) {
        doLayouter.setUseSketchDrawing(true);
        runLayout(doLayouter);
      }
    });
    toolBar.addSeparator();
    toolBar.add(new AbstractAction("Route Edges") {
      public void actionPerformed(ActionEvent e) {
        runLayout(router);
      }
    });

    return toolBar;
  }

  JComboBox createEdgeRealizerComboBox() {
    final EdgeRealizer association = new PolyLineEdgeRealizer();
    final EdgeRealizer generalization = new PolyLineEdgeRealizer();
    generalization.setTargetArrow(Arrow.WHITE_DELTA);
    generalization.setLineType(LineType.LINE_2);
    generalization.setLineColor(new Color(51, 51, 153));
    final EdgeRealizer realization = new PolyLineEdgeRealizer();
    realization.setReversedPathRenderingEnabled(true);
    realization.setTargetArrow(Arrow.WHITE_DELTA);
    realization.setLineType(LineType.DASHED_2);
    realization.setLineColor(new Color(51, 51, 153));

    final Object[] items = {
        association,
        generalization,
        realization
    };

    final JComboBox box = new JComboBox(items);
    box.setRenderer(new EdgeRealizerCellRenderer());
    box.setMaximumSize(new Dimension(box.getMinimumSize().width, box.getMaximumSize().height));
    box.addItemListener(new ItemListener() {
      public void itemStateChanged(ItemEvent ev) {
        if (ev.getStateChange() == ItemEvent.SELECTED) {
          EdgeRealizer r = (EdgeRealizer) box.getSelectedItem();
          if (r != null) {
            for (EdgeCursor ec = view.getGraph2D().selectedEdges(); ec.ok(); ec.next()) {
              EdgeRealizer ser = view.getGraph2D().getRealizer(ec.edge());
              ser.setLineColor(r.getLineColor());
              ser.setTargetArrow(r.getTargetArrow());
              ser.setLineType(r.getLineType());
            }
            view.getGraph2D().setDefaultEdgeRealizer(r.createCopy());
          }
        }
      }
    });
    box.setSelectedIndex(0);

    return box;
  }

  static class EdgeRealizerCellRenderer extends JComponent implements ListCellRenderer {
    private Graph2D graph;
    private EdgeRealizer er;

    public EdgeRealizerCellRenderer() {
      graph = new Graph2D();
      Node s = graph.createNode(0, 12, 1, 1, "");
      Node t = graph.createNode(60, 12, 1, 1, "");
      graph.createEdge(s, t);
    }

    public Component getListCellRendererComponent(
        JList list,
        Object value,
        int index,
        boolean isSelected,
        boolean cellHasFocus) {

      setPreferredSize(new Dimension(60, 24));

      er = (EdgeRealizer) value;
      graph.setRealizer(graph.firstEdge(), er);
      return this;
    }

    public void paint(Graphics g) {
      Graphics2D gfx = (Graphics2D) g;
      er.paint(gfx);
    }
  }

  public static void main(String[] args) {
    EventQueue.invokeLater(new Runnable() {
      public void run() {
        initLnF();
        (new UMLClassDiagramLayouterDemo()).start();
      }
    });
  }
}
