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

import y.base.Edge;
import y.base.EdgeCursor;
import y.layout.LabelLayoutConstants;
import y.layout.LabelRanking;
import y.layout.LayoutStage;
import y.layout.Layouter;
import y.layout.grouping.FixedGroupLayoutStage;
import y.layout.labeling.GreedyMISLabeling;
import y.layout.orthogonal.OrthogonalGroupLayouter;
import y.layout.orthogonal.OrthogonalLayouter;
import y.module.LayoutModule;
import y.option.ConstraintManager;
import y.option.OptionHandler;
import y.view.EdgeLabel;
import y.view.EdgeRealizer;
import y.view.Graph2D;
import y.view.hierarchy.HierarchyManager;

/**
 * This module represents an interactive configurator and launcher for
 * {@link y.layout.orthogonal.OrthogonalLayouter}
 * and {@link y.layout.orthogonal.OrthogonalGroupLayouter} respectively.
 *
 */
public class OrthogonalLayoutModule extends LayoutModule
{
  private static final String ORTHOGONAL = "ORTHOGONAL_LAYOUTER";
  private static final String GROUPING      = "GROUPING";
  private static final String GROUP_POLICY  = "GROUP_LAYOUT_POLICY";
  private static final String IGNORE_GROUPS = "IGNORE_GROUPS";
  private static final String LAYOUT_GROUPS = "LAYOUT_GROUPS";
  private static final String FIX_GROUPS    = "FIX_GROUPS";
  private static final String GROUP_LAYOUT_QUALITY = "GROUP_LAYOUT_QUALITY";

  private static final String LENGTH_REDUCTION = "LENGTH_REDUCTION";
  private static final String STYLE = "STYLE";
  private static final String USE_RANDOMIZATION = "USE_RANDOMIZATION";
  private static final String USE_FACE_MAXIMIZATION = "USE_FACE_MAXIMIZATION";
  private static final String USE_EXISTING_DRAWING_AS_SKETCH = "USE_EXISTING_DRAWING_AS_SKETCH";
  private static final String CROSSING_POSTPROCESSING = "CROSSING_POSTPROCESSING";
  private static final String PERCEIVED_BENDS_POSTPROCESSING = "PERCEIVED_BENDS_POSTPROCESSING";
  private static final String GRID = "GRID";
  private static final String NORMAL = "NORMAL";
  private static final String NORMAL_TREE = "NORMAL_TREE";
  private static final String UNIFORM_NODES = "UNIFORM_NODES";
  private static final String BOX_NODES = "BOX_NODES";
  private static final String MIXED = "MIXED";
  private static final String FIXED_MIXED = "FIXED_MIXED";
  private static final String FIXED_BOX_NODES = "FIXED_BOX_NODES";

  private static final String LAYOUT = "LAYOUT";
  private static final String EDGE_LABEL_MODEL = "EDGE_LABEL_MODEL";
  private static final String EDGE_LABELING = "EDGE_LABELING";
  private static final String LABELING = "LABELING";
  private static final String GENERIC = "GENERIC";
  private static final String NONE = "NONE";
  private static final String INTEGRATED = "INTEGRATED";
  private static final String FREE = "FREE";
  private static final String SIDE_SLIDER = "SIDE_SLIDER";
  private static final String CENTER_SLIDER = "CENTER_SLIDER";
  private static final String AS_IS = "AS_IS";
  private static final String BEST = "BEST";
  private static final String CONSIDER_NODE_LABELS = "CONSIDER_NODE_LABELS";

  private final String[] styleEnum = {NORMAL, NORMAL_TREE, UNIFORM_NODES, BOX_NODES, MIXED, FIXED_MIXED, FIXED_BOX_NODES};

  private static final String[] edgeLabeling = {
    NONE,
    INTEGRATED,
    GENERIC
  };

  private static final String[] edgeLabelModel = {
    BEST,
    AS_IS,
    CENTER_SLIDER,
    SIDE_SLIDER,
    FREE,
  };

  public OrthogonalLayoutModule()
  {
    super (ORTHOGONAL,"yFiles Layout Team",
           "Orthogonal Layouter");
    setPortIntersectionCalculatorEnabled(true);
  }

  public OptionHandler createOptionHandler()
  {
    OptionHandler op = new OptionHandler(getModuleName());
    
    ConstraintManager cm = new ConstraintManager( op );
 
    
    op.useSection(LAYOUT);
    op.addEnum(STYLE,styleEnum,0);
    op.addInt(GRID,25);
    op.addBool(LENGTH_REDUCTION, true);
    op.addBool(USE_EXISTING_DRAWING_AS_SKETCH, false);
    op.addBool(CROSSING_POSTPROCESSING, true);
    op.addBool(PERCEIVED_BENDS_POSTPROCESSING, true);
    op.addBool(USE_RANDOMIZATION, true);
    op.addBool(USE_FACE_MAXIMIZATION,false);
    op.useSection(LABELING);
    op.addEnum(EDGE_LABELING, edgeLabeling, 0);
    op.addEnum(EDGE_LABEL_MODEL, edgeLabelModel, 0);
    op.addBool(CONSIDER_NODE_LABELS, false);
    
    cm.setEnabledOnValueEquals( EDGE_LABELING, NONE, EDGE_LABEL_MODEL, true);    

    cm.setEnabledOnValueEquals( USE_EXISTING_DRAWING_AS_SKETCH, Boolean.FALSE, CROSSING_POSTPROCESSING);
    cm.setEnabledOnValueEquals( USE_EXISTING_DRAWING_AS_SKETCH, Boolean.FALSE, PERCEIVED_BENDS_POSTPROCESSING);
    cm.setEnabledOnValueEquals( USE_EXISTING_DRAWING_AS_SKETCH, Boolean.FALSE, STYLE);
    cm.setEnabledOnValueEquals( USE_EXISTING_DRAWING_AS_SKETCH, Boolean.FALSE, USE_RANDOMIZATION);

    
    op.useSection(GROUPING);
    String[] gEnum = { LAYOUT_GROUPS, FIX_GROUPS, IGNORE_GROUPS };
    op.addEnum(GROUP_POLICY, gEnum, 0);
    op.addDouble(GROUP_LAYOUT_QUALITY, 1.0, 0.0, 1.0);

    cm.setEnabledOnValueEquals( GROUP_POLICY, LAYOUT_GROUPS, GROUP_LAYOUT_QUALITY);
    
    return op;
  }

  public void mainrun()
  {
    OptionHandler op = getOptionHandler();
    
    OrthogonalLayouter orthogonal = new OrthogonalLayouter();
    
    ////////////////////////////////////////////////////////////////////////////
    // Layout
    ////////////////////////////////////////////////////////////////////////////

    switch (OptionHandler.getIndex(styleEnum, op.getString(STYLE))){
      default:
      case 0:
        orthogonal.setLayoutStyle(OrthogonalLayouter.NORMAL_STYLE);
        break;
      case 1:
        orthogonal.setLayoutStyle(OrthogonalLayouter.NORMAL_TREE_STYLE);
        break;
      case 2:
        orthogonal.setLayoutStyle(OrthogonalLayouter.UNIFORM_STYLE);
        break;
      case 3:
        orthogonal.setLayoutStyle(OrthogonalLayouter.BOX_STYLE);
        break;
      case 4:
        orthogonal.setLayoutStyle(OrthogonalLayouter.MIXED_STYLE);
        break;
      case 5:
        orthogonal.setLayoutStyle(OrthogonalLayouter.FIXED_MIXED_STYLE);
        break;
      case 6:
        orthogonal.setLayoutStyle(OrthogonalLayouter.FIXED_BOX_STYLE);
        break;
    }
    orthogonal.setGrid(op.getInt(GRID));
    orthogonal.setUseLengthReduction(
      op.getBool(LENGTH_REDUCTION));
    orthogonal.setUseCrossingPostprocessing(
      op.getBool(CROSSING_POSTPROCESSING));
    orthogonal.setPerceivedBendsOptimizationEnabled(
      op.getBool(PERCEIVED_BENDS_POSTPROCESSING));
    orthogonal.setUseRandomization(
      op.getBool(USE_RANDOMIZATION));
    orthogonal.setUseFaceMaximization(
  	  op.getBool(USE_FACE_MAXIMIZATION));
    orthogonal.setUseSketchDrawing(op.getBool(USE_EXISTING_DRAWING_AS_SKETCH));   


    ////////////////////////////////////////////////////////////////////////////
    // Labels
    ////////////////////////////////////////////////////////////////////////////

    Graph2D graph = getGraph2D();
    boolean normalStyle = (orthogonal.getLayoutStyle() == OrthogonalLayouter.NORMAL_STYLE);
    String el = op.getString(EDGE_LABELING);
    orthogonal.setIntegratedEdgeLabelingEnabled(el.equals(INTEGRATED) && normalStyle);
    orthogonal.setConsiderNodeLabelsEnabled(op.getBool(CONSIDER_NODE_LABELS) && normalStyle);
    if (el.equals(GENERIC) || (el.equals(INTEGRATED) && normalStyle)) {
      setupEdgeLabelModel(el, op.getString(EDGE_LABEL_MODEL));               
    } else if (!op.getBool(CONSIDER_NODE_LABELS) || !normalStyle) {
      orthogonal.setLabelLayouterEnabled(false);
    }    

    Layouter layouter = orthogonal;
    LayoutStage preStage = null;
    
    if(HierarchyManager.containsGroupNodes(graph) && !op.get(GROUP_POLICY).equals(IGNORE_GROUPS))
    {
      FixedGroupLayoutStage fgl = null;
      if(op.get(GROUP_POLICY).equals(FIX_GROUPS))
      {
        fgl = new FixedGroupLayoutStage();
        fgl.setInterEdgeRoutingStyle(FixedGroupLayoutStage.ROUTING_STYLE_ORTHOGONAL);
        orthogonal.prependStage(fgl);
        preStage = fgl;
      } else {
        OrthogonalGroupLayouter ogl = new OrthogonalGroupLayouter();
        ogl.setIntegratedEdgeLabelingEnabled(orthogonal.isIntegratedEdgeLabelingEnabled());
        ogl.setConsiderNodeLabelsEnabled(orthogonal.isConsiderNodeLabelsEnabled());
        ogl.setLabelLayouterEnabled(orthogonal.isLabelLayouterEnabled());        
            
        ogl.setGrid(op.getInt(GRID));
        ogl.setLayoutQuality(op.getDouble(GROUP_LAYOUT_QUALITY));
        layouter = ogl;
      }

      try {
        launchLayouter(layouter);
      } finally {
        if(preStage != null) {
          orthogonal.removeStage(preStage);
        }
      }
    }
    else
    {
      launchLayouter(layouter);
    }
    if (el.equals(GENERIC)) {
      GreedyMISLabeling la = new GreedyMISLabeling();
      la.setPlaceNodeLabels(false);
      la.setPlaceEdgeLabels(true);
      la.setProfitModel(new LabelRanking());
      la.doLayout(graph);
    }
    getGraph2D().updateViews();
  }

  void setupEdgeLabelModel(String edgeLabeling, String edgeLabelModel) {
    if(edgeLabeling.equals(NONE) || edgeLabelModel.equals(AS_IS)) {
      return; //nothing to do
    }
    
    byte model = EdgeLabel.SIDE_SLIDER;
    if (edgeLabelModel.equals(CENTER_SLIDER)) {
      model = EdgeLabel.CENTER_SLIDER;
    } else if (edgeLabelModel.equals(FREE) || edgeLabelModel.equals(BEST)) {
      model = EdgeLabel.FREE;
    }
//    else if (edgeLabelModel.equals(AUTO_ROTATION_ORTHOGONAL_SLIDER)) {
//      model = EdgeLabel.AUTO_ROTATED_ORTHOGONAL_SLIDER;
//    }

    Graph2D graph = getGraph2D();
    for (EdgeCursor ec = graph.edges(); ec.ok(); ec.next()) {
      Edge e = ec.edge();
      EdgeRealizer er = graph.getRealizer(e);
      for (int i = 0; i < er.labelCount(); i++) {
        EdgeLabel el = er.getLabel(i);
        el.setModel(model);
//        el.setRotationAngle(0);
        int prefAlongEdge = el.getPreferredPlacement() & LabelLayoutConstants.PLACEMENT_ALONG_EDGE_MASK;
        int prefOnSide = el.getPreferredPlacement() & LabelLayoutConstants.PLACEMENT_ON_SIDE_OF_EDGE_MASK;
        if (model == EdgeLabel.CENTER_SLIDER && prefOnSide != LabelLayoutConstants.PLACE_ON_EDGE) {
          el.setPreferredPlacement((byte) (LabelLayoutConstants.PLACE_ON_EDGE | prefAlongEdge));
        } else if(model == EdgeLabel.SIDE_SLIDER && prefOnSide == LabelLayoutConstants.PLACE_ON_EDGE) {
          el.setPreferredPlacement((byte) (LabelLayoutConstants.PLACE_RIGHT_OF_EDGE | prefAlongEdge));
        }
      }
    }
  }
}