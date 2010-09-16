package demo.view.hierarchic;


import y.base.Node;
import y.geom.YRectangle;
import y.layout.LayoutGraph;
import y.layout.NodeLabelLayout;
import y.layout.NodeLayout;
import y.layout.hierarchic.incremental.DrawingDistanceCalculator;
import y.layout.hierarchic.incremental.Layer;
import y.layout.hierarchic.incremental.Layers;
import y.layout.hierarchic.incremental.LayoutDataProvider;
import y.layout.hierarchic.incremental.NodeData;


/**
 * <code>DrawingDistanceCalculator</code> that ensures a minimum group node
 * width of <code>max label x - min label x</code>.
 *
 * @author Thomas Behr
 */
public class LabelAwareDrawingDistanceCalculator
        implements DrawingDistanceCalculator {
  private static final double GROUP_LABEL_WIDTH_ADJUSTMENT = 20;


  private final DrawingDistanceCalculator wrappedCalculator;
  private double groupLabelWidthAdjustment;

  public LabelAwareDrawingDistanceCalculator(
          final DrawingDistanceCalculator wrappedCalculator
  ) {
    this.wrappedCalculator = wrappedCalculator;
    this.groupLabelWidthAdjustment = GROUP_LABEL_WIDTH_ADJUSTMENT;
  }


  /**
   * Returns the group label width adjustment.
   * @return the group label width adjustment.
   */
  public double getGroupLabelWidthAdjustment() {
    return groupLabelWidthAdjustment;
  }

  /**
   * Specifies the group label width adjustment.
   * The group label width adjustment is added to label widths to be able to
   * take the special group state switch label of yFiles default
   * <code>GroupNodeRealizer</code> into account.
   * @param adjustment   the group label width adjustment.
   */
  public void setGroupLabelWidthAdjustment( final double adjustment ) {
    this.groupLabelWidthAdjustment = adjustment;
  }


  public void initialize(
          LayoutGraph graph, Layers layers, LayoutDataProvider ldp
  ) {
    if (wrappedCalculator != null) {
      wrappedCalculator.initialize(graph, layers, ldp);
    }
  }

  public double getMinDistance(
          LayoutGraph graph,
          Layer layer,
          LayoutDataProvider ldp,
          Node left,
          Node right
  ) {
    if (left != null && right != null &&
        ldp.getNodeData(left).getType() == NodeData.TYPE_GROUP_BEGIN &&
        ldp.getNodeData(right).getType() == NodeData.TYPE_GROUP_END) {
      final Node groupNode = ldp.getNodeData(left).getAssociatedNode();

      final NodeLayout nl = graph.getNodeLayout(groupNode);
      double minX = nl.getX();
      double maxX = nl.getX() + nl.getWidth();

      final NodeLabelLayout[] labelLayouts = graph.getNodeLabelLayout(groupNode);
      for (int i = 0; i < labelLayouts.length; ++i) {
        final YRectangle lbx = labelLayouts[i].getBox();
        if (minX > lbx.x) {
          minX = lbx.x;
        }
        if (maxX < lbx.x + lbx.width + groupLabelWidthAdjustment) {
          maxX = lbx.x + lbx.width + groupLabelWidthAdjustment;
        }
      }

      return maxX - minX;
    } else if (wrappedCalculator != null) {
      return wrappedCalculator.getMinDistance(graph, layer, ldp, left, right);
    } else {
      return 0;
    }
  }

  public void dispose(
          LayoutGraph graph, Layers layers, LayoutDataProvider ldp
  ) {
    if (wrappedCalculator != null) {
      wrappedCalculator.dispose(graph, layers, ldp);
    }
  }
}
