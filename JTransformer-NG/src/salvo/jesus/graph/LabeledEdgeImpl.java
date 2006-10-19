package salvo.jesus.graph;

/**
 * An implementation of LabeledEdge that dynamically derives the label
 * for an <tt>Edge</tt> based on the label of the <tt>Edge's</tt> vertices.
 * <p>
 * <i>
 * To do:<br>
 * Notification mechanism so that an Edge is informed about a Vertex's label state change,
 * so that we do not have to recreate the label all the time.
 * </i>
 *
 * @author Jesus M. Salvo Jr.
 * @version $Id$
 */

public class LabeledEdgeImpl extends LabeledGraphComponentImpl implements LabeledEdge {

  private boolean followVertexLabel = true;
  private Edge    edgeBeingLabelled;

  public LabeledEdgeImpl( Edge edge ) {
    this.edgeBeingLabelled = edge;
  }

  public boolean isFollowVertexLabel() {
    return this.followVertexLabel;
  }

  public void setFollowVertexLabel(boolean isFollow) {
    this.followVertexLabel = isFollow;
  }

  /**
   * If <tt>getFollowVertexLabel()</tt> returns true,
   * each call to this method derives the label for the <tt>Edge</tt> for whic
   * the label is for. Improvements can be made so that a <tt>LabeledEdge</tt>,
   * <tt>GraphComponentLabel</tt> or the <tt>Edge</tt> itself is notified
   * whenever the <tt>Vertex</tt>s of the <tt>Edge</tt> are changed.
   * <p>
   * For the moment, deriving it for each call will suffice.
   */
  public String getLabel() {
    if( this.followVertexLabel ) {
      String strA;
      String strB;

      if( this.edgeBeingLabelled instanceof DirectedEdge ) {
        DirectedEdge dEdge = (DirectedEdge) this.edgeBeingLabelled;
        strA = dEdge.getSource().getLabel();
        strB = dEdge.getSink().getLabel();
      }
      else {
        strA = this.edgeBeingLabelled.getVertexA().getLabel();
        strB = this.edgeBeingLabelled.getVertexB().getLabel();
      }

      StringBuffer tmpString = new StringBuffer();
      tmpString.append( (strA == null) ? "<Null vertex>" : strA );
      tmpString.append( "-" );

      if( this.edgeBeingLabelled instanceof DirectedEdge )
        tmpString.append( ">" );

      tmpString.append( (strB == null) ? "<Null vertex>" : strB );

      if( this.edgeBeingLabelled instanceof WeightedEdge ) {
        WeightedEdge wEdge = (WeightedEdge) this.edgeBeingLabelled;
        tmpString.append( " (" + wEdge.getWeight() +")" );
      }
      return tmpString.toString();
    }
    else {
      return super.getLabel();
    }
  }


  public void setLabel(String label)
  {
    if( this.followVertexLabel )
      throw new IllegalStateException("edge label is dynamically derived from vertices' label");
    else
      super.setLabel( label );
  }

}