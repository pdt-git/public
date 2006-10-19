package salvo.jesus.graph;

/**
 * An extension of the <tt>LabeledGraphComponent</tt> interface specific for an <tt>Edge</tt>.
 * Specifically, thie interface defines methods on whether or not edges labels
 * take their labels from the vertices.
 *
 * @author Jesus M. Salvo Jr.
 * @version $Id$
 */

public interface LabeledEdge extends LabeledGraphComponent {

  /**
   * @return whether or not it follows the labels of its vertices, including
   * any changes, to make sure the label for the edge is consistent with the
   * label of the vertex.
   */
  public boolean isFollowVertexLabel();

  /**
   * Specifies if the edge should follow the labels of its vertices.
   */
  public void setFollowVertexLabel( boolean isFollow );
}