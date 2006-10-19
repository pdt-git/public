package salvo.jesus.graph;

/**
 * Represents an undirected edge in a graph.
 * <p>
 * To do:<br>
 * Expose the <tt>LabeledEdge</tt> implementation so that
 * <tt>setFollowVertexLabel()</tt> can be called to turn it on or off.
 *
 * @author		Jesus M. Salvo Jr.
 * @version             $Id$
 */
public class EdgeImpl implements Edge {

  /**
   * Delegator to handle label methods defined in <tt>LabeledGraphComponent</tt> interface
   */
  LabeledEdgeImpl   labelDelegator;

  /**
   * The A vertex of the edge.
   */
  protected Vertex  vertexA;

  /**
   * The B vertex of the edge.
   */
  protected Vertex  vertexB;

  /**
   * The string returned when toString() is called.
   */
  protected String    str;

  /**
    * Creates an UndirectedEdge object.
    *
    * @see		Vertex
    */
  public EdgeImpl( Vertex a, Vertex b ) {
    this.vertexA = a;
    this.vertexB = b;

    this.labelDelegator = new LabeledEdgeImpl( this );
  }

  /**
   * Returns the endpoint A of the edge.
   *
   * @return  Vertex  Endpoint A of the edge.
   */
  public Vertex getVertexA() {
    return this.vertexA;
  }

  /**
   * Returns the endpoint B of the edge.
   *
   * @return  Vertex  Endpoint B of the edge.
   */
  public Vertex getVertexB() {
    return this.vertexB;
  }

  /**
   * Returns the Vertex opposite to the specified Vertex in the edge.
   *
   * @return  Vertex  The Vertex object that is the opposite to the specifid
   *                  Vertex. If the specified Vertex is not an endpoint of the
   *                  edge, returns null.
   */
  public Vertex getOppositeVertex( Vertex v ) {
    if( this.vertexA == v )
      return this.vertexB;
    else if( this.vertexB == v )
      return this.vertexA;
    else
      return null;
  }

  public boolean hasLabel() {
    return this.labelDelegator.hasLabel();
  }

  public String getLabel() {
    return this.labelDelegator.getLabel();
  }

  public void setLabel( String label ) {
    this.labelDelegator.setLabel( label );
  }

  public boolean isFollowVertexLabel() {
    return this.labelDelegator.isFollowVertexLabel();
  }

  public void setFollowVertexLabel( boolean isFollow ) {
    this.labelDelegator.setFollowVertexLabel( isFollow );
  }

  /**
    * Returns a String representation of the Edge.
    * By default, the format is:
    * fromVertex.toString() + "->" + toVertex.toString()
    *
    * @return	The String representation of the Edge
    * @see		Vertex
    */
  public String toString(){
    return this.labelDelegator.getLabel();
  }

  /**
    * Creates a clone of this Edge. This calls the EdgeImpl constructor,
    * thereby creating a new instance of EdgeImpl. However, the vertices
    * in both endpoints of the Edge are not cloned.
    *
    * @return  A clone of an instance of Edge.
    */
  protected Object clone(){
    return new EdgeImpl( vertexA, vertexB );
  }

}

