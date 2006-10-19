package salvo.jesus.graph;

/**
 * A weak implementation a directed edge in a graph. This is only used internally
 * to implement the direction of an edge without having an actual edge.
 *
 * @author		Jesus M. Salvo Jr.
 */
class DirectedEdgeWeakImpl implements DirectedEdge {

  /**
   * The source Vertex of the Edge
   */
  private Vertex  sourceVertex;

  /**
   * The sink Vertex of the Edge
   */
  private Vertex  sinkVertex;

  /**
   * The direction of the edge.
   */
  int direction;

  /**
    * Creates an DirectedEdgeWeakImpl object whose origin and destination vertices
    * are specified by the method parameters.
    *
    * @see		Vertex
    */
  public DirectedEdgeWeakImpl( Vertex sourceVertex, Vertex sinkVertex ){
    this.sourceVertex = sourceVertex;
    this.sinkVertex = sinkVertex;
    this.direction = DirectedEdge.DIRECTION_A_TO_B;
  }

  /**
   * Returns the source Vertex of the edge.
   *
   * @return  The source Vertex.
   */
  public Vertex getSource() {
    return this.sourceVertex;
  }

  /**
   * Returns the sink Vertex of the edge.
   *
   * @return  The sink Vertex.
   */
  public Vertex getSink() {
    return this.sinkVertex;
  }

  /**
   * Returns the direction of the Edge
   *
   */
  public int getDirection() {
    return this.direction;
  }

  /**
   * Empty method implemetation that returns null. This method should never
   * be called or delegated to for whatever reason.
   */
  public Vertex getVertexA() { return null; }

  /**
   * Empty method implemetation that returns null. This method should never
   * be called or delegated to for whatever reason.
   */
  public Vertex getVertexB() { return null; }

  /**
   * Empty method implemetation does noething. This method should never
   * be called or delegated to for whatever reason.
   */
  public void setObject( Object obj ) {}

  /**
   * Empty method implemetation that returns null. This method should never
   * be called or delegated to for whatever reason.
   */
  public Object getObject() { return null; }

  /**
   * Empty method implemetation that returns null. This method should never
   * be called or delegated to for whatever reason.
   */
  public Vertex getOppositeVertex( Vertex vertex ) { return null; }

  /**
   * Empty method implemetation does nothing. This method should never
   * be called or delegated to for whatever reason.
   */
  public boolean hasLabel() { return false; }

  /**
   * Empty method implemetation does nothing. This method should never
   * be called or delegated to for whatever reason.
   */
  public String getLabel() { return null; }

  /**
   * Empty method implemetation does nothing. This method should never
   * be called or delegated to for whatever reason.
   */
  public void setLabel(String label) {}

  /**
   * Empty method implemetation does nothing. This method should never
   * be called or delegated to for whatever reason.
   */
  public boolean isFollowVertexLabel() { return false; }

  /**
   * Empty method implemetation does nothing. This method should never
   * be called or delegated to for whatever reason.
   */
  public void setFollowVertexLabel( boolean isFollow ) {}

}