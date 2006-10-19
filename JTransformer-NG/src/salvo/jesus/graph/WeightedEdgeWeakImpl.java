package salvo.jesus.graph;

/**
 * A weak implementation a weighted edge in a graph. This is only used internally
 * to implement the weight of an edge without having an actual edge.
 *
 * @author		Jesus M. Salvo Jr.
 */
class WeightedEdgeWeakImpl implements WeightedEdge {

  /**
   * The weight of the edge
   */
  double  weight;

  /**
    * Creates an WeightedEdgeWeakImpl object. Note that constructor
    * does not have vertices as parameters, since the sole purpose
    * of this class is to implement weight without having an actual edge.
    *
    * @see		Vertex
    */
  public WeightedEdgeWeakImpl( double weight ) {
    this.weight = weight;
  }

  /**
   * Sets the weight
   */
  public void setWeight( double weight ) {
    this.weight = weight;
  }

  /**
   * Returns the weight
   */
  public double getWeight() {
    return this.weight;
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