package salvo.jesus.graph;

/**
 * Represents a weighted edge in a graph.
 *
 * @author		Jesus M. Salvo Jr.
 */
public class WeightedEdgeImpl extends EdgeImpl implements WeightedEdge{
  /**
   * The weight of the edge. The implementation of the WeightedEdge interface
   * is delegated to this object.
   */
  WeightedEdgeWeakImpl  weightOfEdge;

  /**
    * Creates an WeightedEdgeImpl object.
    *
    * @see		Vertex
    */
  public WeightedEdgeImpl( Vertex a, Vertex b, double weight ) {
    super( a, b );
    this.weightOfEdge = new WeightedEdgeWeakImpl( weight );
  }

  /**
   * Returns the weight of the edge.
   */
  public double getWeight() {
    return this.weightOfEdge.getWeight();
  }

  /**
   * Sets the weight of the edge.
   *
   * @param   weight    The new weight of the edge
   */
  public void setWeight( double weight ) {
    this.weightOfEdge.setWeight( weight );
  }

  public String toString(){
    return this.labelDelegator.getLabel();
  }
}