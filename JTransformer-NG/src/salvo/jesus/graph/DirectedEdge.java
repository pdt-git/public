package salvo.jesus.graph;

/**
 * Represents a directed edge in a graph.
 *
 * @author		Jesus M. Salvo Jr.
 */
public interface DirectedEdge extends Edge {

  static final int  DIRECTION_A_TO_B = 1;
  static final int  DIRECTION_B_TO_A = -1;
  static final int  NODIRECTION = 0;

  /**
   * Returns the source Vertex of the edge.
   *
   * @return  The source Vertex.
   */
  public Vertex getSource();

  /**
   * Returns the sink Vertex of the edge.
   *
   * @return  The sink Vertex.
   */
  public Vertex getSink();

  /**
   * Returns the direction of the Edge
   *
   */
  public int getDirection();
  
}

