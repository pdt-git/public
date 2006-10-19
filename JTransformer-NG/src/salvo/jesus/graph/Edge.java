package salvo.jesus.graph;

import java.io.*;

/**
 * An interface for edges in a Graph.
 *
 * @author		Jesus M. Salvo Jr.
 */
public interface Edge extends LabeledEdge {
  /**
   * Returns the endpoint A of the edge.
   *
   * @return  Vertex  Endpoint A of the edge.
   */
  public Vertex getVertexA();

  /**
   * Returns the endpoint B of the edge.
   *
   * @return  Vertex  Endpoint B of the edge.
   */
  public Vertex getVertexB();

  /**
   * Returns the Vertex opposite to the specified Vertex in the edge.
   *
   * @return  Vertex  The Vertex object that is the opposite to the specifid
   *                  Vertex. If the specified Vertex is not an endpoint of the
   *                  edge, returns null.
   */
  public Vertex getOppositeVertex( Vertex v );

}

