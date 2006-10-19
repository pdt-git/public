package salvo.jesus.graph;

import java.util.*;
import salvo.jesus.graph.algorithm.*;

/**
 * The DirectedAcyclicGraph class represents a directed acyclic graph (DAG)
 * where there is no cyclic paths for any of its vertices.
 *
 * A cylic path is a path from a vertex back to itself by following the
 * direction of the edges.
 *
 * @author		Jesus M. Salvo Jr.
 */

public interface DirectedAcyclicGraph extends DirectedGraph {

  /**
    * Returns a List of vertices that is not depended on by other vertices.
    * That is, a List of vertices where there are no Edges pointing to it.
    *
    * @return	List of vertices
    */
  public List getRoot( );

  /**
   * @see TopologicalSorting#traverse()
   */
  public List topologicalSort( );

  /**
   * @see TopologicalSorting#reverseTraverse()
   */
  public List reverseTopologicalSort( );

  /**
   * @see TopologicalSorting#traverse(Vertex)
   */
  public List topologicalSort( Vertex startat );

  /**
   * @see TopologicalSorting#reverseTraverse(Vertex)
   */
  public List reverseTopologicalSort( Vertex startat );
}
