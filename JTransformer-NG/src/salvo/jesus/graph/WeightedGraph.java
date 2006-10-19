package salvo.jesus.graph;

import java.util.*;

/**
 * A interface for a Graph where all edges have a specified weight.
 *
 * @author		Jesus M. Salvo Jr.
 */

public interface WeightedGraph extends Graph {

  /**
   * Convenience method to add a WeightedEdge with a specified weight
   * into the WeightedGraph. The default addEdge( v1, v2 ) will add a
   * WeightedEdge with zero weight, after which you can call setWeight()
   * to specify the weight.
   *
   * @return  The WeightedEdge that has been added.
   */
  public WeightedEdge addEdge( Vertex v1, Vertex v2, double weight ) throws Exception;

  /**
   * Determines the Vertex that is 'closest' to the Vertex specified.
   * The definition of the closest vertex in this context is a
   * vertex that is directly adjacent to Vertex v where the edge
   * has the least weight.
   *
   * @return  The Vertex closes to Vertex v.
   */
  public Vertex getClosest( Vertex v );

  /**
   * Determine a minimum spanning tree for the weighted graph.
   * There is no guarantee that the same method call will result in
   * the same result, as long as it satisifies the property of
   * a minimum spanning tree.
   *
   * @return  Subgraph connecting all the Vertices such that the sum
   * of the weights of the Edges is at least as small as the sum of the weights of
   * any other collection of Edges connecting all the Vertices.
   */
  public WeightedGraph minimumSpanningTree();

  /**
   * Determine a shortest path spanning tree for the weighted graph.
   * Shortest path spanning tree need not be unique. Therefore, there is
   * no guarantee that calling this method twice for the same weighted
   * graph will return exactly the same shortest path spanning tree,
   * unless there is only one shortest path spanning tree.
   * <p>
   * Also note that the graph returned by this method is a new instance
   * of WeightedGraph. However, its vertices and edges will be the same instance
   * as those of this WeightedGraph. Therefore, <b>do not</b> modify the contents
   * of the returned <tt>WeightedGraph</tt> such that any of its vertices or edges
   * are removed.
   *
   * @param vertex  The Vertex in the weighted graph that we want to get
   * the shortest paths to all other vertices.
   * @return  Shortest spanning tree subgraph from the vertex
   * parameter to all other vertices that are in the same connected set
   * as the vertex.
   */
  public WeightedGraph shortestPath( Vertex vertex );
}