package salvo.jesus.graph.algorithm;

import salvo.jesus.graph.*;
import java.util.*;
import java.io.*;

/**
 * Abstract class for implementing the shortest path algorithm.
 * A shortest path spanning tree is a subgraph of the original
 * weighted graph showing how to reach all other vertices from
 * a given vertex in the same connected set in the shortest possible way.
 * The shortest path between two vertices should be such that the sum
 * of the weights of all the edges between the two vertices be at a minimum.
 * Note that, like minimum spanning trees, there may be more than one
 * shortest spanning tree for a single weighted graph.
 *
 * Concrete subclasses must never modify the weighted graph where
 * it is computing the shortestpath.
 *
 * @author  Jesus M. Salvo Jr.
 */

public abstract class ShortestPathAlgorithm implements Serializable {

  /**
   * The WeightedGraph object that the algorithm uses to determine
   * the shortest path spanning tree.
   */
  WeightedGraph   wgraph;

  public ShortestPathAlgorithm( WeightedGraph wgraph ) {
    this.wgraph = wgraph;
  }

  /**
   * Abstract method to be implemented by subclasses to determine
   * a shortest path spanning tree from a given vertex in the form
   * of a graph.
   *
   * @return  A new WeightedGraph that represents the shortest path spanning
   * tree of the original WeightedGraph. <b>Do not</b> modify the contents
   * of the returned WeightedGraph.
   */
  public abstract WeightedGraph shortestPath( Vertex from );

}