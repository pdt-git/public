package salvo.jesus.graph.algorithm;

import salvo.jesus.graph.*;
import java.util.*;
import java.io.*;

/**
 * Abstract class for an algorithm implementing the minimum spanning tree.
 * Classes implementing the WeightedGraph interface uses the Strategy
 * pattern to allow different implementations of the minimum spanning tree
 * algorithm to be used.
 *
 * Concrete implementations of this class must never modify WeightedGraph.
 *
 * @author  Jesus M. Salvo Jr.
 */

public abstract class MinimumSpanningTreeAlgorithm implements Serializable {

  /**
   * The WeightedGraph object that the algorithm uses to determine
   * the minimum spanning tree.
   */
  WeightedGraph   wgraph;

  public MinimumSpanningTreeAlgorithm( WeightedGraph wgraph ) {
    this.wgraph = wgraph;
  }

  /**
   * Abstract method to be implemented by subclasses.
   */
  public abstract WeightedGraph minimumSpanningTree();
}