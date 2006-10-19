package salvo.jesus.graph;

import java.util.*;
import salvo.jesus.graph.algorithm.*;
import salvo.jesus.graph.listener.*;

/**
 * The DirectedAcyclicGraph class represents a directed acyclic graph (DAG)
 * where there is no cyclic paths for any of its vertices.
 *
 * A cylic path is a path from a vertex back to itself by following the
 * direction of the edges.
 *
 * @author		Jesus M. Salvo Jr.
 */

public class DirectedAcyclicGraphImpl extends DirectedGraphImpl implements DirectedAcyclicGraph {
  /**
   * Delegate object to handle topological sorting
   */
  private TopologicalSorting topologicalsorting;

  private DirectedAcyclicGraphListener listener;
    
  /**
   * Creates a DirectedAcyclicGraph object.
   */
  public DirectedAcyclicGraphImpl() {
    super();
    topologicalsorting = new TopologicalSorting( this );
    listener = new DirectedAcyclicGraphListener(this);
  }

  /**
   * @see DirectedAcyclicGraph#getRoot
   */
  public List getRoot( ){
      return listener.getRoot();
  }

  /**
   * @see TopologicalSorting#traverse()
   */
  public List topologicalSort( ){
    return this.topologicalsorting.traverse();
  }

  /**
   * @see TopologicalSorting#reverseTraverse()
   */
  public List reverseTopologicalSort( ){
    return this.topologicalsorting.reverseTraverse();
  }

  /**
   * @see TopologicalSorting#traverse(Vertex)
   */
  public List topologicalSort( Vertex startat ){
    return this.topologicalsorting.traverse( startat );
  }

  /**
   * @see TopologicalSorting#reverseTraverse(Vertex)
   */
  public List reverseTopologicalSort( Vertex startat ){
    return this.topologicalsorting.reverseTraverse( startat );
  }

}
