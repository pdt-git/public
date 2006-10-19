package salvo.jesus.graph.algorithm;

import salvo.jesus.graph.*;
import salvo.jesus.util.*;
import java.util.*;

/**
 * A concrete implementation of ShortestPathAlgorithm using Dijkstra's method.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class ShortestPathDijkstraAlgorithm extends ShortestPathAlgorithm {
  /**
   * List of vertices that has been added to the tree
   */
  private List  visited;

  /**
   * Heap of FringeObjects that are to be processed.
   */
  private Heap  fringe;

  /**
   * Subgraph forming the shortest spanning tree.
   */
  private WeightedGraph shortestpathtree;

  /**
   * Comparator used to be compare priorities in the fringe.
   */
  private HeapNodeComparator  comparator;

  /**
   * Creates an instance of ShortestPathDijkstraAlgorithm.
   *
   * @param wgraph  The WeightedGraph where a shortest path spanning tree will be determined.
   * @param comparator  The HeapNodeComparator to be used to compare priorities of objects in the fringe/heap.
   */
  public ShortestPathDijkstraAlgorithm( WeightedGraph wgraph, HeapNodeComparator comparator ) {
    super( wgraph );
    this.visited = new ArrayList( 10 );
    this.comparator = comparator;
    this.fringe = new Heap( new HeapNodeComparator( 1 ));
  }

  /**
   * Determines the shortest path from a given vertex to all other vertices
   * that are in the same connected set as the given vertex in the weighted graph
   * using Dijkstra's algorithm.
   *
   * @return  A WeightedGraph comprising of the shortest path spanning tree.
   * @param from  The Vertex from where we want to obtain the shortest path
   * to all other vertices.
   */
  public WeightedGraph shortestPath( Vertex from ) {
    // clear the graph.
    // this.shortestpathtree.clear();
    this.shortestpathtree = new WeightedGraphImpl();

    // Now add the vertex we are interested in to the fringe.
    this.fringe.insert( new HeapNode( new FringeObject( from, null ), 0.0 ));

    // Continually process while there are vertices in the heap.
    while( !this.fringe.isEmpty() ) {

      // Moves the highest priority node from the heap to the tree.
      this.moveToVisited();
    }

    // Clear the vectors
    this.visited.clear();
    this.fringe.clear();

    return shortestpathtree;
  }

  /**
   * Moves the vertex that has the highest priority in the heap
   * from the fringe to the tree,
   */
  private void moveToVisited() {
    HeapNode  prioritynode;

    // Remove the node with highest priority from the fringe ...
    prioritynode = this.fringe.remove( );

    FringeObject  fringeobject = (FringeObject) prioritynode.getObject();
    Vertex    priorityvertex = fringeobject.vertex;

    // ... and add it to the tree.
    this.visited.add( priorityvertex );
    if( fringeobject.edge != null ) {
        try {
            this.shortestpathtree.addEdge( fringeobject.edge );
        }
        catch( Exception ex ) {
            ex.printStackTrace();
        }
    }

    // ... then move all the adjacent vertices of the vertex
    // we just moved to the tree and put them in the fringe.
    this.moveAdjacentVerticesToFringe( prioritynode );
  }

  /**
   * Gets all the adjacent vertices from unseen to the fringe.
   * The parameter vertex must be a vertex that is being moved
   * from the fringe to the tree. This method must only be called
   * by the method moveToVisited().
   *
   * @param vertex  The vertex that is being moved from the fringe to the tree
   * and whose adjacent vertices we want added to the fringe.
   */
  private void moveAdjacentVerticesToFringe( HeapNode prioritynode ) {
    // Get the vertex encapsulated by the heap node
    Vertex    priorityvertex = ((FringeObject) prioritynode.getObject()).vertex;

    // Then get the edges of the vertex
    // We cant use wgraph.getAdjacentVertices() as that will
    // not tell us what edge to use.
    List    incidentedges = this.wgraph.getEdges( priorityvertex );

    // Get the priority of the heap node
    double    priority = prioritynode.getPriority();

    // We need to iterate through the edges
    Iterator  iterator = incidentedges.iterator();

    // Because we are using HeapNodes in the fringe,
    // we need a way to compare the object (a Vertex) encapsulated in a HeapNode in the
    // fringe and a Vertex
    HeapNodeObjectComparator  heapnodeobjectcomparator = new HeapNodeObjectComparator();

    // The current incident edge iterated
    WeightedEdge  incidentedge;

    // The current adjacent vertex within the iteration
    Vertex    adjacentvertex;

    // Either an existing HeapNode or an exsting one in the fringe
    HeapNode  heapnode;

    // The new priority of a heapnode
    double    fringepriority;

    // For each edge of the vertex ...
    while( iterator.hasNext() ) {
      incidentedge = (WeightedEdge) iterator.next();

      // ... get the vertex opposite to the priority vertex
      // meaning get the adjacent vertex
      adjacentvertex = incidentedge.getOppositeVertex( priorityvertex );

      // ... check if the adjacent vertex has been visited
      // If it has been visited, then proceed with the next edge
      if( this.visited.contains( adjacentvertex )) continue;

      // ... check if the adjacent vertex is already in the fringe
      heapnode = (HeapNode) this.fringe.contains( adjacentvertex, heapnodeobjectcomparator );

      // ... if it is not yet on the fringe, add it to the fringe
      if( heapnode == null ) {
        fringepriority = priority + incidentedge.getWeight();
        heapnode = new HeapNode( new FringeObject( adjacentvertex, incidentedge ), fringepriority );
        this.fringe.insert( heapnode );
      }
      // If it is already the fringe, reassign its priority,
      // only if it will give it a "higher" priority.
      // Use the HeapNodeComparator to determine which is "higher".
      else {
        fringepriority = priority + incidentedge.getWeight();
        if( this.comparator.compare( new HeapNode( adjacentvertex, fringepriority), heapnode ) < 0 ) {
          this.fringe.setPriority( heapnode, fringepriority );
          // Also reassign the edge that is used to get the shortest path
          FringeObject   fobject = (FringeObject) heapnode.getObject();
          fobject.edge = incidentedge;
        }
      }

    }
  }

}

/**
 * A Comparator for comparing the Vertex in the FringObject of the HeapNode
 * against another Vertex.
 */
class HeapNodeObjectComparator implements Comparator {

  /**
   * Compare a Vertex against the Vertex in the FringeObject of a HeapNode.
   * The comparison is made using the == operator to compare the actual instance.
   *
   * @return  Returns 0 if they are the same object. Returns -1 otherwise.
   */
  public int compare( Object vertex, Object hnode ) {
    Vertex    compareTo = (Vertex) vertex;
    HeapNode  heapnode = (HeapNode) hnode;
    FringeObject  fringeobject = (FringeObject) heapnode.getObject();
    Vertex    heapobject = (Vertex) fringeobject.vertex;

    if( compareTo == heapobject )
      return 0;
    else
      return -1;
  }
}

/**
 * An Object encapsulated in a HeapNode, which in turn is added to the fringe.
 * The classical algorithm only mentions of storing vertices in the fringe, but
 * it is programatically difficult to determine what edge to add to the
 * shortest path spanning tree when moving a vertex from the fringe to the tree.
 * It is therefore easier to store the edge along with the vertex in the fringe.
 *
 * The edge stored along with the vertex in the fringe is the
 * edge connecting the vertex that has been moved from the fringe to the tree
 * and the vertex adjacent to the vertex that has been moved from the fringe
 * and being added to the fringe. Got that?
 *
 */
class FringeObject {
  Vertex        vertex;
  WeightedEdge  edge;

  public FringeObject( Vertex vertex, WeightedEdge edge ) {
    this.vertex = vertex;
    this.edge = edge;
  }

  public String toString() {
    return "Vertex: " + vertex + "; Edge: " + edge;
  }
}
