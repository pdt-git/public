package salvo.jesus.graph.algorithm;

import salvo.jesus.graph.*;
import java.util.*;

/**
 * A contrete implementation of the MinimumSpanningTreeAlgorithm using
 * Kruskal's method.
 *
 * @author Jesus M. Salvo Jr.
 */

public class MinimumSpanningTreeKruskalAlgorithm extends MinimumSpanningTreeAlgorithm {

  /**
   * Creates an instance of MinimumSpanningTreeKruskalAlgorithm
   *
   * @param wgraph  The WeightedGraph where the minimum spanning tree will be determined.
   */
  public MinimumSpanningTreeKruskalAlgorithm( WeightedGraph wgraph ) {
    super( wgraph );
  }

  /**
   * Determine the minimum spanning tree of a weighted graph using Kruskal's method.
   */
  public WeightedGraph minimumSpanningTree() {
    HashSet   allEdges = new HashSet();
    WeightedGraph  spanningtree = new WeightedGraphImpl();
    Graph     spanningtestgraph = new GraphImpl();
    List      addedvertices = new ArrayList( 10 );
    Vertex    v1, v2;
    Iterator  iterator;
    List      incidentEdges;
    Iterator  edgeiterator;
    Edge      edge;

    // Collate all edges in the graph.
    // Use a HashSet to prevent the same Edge being stored twice.
    iterator = this.wgraph.getVerticesIterator();
    WeightedEdge  currentEdge;
    while( iterator.hasNext() ) {
      incidentEdges = wgraph.getEdges( (Vertex) iterator.next());
      edgeiterator = incidentEdges.iterator();
      while( edgeiterator.hasNext() ) {
        allEdges.add( (WeightedEdge) edgeiterator.next() );
      }
    }

    // Copy the HashSet to a List so we can sort the edges using
    // the sort() method of the Collections object.
    List listEdges = new ArrayList( allEdges );
    Collections.sort( listEdges,
      new Comparator() {
        public int compare( Object obj1, Object obj2 ) {
          WeightedEdge edge1 = (WeightedEdge) obj1;
          WeightedEdge edge2 = (WeightedEdge) obj2;

          if( edge1.getWeight() < edge2.getWeight() )
            return -1;
          else if( edge1.getWeight() > edge2.getWeight() )
            return 1;
          else
            return 0;
        }

        public boolean equals( Object obj ) {
          return obj.equals( this );
        }
      });

    iterator = listEdges.iterator();
    // For each edge ...
    while( iterator.hasNext()) {
      edge = (WeightedEdge) iterator.next();

      // We make use of a graph itself to test the feasibility
      // of adding an edge into the spanning tree
      v1 = edge.getVertexA();
      v2 = edge.getVertexB();

      try {
          // Of courese, only add the vertex if it does not exist yet.
          if( !addedvertices.contains( v1 ) ) {
            spanningtestgraph.add( v1 );
            addedvertices.add( v1 );
          }
          if( !addedvertices.contains( v2 ) ) {
            spanningtestgraph.add( v2 );
            addedvertices.add( v2 );
          }

          // ... if the edge will not cause a cycle among the edges
          // in the spanning tree, add the edge to the spanning tree.
          if( !spanningtestgraph.isConnected( v1, v2 ) ) {
            spanningtestgraph.addEdge( v1, v2 );
            spanningtree.addEdge( edge );
          }
      }
      catch( Exception ex ) {
        ex.printStackTrace();
      }

    }

    spanningtestgraph = null;
    return spanningtree;
  }
}