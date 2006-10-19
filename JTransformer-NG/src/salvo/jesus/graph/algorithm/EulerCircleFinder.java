package salvo.jesus.graph.algorithm;

import salvo.jesus.graph.*;
import javax.swing.*;
import java.util.*;


/**
 *  This class finds an Euler circle in a graph. A Euler circle is a path through
 *  a graph that visits every edge of the graph excatly once and ends at the same
 *  vertex it started.
 *  <p>This algortihm does <strong>not</strong> check if the searched graph is entirely
 *  connected. It is the callers responsibility to assure the graph is entirely connected.
 *  Otherwise an Euler circle in any connected set of the graph will be found.
 *  <p>If there is no Euler circle in the graph a GraphException will be thrown when the
 *  graph is searched. Euler's Circle Law says a graph has a Euler circle if and only if
 *  the graph is entirely connected and every vertex has an even degree (i.e. number of
 *  edges).
 *  <p>It seems to me that the time needed to find a Euler circle using this
 *  algorithm is proportional to the number of edges in the graph.
 *  @author David Dannberg (9dannber@informatik.uni-hamburg.de)
 */
public class EulerCircleFinder {

  private final static EulerCircleFinder s_Singleton = new EulerCircleFinder();

  private static EulerCircleFinder getSingletonInstance() {
    return s_Singleton;
  }

  private Graph m_Graph;

  private EulerCircleFinder () {}

  /**
   *  Sets the graph that is to be searched for a Euler circle.
   *
   *  @param g the graph to be searched
   *  @throws IllegalArgumentException if <code>g</code> has no vertices.
   */
  private void setGraph (Graph g) {
    if (g.getVerticesCount() == 0)
      throw new IllegalArgumentException ("Graph has no vertices");
    try {
      m_Graph = clone (g);
    } catch (Exception e) {
      e.printStackTrace();
      throw new IllegalArgumentException ("Exception while creating copy of the Graph");
    }
  }

  private Graph getGraph () {
    return m_Graph;
  }



  private static void addCircle (List targetCircle, List newCircle) {
    // First and last element of newCircle must be same
    if (newCircle.size() == 0 || newCircle.get(0) != newCircle.get(newCircle.size()-1))
      throw new IllegalArgumentException();
    int position = targetCircle.indexOf(newCircle.get(0));
    targetCircle.remove(position);
    targetCircle.addAll(position, newCircle);
  }

  /**
   * Removes edges from the graph. Those vertices that have a degree of 0 (i.e. have no
   * more edges) after removing the given edges also removed.
   *
   * @param edgesToRemove a list of <code>edge</code> objects wich should be removed from
   *        the used graph.
   */
  private void removeEdges(List edgesToRemove) throws Exception {
    for (Iterator it = edgesToRemove.iterator();it.hasNext();) {
      getGraph().removeEdge((Edge)it.next());
    }
    Set lostVertices = getGraph().getVertices(0);
    for (Iterator it = lostVertices.iterator();it.hasNext();) {
      getGraph().remove((Vertex)it.next());
    }
  }

  /**
   * Returns an edge of a specified vertex, which is <strong>not</strong> contained in
   * a list.
   * @return The edge returned matches both of the following conditions:
   *         <ul>
   *         <li>The edge is an incident edge of the given vertex <code>v</code></li>
   *         <li>There is no object contained in the given list <code>usedEdges</code> that
   *             equals the edge</li>
   *         </ul>
   *         If the graph has no edge that matches these conditions <code>null</code> is
   *         returned
   */
  private Edge findUnusedEdge (Vertex v, List usedEdges) {
    List vertexEdges = getGraph().getEdges(v);
    vertexEdges.removeAll(usedEdges);
    if (vertexEdges.size() > 0) {
      return (Edge)vertexEdges.get(0);
    } else {
      return null;
    }
  }

  /**
   * Returns a Path wich is a Euler circle in the given graph or <code>null</code>
   * if there is no Euler circle in this graph.
   * @see #setGraph
   */
  public static Path find (Graph g)  {
      getSingletonInstance().setGraph(g);
      try {
        List eulerCirlce = getSingletonInstance().findEulerCircle((Vertex)getSingletonInstance().getGraph().getVerticesIterator().next());
        Path eulerPath = new PathImpl ();
        for (Iterator it = eulerCirlce.iterator();it.hasNext();) {
          try {
            eulerPath.add((Vertex)it.next());
          } catch (Exception e) {
            System.err.println("Could not create Path-Object. Path vertices are: " + eulerCirlce);
            System.err.println("Original Exception was");
            e.printStackTrace();
          }
        }
        return eulerPath;
      } catch (NoEulerCircleException nece) {
        return null;
      }
  }

  private List findEulerCircle(Vertex startVertex) throws NoEulerCircleException {
    switch (getGraph().getVerticesCount()) {
      case 0 : throw new RuntimeException ("Error in finding algorithm"); // should not occur
      case 1 : return Arrays.asList(new Vertex[] { startVertex });
      default:
        Vertex currentVertex = startVertex;
        List visitedEdges = new LinkedList();
        List visitedVertices = new LinkedList();
        visitedVertices.add(startVertex);
        // Find a primary circle in the graph within this loop
        do  {
          Edge unusedEdge = findUnusedEdge(currentVertex, visitedEdges);
          if (unusedEdge == null) {
            throw new NoEulerCircleException ();
          }
          currentVertex = unusedEdge.getOppositeVertex(currentVertex);
          visitedEdges.add(unusedEdge);
          visitedVertices.add(currentVertex);
        } while (startVertex != currentVertex);
        // remove primary circle from the graph
        try {
          removeEdges (visitedEdges);
        } catch (Exception e) {
          throw new RuntimeException ("Error in finding algorithm"); // should not occur
        }
        List primaryCircle = new LinkedList (visitedVertices);
        // find more circles in the remaining graph and combine all circles to one
        for (Iterator it = primaryCircle.iterator();it.hasNext();) {
          Vertex v = (Vertex)it.next();
          if (getGraph().cloneVertices().contains(v)) {
            addCircle(visitedVertices, findEulerCircle(v));
          }
        }
      return visitedVertices;
    }
  }
/*
  // Just a test
  public static void main (String[] args) throws Exception {
    Graph g = new GraphImpl ();
    Vertex v1 = new VertexImpl ("1");
    Vertex v2 = new VertexImpl ("2");
    Vertex v3 = new VertexImpl ("3");
    Vertex v4 = new VertexImpl ("4");
    Vertex v5 = new VertexImpl ("5");
    Vertex v6 = new VertexImpl ("6");
    Vertex v7 = new VertexImpl ("7");
    Vertex v8 = new VertexImpl ("8");
    g.add(v1); g.add(v2); g.add(v3); g.add(v4); g.add(v5); g.add(v6); g.add(v7); g.add(v8);
    g.addEdge(v1,v2);
    g.addEdge(v2,v3);
    g.addEdge(v3,v4);
    g.addEdge(v4,v1);
    g.addEdge(v2,v4);
    g.addEdge(v4,v5);
    g.addEdge(v5,v2);
    g.addEdge(v5,v6);
    g.addEdge(v6,v8);
    g.addEdge(v8,v5);
    g.addEdge(v8,v7);
    g.addEdge(v7,v1);
    g.addEdge(v1,v8);


    System.out.println(find(g));
    GraphEditor gp = new GraphEditor (g);
    JFrame frame = new JFrame ();
    frame.setContentPane(gp);
    frame.setSize(300,200);
    frame.setVisible(true);
  }
*/

  public static Graph clone(Graph g) throws Exception {
    Graph clone = new GraphImpl();
    for (Iterator vertexIt = g.getVerticesIterator();vertexIt.hasNext();) {
      Vertex v = (Vertex)vertexIt.next();
      clone.add(v);
    }

    for (Iterator vertexIt = clone.getVerticesIterator();vertexIt.hasNext();) {
      Vertex v = (Vertex)vertexIt.next();
      List edges = g.getEdges(v);
      for (Iterator edgesIt = edges.iterator();edgesIt.hasNext();) {
        Edge e = (Edge)edgesIt.next();
        if (e.getVertexA() == v) {
          clone.addEdge(e);
        }
      }
    }
    return clone;
  }
}

class NoEulerCircleException extends GraphException {}
