package salvo.jesus.graph;

import java.util.*;

/**
 * This event is used to notify interested parties that a Vertex object
 * has been added to a Graph object.
 *
 * @author		Jesus M. Salvo Jr.
 */
public class GraphAddVertexEvent extends EventObject {
  /**
    * The Vertex object that was added to a Graph object
    */
  private Vertex vertex;

  /**
   * see getEdge()
   */
  private Edge edge;
    
  /**
    * Creates a GraphAddVertexEvent object indicating the source of the event
    * and the Vertex that was added to a Graph object
    *
    * @param	source		source of the event. This is usually the Graph object
    * where the Vertex object was added.
    * @param	newvertex	Vertex object that was added to a Graph object
    * @param	edge		see getEdge()
    */
  public GraphAddVertexEvent( Object source, Vertex newvertex, Edge edge ) {
    super( source );
    this.vertex = newvertex;
    this.edge = edge;
  }

  /**
    * Returns the Vertex object that was added to a Graph object
    *
    * @return		The Vertex object added
    */
  public Vertex getVertex( ) {
    return this.vertex;
  }

  /**
   * If this vertex is being added implicitly due to the addition of an
   * edge, then the edge being added; else null.
   */
  public Edge getEdge()
  {
      return edge;
  }
    
}
