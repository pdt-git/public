package salvo.jesus.graph;

import java.util.*;

/**
 * This event is used to notify interested parties that a Vertex object
 * is about to be removed from a Graph object.
 *
 * @author		Jesus M. Salvo Jr.
 */
public class GraphRemoveVertexEvent extends EventObject {
  /**
    * The Vertex object that is about to be removed from a Graph object
    */
  Vertex	vertex;

  /**
    * Creates a GraphRemoveVertexEvent object indicating the source of the event
    * and the Vertex is about to be removed from a Graph object
    *
    * @param	source		source of the event. This is usually the Graph object
    * where the Vertex object is to be removed
    * @param	newvertex	Vertex object that is about to be removed from a Graph object
    */
  public GraphRemoveVertexEvent( Object source, Vertex vertextoremove ) {
    super( source );
    this.vertex = vertextoremove;
  }

  /**
    * Returns the Vertex object is about to be removed from a Graph object
    *
    * @return		The Vertex object to be removed
    */
  public Vertex getVertex( ) {
    return vertex;
  }
}
