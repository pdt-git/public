package salvo.jesus.graph;

import java.io.*;

/**
 * This is a non-public class the extends salvo.jesus.graph.Vistor
 * to check if there is a cycle from a Vertex back to itself.
 *
 * @author		Jesus M. Salvo Jr.
 * @see			salvo.jesus.graph.Visitor
 */
class CycleCheckVisitor extends NullVisitor implements Serializable {
  /**
    * Vertex to check for a cycle path
    */
  Vertex	objectToCheck;

  /**
    * Creates a new instance of CheckCycleVisitor and specifies
    * which Vertex to be checked for a cycle path
    *
    * @param		objectToCheck		Vertex to be checked for a cycle path
    */
  CycleCheckVisitor( Vertex objectToCheck ){
    super();
    this.objectToCheck = objectToCheck;
  }

  /**
    * Override of superclass' visit() method. Compares the Vertex
    * being visited to the Vertex we are checking for a cycle path.
    * If they are the same, a cycle has been reached and false is returned.
    * Otherwise, true is returned
    *
    * @param		objectToVisit		Vertex being visited. This is compared
    * to the Vertex we are trying to check for a cycle path.
    * @return	false if the Vertex being visited is the same as the
    * Vertex we are trying to check for a cycle path. True otherwise.
    */
  public boolean visit( Vertex objectToVisit ){
    if( objectToVisit == objectToCheck )
      return false;
    else
      return true;
  }
}
