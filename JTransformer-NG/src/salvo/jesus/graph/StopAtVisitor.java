package salvo.jesus.graph;

/**
 * A <tt>Visitor</tt> that notifies a traversal to stop at a particular <tt>Vertex</tt>.
 *
 * @author Jesus M. Salvo Jr.
 *
 * $Id$
 */

public class StopAtVisitor extends NullVisitor {
  /**
    * Vertex to check for when visiting a vertex
    */
  private Vertex	objectToCheck;

  /**
   * Set to true when objectToCheck visited.
   */
  private boolean found;
    
  /**
    * Creates a new instance of StopAtVisitor and specifies
    * which Vertex stop
    *
    * @param		objectToCheck		stop at the specified vertex
    */
  public StopAtVisitor( Vertex objectToCheck ){
    super();
    this.objectToCheck = objectToCheck;
  }

  /**
    * Override of superclass' visit() method. Compares the Vertex
    * being visited to the Vertex specified in the constructor.
    * If they are the same, return false. Otherwise, return true.
    *
    * @param	objectToVisit		Vertex being visited.
    * @return	false if the Vertex being visited is the same as the
    * Vertex specified in the constructor. True otherwise.
    */
  public boolean visit( Vertex objectToVisit ){
      if( objectToVisit == objectToCheck ) {
          found = true;
          return false;
      } else {
          return true;
      }
  }

  /**
   * @return whether the specified object was found during the traversal.
   */
  public boolean wasFound()
  {
      return found;
  }
}
