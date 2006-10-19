package salvo.jesus.graph;

import java.io.*;

/**
 * A vertex in a graph. This class encapsulates an object that the vertex will represent.
 * Hence, a Vertex can represent any object that extends java.lang.Object by simply
 * calling setObject() or specifying the objet on the constructor.
 *
 * @author		Jesus M. Salvo Jr.
 */
public class VertexImpl implements Vertex {

  /**
   * Delegator to handle label methods defined in <tt>LabeledGraphComponent</tt> interface
   */
  LabeledGraphComponentImpl   labelDelegator;

  /**
    * The object that the vertex represents.
    */
  protected Object	object;

  /**
    * Constructor that initializes this.object to null
    */
  public VertexImpl( ){
    object = null;
    this.labelDelegator = new LabeledGraphComponentImpl();
    this.labelDelegator.setLabel( "<Null vertex>" );
  }

  /**
    * Creates a new Vertex object that initializes this.object to newobject
    *
    * @param	newobject	The object that the Vertex will encapsulate
    */
  public VertexImpl( Object newobject ) {
    this.object = newobject;
    this.labelDelegator = new LabeledGraphComponentImpl();
    this.labelDelegator.setLabel( this.object.toString() );
  }

  /**
    * Getter method that returns the object that the Vertex represents
    *
    * @return	The object that this Vertex encapsulates
    */
  public Object getObject( ){
    return object;
  }

  /**
    * Setter method sets this.object to newobject
    */
  public void setObject( Object newobject ){
    this.object = newobject;
  }

  public boolean hasLabel() {
    return this.labelDelegator.hasLabel();
  }

  /**
   * Returns the label for this vertex. Keep in mind that <tt>Vertex</tt>.
   * now extend <tt>LabeledGraphComponent</tt>, so that this method
   * really just call <tt>LabeledGraphComponent.getLabel()</tt>.
   * <p>
   * Initially, when the vertex is created, the label is the the value
   * of the <tt>toString()</tt> method of the object passed-in to the constructor.
   * In the case of the constructor with no argument called, the label
   * is initially "<Null vertex>". After that, the label has no more relation
   * to the object encapsulated by the <tt>Vertex</tt>, so that even changing
   * the object encapsulated by the <tt>Vertex</tt> does not change the label at all.
   */
  public String getLabel() {
    return this.labelDelegator.getLabel();
  }

  public void setLabel( String label ) {
    this.labelDelegator.setLabel( label );
  }


  /**
    * If <tt>setString()</tt> has never been called, this
    * method then simply calls <tt>this.object.toString()</tt>.
    * Otherwise, it returns the string that was set during the
    * call to <tt>setString()</tt>.
    *
    * @return   String representation of the Vertex object
    */
  public String toString( ){
    return this.labelDelegator.getLabel();
  }
}
