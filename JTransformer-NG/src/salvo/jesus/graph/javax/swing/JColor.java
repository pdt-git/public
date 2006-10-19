package salvo.jesus.graph.javax.swing;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.io.*;

/**
 * Encapsulates a Color object so that the Color can be changed without
 * changing reference to the JColor object.
 * <p>
 * This class was created because the Color class has no method to change
 * its existing Color composition, making it difficult to change a color
 * without creating another Color object.
 * <p>
 * Therefore, programs should make a refernece to a JColor object instead
 * of a Color object, allowing changing of the encapsulated Color object
 * without changing the reference to the JColor object.
 *
 * @author  Jesus M. Salvo Jr.
 */
public class JColor implements Serializable {
  /**
   * Color object represented by JColor.
   */
  Color color;

  /**
   * A List object of ActionListeners interesting in listening
   * for changes in the Color object encapsulated by JColor.
   */
  java.util.List listeners;

  /**
   * Creates a JColor object that encapsulates a Color object.
   *
   * @param color   The Color object to be encapsulated.
   */
  public JColor( Color color ) {
    this.color = color;
    this.listeners = new ArrayList( 10 );
  }

  /**
   * Returns the Color encapsulated by JColor.
   */
  public Color getColor() {
    return this.color;
  }

  /**
   * Changes the Color object encapsulated by JColor
   *
   * @param   color   New Color object to be encapsulated.
   */
  public void setColor( Color color ) {
    ActionListener  listener;
    int count = listeners.size(), i;

    this.color = color;
    for( i = 0; i < count; i++ ) {
      listener = (ActionListener) listeners.get( i );
      listener.actionPerformed( new ActionEvent( this, 0, "Color Changed" ));
    }
  }

  /**
   * Registers a listener to be notified of changes to the Color object
   * encapsulated by JColor.
   *
   * @param   listener  ActionListener object to be notified.
   */
  public void addActionListener( ActionListener listener ) {
    this.listeners.add( listener );
  }

  /**
   * Unregisters a listener of changes in Color object encapsulated by JColor.
   */
  public void removeActionListener( ActionListener listener ) {
    this.listeners.remove( listener );
  }
}

