package salvo.jesus.graph.javax.swing;

import java.awt.event.*;
import java.awt.*;
import javax.swing.*;
import java.io.*;

/**
 * Listens for both button clicks / press on a JButton representing a color and
 * on changes of the Color object represented by a JColor object.
 * <p>
 * The JButton object - button - is essentially a view of the model JColor.
 * This is therefore a controller between the JColor object and the button
 * representing the JColor object.
 *
 * @author    Jesus M. Salvo Jr.
 */
public class JButtonColorListener implements ActionListener, Serializable {
  Component   parent;
  JColor      color;
  JButton     button;
  JGraphColorChooser  colorchooser;
  JDialog     dialog;

  /**
   * Creates a ButtonColorListener object. (The controller)
   *
   * @param   parentpanel   The component where the button is a component of.
   * @param   color         The JColor object that the button models itself on (The model)
   * @param   button        The JButton object that visually represents a JColor object. (The view)
   */
  public JButtonColorListener( Component parent, JColor color, JButton button ) {
    this.parent = parent;
    this.color = color;
    this.button = button;
    this.colorchooser = new JGraphColorChooser( color.getColor() );
  }

  /**
   * Implementation of the actionPerformed() method of the ActionListener interface.
   * This method is called when a JColorButton is clicked / pressed and when
   * the Color object encapsulated in a JColor object is changed via the JColor.setColor() method.
   *
   * @param   e   ActionEvent object
   */
  public void actionPerformed( ActionEvent e ){
    if( e.getSource() instanceof JButton )
      buttonClicked();
    else if( e.getSource() instanceof JColor )
      colorChanged();
  }

  /**
   * Called by actionPerformed() if the source object is an istance of JColor.
   * This method will change the background color of the button to be the same
   * as the new color of JColor.
   */
  private void colorChanged() {
    button.setBackground( color.getColor() );
  }

  /**
   * Called by actionPerformed() if the source object is an istance of JButton.
   * This method creates and shows a color chooser dialog. If a new color
   * was specified in the color chooser, then JColor's color is changed
   * which also triggers changing the color of the button.
   */
  private void buttonClicked() {
    dialog = JColorChooser.createDialog( this.parent, "Choose Color",
      true, colorchooser, new ActionListener() {
          /*
           * Listener for the OK button of the dialog box.
           * This will change the color (model) and the button (view) representing
           * the color.
           */
          public void actionPerformed( ActionEvent e ) {
            color.setColor( colorchooser.getColor());
            dialog.setVisible( false );
            dialog.dispose();
          }
        },
          /*
           * Listener for the Cancel button of the dialog box.
           * Ignores any changes.
           */
          new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
              dialog.setVisible( false );
              dialog.dispose();
            }
          } );

    if( dialog != null ) {
      dialog.pack();
      dialog.setVisible( true );
    }
  }
}

