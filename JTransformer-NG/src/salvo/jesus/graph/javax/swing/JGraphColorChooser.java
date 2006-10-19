package salvo.jesus.graph.javax.swing;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.colorchooser.*;

/**
 * A subclass of JColorChooser that has no preview panel.
 * We may have to customize this later on to use a different color chooser.
 * There is nothing special about this class apart from removing the preview panel.
 *
 * @author  Jesus M. Salvo Jr.
 */
public class JGraphColorChooser extends JColorChooser {

  /**
   * Creates a JGraphColorChooser object that has no preview panel.
   */
  public JGraphColorChooser() {
    super();
    try {
      initJGraphColorChooser();
    }
    catch( Exception e ) {
      e.printStackTrace();
    }
  }

  /**
   * Creates a JGraphColorChooser object with the specified color
   * initially selected but has no preview panel.
   *
   * @param   initialcolor    color to be initially selected
   */
  public JGraphColorChooser( Color initialcolor ) {
    super( initialcolor );
    try {
      initJGraphColorChooser();
    }
    catch( Exception e ) {
      e.printStackTrace();
    }
  }

  /**
   * Creates a JGraphColorChooser object with the specified
   * selection model
   *
   * @param   model   ColorSelectionModel to be used
   */
  public JGraphColorChooser( ColorSelectionModel model ) {
    super( model );
    try {
      initJGraphColorChooser();
    }
    catch( Exception e ) {
      e.printStackTrace();
    }
  }

  /**
   * Called by the constructors. This method removes the preview panel.
   */
  private void initJGraphColorChooser() throws Exception {
    this.setPreviewPanel( new JPanel());
  }
}

