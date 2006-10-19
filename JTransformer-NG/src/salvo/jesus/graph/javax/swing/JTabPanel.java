package salvo.jesus.graph.javax.swing;

import java.io.*;
import javax.swing.JPanel;

/**
 * Abstract class for components to act as a tabpage in a GraphTabbedPane,
 * which is a subclass of JTabbedPane.
 *
 * Classes which implement this class must implement the apply()
 * and ok() methods, which are automatically called when the Apply
 * and OK JButtons are pressed presumably on a container that contains
 * a GraphTabbedPane, such as VisualGraphComponentPropertiesPanel.
 *
 * @author  Jesus M. Salvo Jr.
 */
public abstract class JTabPanel extends JPanel implements Serializable {

  /**
   * The intention is that when an * "Apply" JButton is clicked on a dialog,
   * all JTabPanels (tabpages) will have their apply() method called.
   */
  public abstract void apply();

  /**
   * The intention is that when an * "OK" JButton is clicked on a dialog,
   * all JTabPanels (tabpages) will have their ok() method called.
   */
  public abstract void ok();
}

