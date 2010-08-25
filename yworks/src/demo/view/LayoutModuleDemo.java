/****************************************************************************
 **
 ** This file is part of yFiles-2.7.0.1. 
 ** 
 ** yWorks proprietary/confidential. Use is subject to license terms.
 **
 ** Redistribution of this file or of an unauthorized byte-code version
 ** of this file is strictly forbidden.
 **
 ** Copyright (c) 2000-2010 by yWorks GmbH, Vor dem Kreuzberg 28, 
 ** 72070 Tuebingen, Germany. All rights reserved.
 **
 ***************************************************************************/
package demo.view;

import demo.view.DemoBase;
import y.module.LayoutModule;
import y.option.OptionHandler;
import y.view.Arrow;
import y.view.hierarchy.HierarchyManager;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.JToolBar;
import javax.swing.ListCellRenderer;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.Action;
import javax.swing.JButton;

/**
 * Demonstrates how layout modules can be added to the GUI of an application.
 * A layout module is a layout algorithm combined
 * with an option dialog, that allows to change the
 * options of a layout algorithm interactively
 * (only available if layout is part of distribution).
 *
 */
public class LayoutModuleDemo extends DemoBase {
  public LayoutModuleDemo() {
    //use a delta arrow to make edge directions clear
    view.getGraph2D().getDefaultEdgeRealizer().setArrow(Arrow.DELTA);

    //to enable loading of hierachically grouped graphs/
    new HierarchyManager(view.getGraph2D());
    loadGraph("resource/sample.graphml");
  }

  /**
   * Creates a toolbar for choosing, configuring, and running layout algorithms.
   */
  protected JToolBar createToolBar() {
    final JComboBox modules = new JComboBox();
    modules.setMaximumSize(new Dimension(200, 100));
    modules.setRenderer(new ListCellRenderer() {
      final DefaultListCellRenderer dlcr = new DefaultListCellRenderer();

      public Component getListCellRendererComponent(
              final JList list,
              final Object value,
              final int index,
              final boolean isSelected,
              final boolean cellHasFocus
      ) {
        if (value instanceof LayoutModule) {
          String name = ((LayoutModule) value).getModuleName().toLowerCase();
          if ("circular".equals(name)) {
            name = "Circular Layout";
          } else if ("diagonal".equals(name)) {
            name = "Diagonal Layout";
          } else if ("incremental_hierarchic".equals(name)) {
            name = "Incremental Hierarchic Layout";
          } else if ("organic".equals(name)) {
            name = "Organic Layout";
          } else if ("orthogonal_layouter".equals(name)) {
            name = "Orthogonal Layout";
          } else if ("random".equals(name)) {
            name = "Random Layout";
          } else if ("tree".equals(name)) {
            name = "Tree Layout";
          }
          return dlcr.getListCellRendererComponent(list, name, index, isSelected, cellHasFocus);
        } else {
          return dlcr.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        }
      }
    });
    modules.addItem(new y.module.IncrementalHierarchicLayoutModule());
    modules.addItem(new y.module.OrganicLayoutModule());
    modules.addItem(new y.module.OrthogonalLayoutModule());
    modules.addItem(new y.module.CircularLayoutModule());
    modules.addItem(new y.module.RandomLayoutModule());
    modules.addItem(new OrthogonalLayoutModule());
    modules.setSelectedIndex(1);

    final JToolBar jtb = super.createToolBar();
    jtb.addSeparator();
    jtb.add(modules);
    jtb.add(createActionControl(new AbstractAction("Configure Layout") {
      {
        final URL resource = DemoBase.class.getResource("resource/properties.png");
        if (resource != null) {
          putValue(Action.SMALL_ICON, new ImageIcon(resource));
          putValue(Action.SHORT_DESCRIPTION, "Configure Layout");
        }
      }
      public void actionPerformed( final ActionEvent e ) {
        final LayoutModule module = (LayoutModule) modules.getSelectedItem();
        if (module != null) {
          final OptionHandler settings = module.getOptionHandler();
          if (settings != null) {
            settings.showEditor();
          }
        }
      }
    }));
    jtb.add(createActionControl(new AbstractAction("Calculate Layout") {
      {
        final URL resource = DemoBase.class.getResource("resource/layout.png");
        if (resource != null) {
          putValue(Action.SMALL_ICON, new ImageIcon(resource));
          putValue(Action.SHORT_DESCRIPTION, "Calculate Layout");
        }
      }
      public void actionPerformed( final ActionEvent e ) {
        final LayoutModule module = (LayoutModule) modules.getSelectedItem();
        if (module != null) {
          module.start(view.getGraph2D());
        }
      }
    }));
    return jtb;
  }

  /**
   * Creates a control for triggering the specified action from the
   * demo toolbar.
   * @param action   the <code>Action</code> that is triggered by the
   * created control.
   * @return a control for triggering the specified action from the
   * demo toolbar.
   */
  private JComponent createActionControl( final Action action ) {
    return new JButton(action);
  }

  public static void main( String[] args ) {
    EventQueue.invokeLater(new Runnable() {
      public void run() {
        initLnF();
        (new LayoutModuleDemo()).start();
      }
    });
  }
}
