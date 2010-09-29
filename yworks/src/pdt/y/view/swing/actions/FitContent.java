package pdt.y.view.swing.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

import y.view.Graph2DView;
  /**
   * Action that fits the content nicely inside the view.
   */
  public class FitContent extends AbstractAction {
	/**
	 * 
	 */
	private static final long serialVersionUID = 5976057135568753518L;
	
	
	private Graph2DView view;  
    public FitContent(final Graph2DView view) {
      super("Fit Content");
      this.view=view;
    }

    @Override
	public void actionPerformed(ActionEvent e) {
      view.fitContent();
      view.updateView();
    }
  }