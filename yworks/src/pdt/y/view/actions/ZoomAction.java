package pdt.y.view.actions;

import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import y.view.Graph2DView;

/**
   * Action that applies a specified zoom level to the view.
   */
  public class ZoomAction extends AbstractAction {
    /**
	 * 
	 */
	private static final long serialVersionUID = 2285120004107395797L;
	double factor;
	private Graph2DView view;

    public ZoomAction(Graph2DView view, double factor) {
      super("Zoom " + (factor > 1.0 ? "In" : "Out"));
      URL imageURL;
      if (factor > 1.0d) {
        imageURL = ClassLoader.getSystemResource("demo/view/resource/zoomIn.png");
      } else {
        imageURL = ClassLoader.getSystemResource("demo/view/resource/zoomOut.png");
      }
      if (imageURL != null) {
        this.putValue(Action.SMALL_ICON, new ImageIcon(imageURL));
      }
      this.putValue(Action.SHORT_DESCRIPTION, "Zoom " + (factor > 1.0 ? "In" : "Out"));
      this.factor = factor;
      this.view = view;
    }

    public void actionPerformed(ActionEvent e) {
      view.setZoom(view.getZoom() * factor);
      // optional code that adjusts the size of the
      // view's world rectangle. The world rectangle
      // defines the region of the canvas that is
      // accessible by using the scroll bars of the view.
      Rectangle box = view.getGraph2D().getBoundingBox();
      view.setWorldRect(box.x - 20, box.y - 20, box.width + 40, box.height + 40);

      view.updateView();
    }
  }