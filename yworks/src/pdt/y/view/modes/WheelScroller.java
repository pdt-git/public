package pdt.y.view.modes;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.Point2D;

import y.view.Graph2DView;

public class WheelScroller implements MouseWheelListener
{
  protected Graph2DView view;
  
  public WheelScroller(Graph2DView view)
  {
    this.view = view;
  }
  
  public void mouseWheelMoved(MouseWheelEvent e)
  {
    Point2D p2 = view.getCenter();
    Point p = view.getViewPoint();
    Dimension d = view.getViewSize();
    Rectangle r = view.getWorldRect();
    
    if (e.getWheelRotation() >= 0)
    {
      if (r.getY() + r.getHeight() - 1 > p.getY() + d.height / view.getZoom())
        p2.setLocation(p2.getX(), p2.getY() + e.getScrollAmount());
    }
    else
    {
      if (r.getY() + 1 < p.getY())
        p2.setLocation(p2.getX(), p2.getY() - e.getScrollAmount());
    }
    
    if (r.contains(p2))
    {
      view.setCenter(p2.getX(), p2.getY());
      view.updateView();
    }
  }
}
