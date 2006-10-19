package salvo.jesus.util;

import java.util.Comparator;
import java.io.*;

/**
 * A Comparator to compare HeapNodes based on their priority.
 * Two <tt>HeapNode</tt>s are the same if they have the same priority,
 * regardless of the object encaspulated by the <tt>HeapNode</tt>s.
 *
 * @author  Jesus M. Salvo Jr.
 */
public class HeapNodeComparator implements Comparator, Serializable {
  /**
   * Control how compare() return its value. If order is < 0,
   * then compare() will return -1 if the priority of the first heap node
   * argument is numerically less than the priority of the second heap node argument.
   * If order >= 0, then compare() will return -1 if the priority of the first heap node
   * argument is numerically greater than priority of the second heap node argument.
   */
  int   order;

  /**
   * Creates an instance of HeapNodeComparator
   */
  public HeapNodeComparator( int order ) {
    this.order = order;
  }

  /**
   * Compares two HeapNode objects.
   *
   * @param o1  Must be an instance of HeapNode
   * @param o2  Must be an instance of HeapNode
   */
  public int compare(Object o1, Object o2) {
    HeapNode node1 = (HeapNode) o1;
    HeapNode node2 = (HeapNode) o2;
    double    node1priority = node1.getPriority();
    double    node2priority = node2.getPriority();

    if ( this.order < 0 ) {
      if ( node1priority < node2priority )
        return -1;
      else if ( node1priority > node2priority )
        return 1;
      else
        return 0;
    }
    else {
      if ( node1priority < node2priority )
        return 1;
      else if ( node1priority > node2priority )
        return -1;
      else
        return 0;
    }
  }

  public boolean equals(Object obj) {
    return obj.equals( this );
  }
}
