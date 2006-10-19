package salvo.jesus.util;

import java.util.*;
import java.io.*;

/**
 * A Queue represents a first-in-first-out (FIFO) data structure.
 * Unlike java.lang.Stack that simply extends java.util.Vector,
 * this class does not extend java.util.Vector but delegates its methods
 * to a java.util.Vector. Therefore, unlike java.util.Stack,
 * the methods from java.util.Vector are not available.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class Queue implements Serializable {
  List  delegate;

  /**
   * Creates an empty queue
   */
  public Queue() {
    this.delegate = new ArrayList( 10 );
  }

  /**
   * Puts an item at the end of the queue. Note that this will not
   * check of duplicate items in the queue.
   *
   * @param item    The item to be added at the end of the queue
   */
  public void put( Object item ) {
    this.delegate.add( item );
  }

  /**
   * Gets and removes the item at the beginning of the queue
   *
   * @return  The item at the beginning of the queue.
   */
  public Object get( ) throws EmptyQueueException {
    try {
      return this.delegate.remove( 0 );
    }
    catch( ArrayIndexOutOfBoundsException e ) {
      throw new EmptyQueueException();
    }
  }

  /**
   * Tests if the queue is empty
   *
   * @return  True if the queue is empty, false otherwise.
   */
  public boolean isEmpty() {
    return this.delegate.isEmpty();
  }

  /**
   * Clears this queue, making it empty.
   */
  public void clear() {
    this.delegate.clear();
  }
  
  
  /**
   * Tests if the item is in the queue. Because the Queue allows duplicate
   * items, isQueued() will return true even if the item was removed from
   * the queue if the item was queued twice before being removed from the queue.
   *
   * @return  True if the item is in the queue, false otherwise.
   */
  public boolean isQueued( Object item ) {
    return this.delegate.contains( item );
  }

  /**
   * Returns a String representation of the queue. This simply calls
   * the toString() method of the delegate, which is an <tt>ArrayList</tt>.
   */
  public String toString() {
    return this.delegate.toString();
  }
}
