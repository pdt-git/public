package salvo.jesus.util;

/**
 * Thrown by methods on the Queue class to indicate that the Queue is empty
 * 
 * @author  Jesus M. Salvo Jr.
 */
public class EmptyQueueException extends Exception {

  /**
   * Creates a new EmptyQueueException will null as its error message.
   */
  public EmptyQueueException() {
    super();
  }
}