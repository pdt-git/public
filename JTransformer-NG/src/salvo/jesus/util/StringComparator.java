package salvo.jesus.util;

import java.util.Comparator;
import java.io.*;

/**
 * A Comparator to compare the characters between two String objects.
 *
 * @author  Jesus M. Salvo Jr.
 */

public class StringComparator implements Comparator, Serializable {

  /**
   * Creates an instance of StringComparator
   */
  public StringComparator() {}

  /**
   * Compares two HeapNode objects.
   *
   * @param o1  Must be an instance of String
   * @param o2  Must be an instance of String
   */
  public int compare(Object o1, Object o2) {
    String  s1 = (String) o1;
    String  s2 = (String) o2;

    return( s1.compareTo( s2 ));
  }

  public boolean equals(Object obj) {
    return obj.equals( this );
  }
}
