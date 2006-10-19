package salvo.jesus.util;

import java.util.*;

public class Collections {

  /**
   * Returns a subset of the specified collection whose elements match a given object
   * using a specified Comparator. Only elements in the collection that match the
   * given object compareTo using the specified comparator are returned in the subset.
   *
   * @param   c   Collection object from which a subset will be taken.
   * @param   compareTo   Object to which elements of the collection will be compared against.
   * @param   comparator  Comparator object determining how elements in the collection will
   *                      be compared to the compareTo object.
   * @return  Collection object that is a subset of the collection specified in the argument.
   */
  public static Set subset( Collection c, Object compareTo, Comparator comparator ) {
    Set       subset = new HashSet();
    Iterator  iterator = c.iterator();
    Object    currentobject;

    while( iterator.hasNext() ) {
      currentobject = iterator.next();
      if( comparator.compare( compareTo, currentobject ) == 0 )
        c.add( currentobject );
    }

    return subset;
  }

  /**
   * Returns true if the specified Collection has an object that matches
   * the argument object compareTo using the given Comparator. This therefore
   * does use obj1.equals( obj2 ) but uses comparator.compare( obj1, obj2 ).
   *
   * @param   c   Collection object from which a subset will be taken.
   * @param   compareTo   Object to which elements of the collection will be compared against.
   * @param   comparator  Comparator object determining how elements in the collection will
   *                      be compared to the compareTo object.
   * @return  boolean   True if the Collection has a matching object
   */
  public static Object contains( Collection c, Object compareTo, Comparator comparator ) {
    Set       subset = new HashSet();
    Iterator  iterator = c.iterator();
    Object    currentobject;

    while( iterator.hasNext() ) {
      currentobject = iterator.next();
      if( comparator.compare( compareTo, currentobject ) == 0 )
        return currentobject;
    }

    return null;
  }

} 
