package org.cs3.pl.prolog;

/**
 * marks an object as being reusable.
 * 
 * Lifcycle of an reusable object looks like this:
 * Once the instance is not used any more, a special method, like dispose() is 
 * called on it.
 * This method should hand over the Reusable instance with some kind of instance
 * pool, e.g. PrologSessionPool. After that there the using code should take 
 *   care to not further reference the instance.
 * 
 * The pool will create a SoftReference to the instance, and register it with a special
 * queue. Once all strong references to the instance are gone, the gc will eventualy 
 * decide to enqueue the instance. The pool should poll this queue and once
 * it gets an instance decide wether it should be recycled or finaly destroyed. 
 */
public interface Reusable {
    /**
     * reuse an recycled instance.
     *
     *should reset the object's state so that it can be used by another instance.
     */    
    public void reuse();
    
    /**
     * destroy an recycled instance
     */
    public void destroy();
    
    public void recylce();
}
