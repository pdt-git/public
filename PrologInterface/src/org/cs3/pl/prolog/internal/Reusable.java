package org.cs3.pl.prolog.internal;

/**
 * marks an object as being reusable.
 * 
 * Lifcycle of an reusable object looks like this:
 * Once the instance is not used any more, recycle() is 
 * called on it.
 * This method should hand over the Reusable instance with some kind of instance
 * pool, e.g. PrologSessionPool. After that there the using code should take 
 *   care to not further reference the instance.
 * <p>
 * the pool will eventualy decide about the future of the object and either call
 * destroy() or reuse().
 *
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
