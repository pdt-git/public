/*
 * Created on 05.09.2004
 *
 */
package org.cs3.jlmp.extension;

/**
 * @author rho
 *
 */
public interface IJTransformerObserver {
    
    public final static int JT_FACTBASE_UPDATED = 1;
    public final static int JT_ENGINE_STARTUP = 2;
	public static final int JT_BUILD_ERROR = 3;

    /**
     * Interface for observers on JTransformer.
     * Depending on the update kind additional information
     * is provided in the info parameter:
     * <menu>
     * <li>IJTransformer#JT_FACTBASE_UPDATED:
     * The PrologClient will be passed as the first parameter,
     * the second is the updated project
     * </li>
     * <li>IJTransformer#JT_ENGINE_STARTUP:
     * The PrologClient will be passed as the first and only parameter
     * </li>
     * </menu>
     * @param kind 
     * @param info
     */
    public void update(int kind, Object[] info);
}
