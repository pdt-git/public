package org.cs3.pl.prolog;

/**
 * A System initialization hook. These can be registered with the PrologInterface
 * to be called before any sessions can  be created by the application.
 * @author terra
 */
public interface InitHook {
	
	/**
	 * The callback method to be called once the PrologInterface (re)starts. Care should
	 * be taken not to dispose the initial session passed. All queries on this session
	 * <u>must</u> be explicitly closed, if any threads are started, before returning
	 * from onInit().
	 * 
	 * @param s the system initialization session
	 */
	
	public void onInit(PrologSession s);

}
