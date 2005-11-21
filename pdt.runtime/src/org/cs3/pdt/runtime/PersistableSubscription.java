package org.cs3.pdt.runtime;

import java.util.Map;

/**
 * A Subscription that can be persisted.
 * 
 * PersistableSubscriptions can store/restore their complete state to/from a
 * key->value map. It is the responsibility of the implementing class to supply
 * adaequat implementations to saveState() and restoreState(Map).
 * 
 * implementing classes MUST be default-constructable.
 * 
 * @author lukas
 * 
 */
public interface PersistableSubscription extends Subscription {
	/**
	 * retrieve the bundle id of the hosting plugin.
	 * 
	 * when the subscription is restored, the host bundle needs to be known, so
	 * the correct classloader is used.
	 * 
	 * @return the bundle id of the plugin hosting the subscription.
	 */
	public abstract String getHostId();

	/**
	 * Restore from saved state.
	 * 
	 * This is called by the registry. Implementations should use this method to
	 * completely initialize their state according to the given parameter
	 * values. The framework assumes that the subscription is completely
	 * initialized after this call.
	 * 
	 * @param params
	 *            a map of parameternames and respective values. Key and value
	 *            type is String. This map contains the same data as the on that
	 *            was previously returned by saveState.
	 */
	public abstract void restoreState(Map params);

	/**
	 * save state.
	 * 
	 * This method is called by the framework before shutdown. Implementations
	 * should return a (modifyable!) map of parameter names and respective
	 * values. Key and value type MUST be String. The map should contain enough
	 * information to completely restore the state by calling restoreState(Map)
	 * on an uninitialized Instance.
	 * 
	 * @return the saved state.
	 */
	public abstract Map saveState();

	/**
	 * Check wether this subscription should be persisted on shutdown.
	 * 
	 * @return if true, the framework will save the state on shutdown using
	 *         saveState() and restore it at next startup using restoreState().
	 */
	public abstract boolean isPersistent();
}
