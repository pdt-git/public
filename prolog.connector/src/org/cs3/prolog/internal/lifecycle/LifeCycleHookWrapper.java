/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.internal.lifecycle;

import java.util.HashSet;

import org.cs3.prolog.lifecycle.LifeCycleHook;
import org.cs3.prolog.pif.PrologInterfaceException;

public class LifeCycleHookWrapper {

	String id;

	/** things I depend on */
	public HashSet<LifeCycleHookWrapper> post = new HashSet<LifeCycleHookWrapper>();

	/** things that depend on me */
	public HashSet<LifeCycleHookWrapper> pre = new HashSet<LifeCycleHookWrapper>();

	// public LifeCycleHook hook;
	public HashSet<LifeCycleHook> hooks = new HashSet<LifeCycleHook>();

	private final LifeCycle context;

	public LifeCycleHookWrapper(LifeCycle context,LifeCycleHook hook, String id) {
		this.context = context;
		if (hook != null) {
			this.hooks.add(hook);
		}
		this.id = id;
	}

	public LifeCycleHookWrapper(LifeCycleHookWrapper value) {
		this.context = value.context;
		this.hooks = new HashSet<LifeCycleHook>(value.hooks);
		this.id = value.id;
		this.post = new HashSet<LifeCycleHookWrapper>(value.post);
		this.pre = new HashSet<LifeCycleHookWrapper>(value.pre);
	}

	public void onInit(HashSet<LifeCycleHookWrapper> done) {
		if (done.contains(this)) {
			return;
		}
		done.add(this);
		for (LifeCycleHookWrapper elm : post) {
			elm.onInit( done);

		}

		for (final LifeCycleHook hook : hooks) {
			context.enqueueWork(new NamedWorkRunnable("onInit_on_"+id) {
				
				@Override
				public void run() throws PrologInterfaceException {
					hook.onInit(context.getPrologInterface(), context.getInitialSession());
				}

			});
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
	 */
	public void afterInit(HashSet<LifeCycleHookWrapper> done) {
		if (done.contains(this)) {
			return;
		}
		done.add(this);
		for (LifeCycleHookWrapper elm : post) {
			elm.afterInit( done);
		}

		for (final LifeCycleHook hook : hooks) {
			context.enqueueWork(new NamedWorkRunnable("afterInit_on_"+id) {
				
				@Override
				public void run() throws PrologInterfaceException {
					hook.afterInit(context.getPrologInterface());
				}

			});
		}

	}

	public void beforeShutdown(HashSet<LifeCycleHookWrapper> done) {
		if (done.contains(this)) {
			return;
		}
		done.add(this);
		for (LifeCycleHookWrapper elm : pre) {
			elm.beforeShutdown( done);
		}

		for (final LifeCycleHook hook : hooks) {
			context.enqueueWork(new NamedWorkRunnable("beforeShutdown_on_"+id) {
				
				@Override
				public void run() throws PrologInterfaceException {
					hook.beforeShutdown(context.getPrologInterface(),context.getShutdownSession());
				}

			});
		}

	}

	public void onError() {
		for (final LifeCycleHook hook : hooks) {
			context.enqueueWork(new NamedWorkRunnable("onError_on_"+id) {
				@Override
				public void run() throws PrologInterfaceException {
					hook.onError(context.getPrologInterface());
				}
			});
		}
	}
}

