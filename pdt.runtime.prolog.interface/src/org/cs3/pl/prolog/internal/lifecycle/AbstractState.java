package org.cs3.pl.prolog.internal.lifecycle;

import java.util.HashMap;
import java.util.Iterator;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterfaceException;

public abstract class AbstractState implements State {

	protected final LifeCycle context;

	
	public boolean isUp() {	
		return false;
	}
	
	public boolean isDown() {	
		return false;
	}
	
	
	public PrologInterfaceException getError() {	
		return null;		
	}
	
	protected AbstractState(LifeCycle context) {
		this.context = context;

	}

	
	public State reset() {
	
		return this;
	}
	
	public void enter() {
	
		
	}
	
	public State addLifeCycleHook(LifeCycleHook hook, String id,
			String[] dependencies) {

		HashMap<String, LifeCycleHookWrapper> hooks = context.getHooks();
		if (id == null) {
			id = "<<" + hooks.size() + ">>";
		}
		if (dependencies == null) {
			dependencies = new String[0];
		}
		Debug.debug("requested to add hook: id=\"" + id + "\", dependencies=\""
				+ Util.prettyPrint(dependencies) + "\"");

		LifeCycleHookWrapper node = hooks.get(id);

		if (node == null) {
			Debug.debug("\t-> hook unknown, new wrapper created.");
			node = new LifeCycleHookWrapper(context,hook, id);

			hooks.put(id, node);

		} else {
			Debug
					.debug("\t-> hook exists, reusing wrapper, but adding hook code..");
			node.hooks.add(hook);

		}
		for (int i = 0; i < dependencies.length; i++) {
			LifeCycleHookWrapper dep = hooks
					.get(dependencies[i]);
			Debug.debug("\t-> looking up dependency \"" + dependencies[i]
					+ "\"");
			if (dep == null) {
				Debug.debug("\t\t-> hook unknown, new wrapper created.");
				dep = new LifeCycleHookWrapper(context,null, dependencies[i]);

				hooks.put(dependencies[i], dep);
			}
			dep.pre.add(node);
			node.post.add(dep);
			Debug.debug("\t-> edges added.");
		}

		return this;
	}

	
	public State error(Throwable e) {
		if(e instanceof PrologInterfaceException){
			return new ErrorState(context,(PrologInterfaceException) e);
		}
		return new ErrorState(context, new PrologInterfaceException(e));
	}

	
	public State workDone() {
		return this;
	}

	
	public State removeLifeCycleHook(String hookId) {
		HashMap<String, LifeCycleHookWrapper> hooks = context.getHooks();
		LifeCycleHookWrapper wrapper = hooks.get(hookId);
		if (wrapper != null) {
			removeWrapper(hooks, wrapper);
			wrapper.hooks.clear();// extra paranoia :-)
		}
		return this;
	}

	
	public State removeLifeCycleHook(LifeCycleHook hook, String hookId) {
		HashMap<String, LifeCycleHookWrapper> hooks = context.getHooks();
		LifeCycleHookWrapper wrapper = hooks.get(hookId);
		if (wrapper != null) {
			wrapper.hooks.remove(hook);
			if (wrapper.hooks.isEmpty()) {
				removeWrapper(hooks, wrapper);
			}
		}
		return this;
	}

	private void removeWrapper(HashMap<String, LifeCycleHookWrapper> hooks,
			LifeCycleHookWrapper wrapper) {
		hooks.remove(wrapper);
		for (Iterator<LifeCycleHookWrapper> it = wrapper.pre.iterator(); it.hasNext();) {
			LifeCycleHookWrapper elm = it.next();
			elm.post.remove(wrapper);
		}
		for (Iterator<LifeCycleHookWrapper> it = wrapper.post.iterator(); it.hasNext();) {
			LifeCycleHookWrapper elm = it.next();
			elm.pre.remove(wrapper);
		}
	}

	
	public State start() {
		return this;
	}

	
	public State stop() {
		return this;
	}

}
