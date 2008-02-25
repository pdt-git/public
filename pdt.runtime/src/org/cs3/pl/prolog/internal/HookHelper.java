/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

/*
 */
package org.cs3.pl.prolog.internal;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;
import java.util.Map.Entry;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.LifeCycleHook3;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;

/**
 */
public class HookHelper {
	PrologInterface pif;

	/**
	 * @param pif
	 */
	public HookHelper(PrologInterface pif) {
		super();
		this.pif = pif;
	}

	private final class StartupThread extends Thread {

		private StartupThread(String name) {
			super(name);
		}

		public void run() {
			HashMap<String, LifeCycleHookWrapper> cloned = null;
			HashSet<LifeCycleHookWrapper> done = new HashSet<LifeCycleHookWrapper>();
			synchronized (HookHelper.this) {
				/*
				 * why clone AND sync? ld: sync to make sure that hooks
				 * operation is serialized clone to make it possible to
				 * (un)register hooks in the hook methods (which do run on the
				 * same thread and are not blocked by the synchronized) - this
				 * would not be possible otherwise since the iterator would
				 * throw a ConcurrentModificationException.
				 */

				cloned = cloneHooks();
				
				for (LifeCycleHookWrapper h : cloned.values()) {
					h.afterInit(pif, done);
				}
				HashSet<LifeCycleHook3> clonedLateHooks = new HashSet<LifeCycleHook3>();
				synchronized (lateHooks) {
					clonedLateHooks.addAll(lateHooks);
					lateHooks.clear();
				}
				for (LifeCycleHook3 h : clonedLateHooks) {
					h.lateInit(pif);
				}
			}
		}

		public void start() {
			setDaemon(true);
			super.start();
		}
	}

	private StartupThread startupThread;
	

	private HashMap<String, LifeCycleHookWrapper> hooks = new HashMap<String, LifeCycleHookWrapper>();

	private HashSet<LifeCycleHook3> lateHooks = new HashSet<LifeCycleHook3>();

	private HashMap<String, LifeCycleHookWrapper> cloneHooks() {
		HashMap<String, LifeCycleHookWrapper> clone = new HashMap();
		synchronized (hooks) {
			for (Entry<String, LifeCycleHookWrapper> entry : hooks.entrySet()) {
				LifeCycleHookWrapper wrapper = new LifeCycleHookWrapper(entry
						.getValue());
				clone.put(entry.getKey(), wrapper);
			}
		}
		return clone;
	}

	public void addLifeCycleHook(LifeCycleHook hook, String id,
			String[] dependencies) {
		boolean isNew = false;
		synchronized (hooks) {

			if (id == null) {
				id = "<<" + hooks.size() + ">>";
			}
			if (dependencies == null) {
				dependencies = new String[0];
			}
			Debug.debug("requested to add hook: id=\"" + id
					+ "\", dependencies=\"" + Util.prettyPrint(dependencies)
					+ "\"");

			LifeCycleHookWrapper node = (LifeCycleHookWrapper) hooks.get(id);

			if (node == null) {
				Debug.debug("\t-> hook unknown, new wrapper created.");
				node = new LifeCycleHookWrapper(hook, id);

				hooks.put(id, node);
				isNew = true;
			} else {
				Debug
						.debug("\t-> hook exists, reusing wrapper, but adding hook code..");
				isNew = node.hooks.add(hook);

			}
			for (int i = 0; i < dependencies.length; i++) {
				LifeCycleHookWrapper dep = (LifeCycleHookWrapper) hooks
						.get(dependencies[i]);
				Debug.debug("\t-> looking up dependency \"" + dependencies[i]
						+ "\"");
				if (dep == null) {
					Debug.debug("\t\t-> hook unknown, new wrapper created.");
					dep = new LifeCycleHookWrapper(null, dependencies[i]);

					hooks.put(dependencies[i], dep);
				}
				dep.pre.add(node);
				node.post.add(dep);
				Debug.debug("\t-> edges added.");
			}
		}
		if (isNew && hook instanceof LifeCycleHook3) {
			synchronized (lateHooks) {
				if (pif.getState() == PrologInterface.UP
						&& !lateHooks.contains(hook)) {
					((LifeCycleHook3) hook).lateInit(pif);

				} else if (pif.getState() == PrologInterface.START_UP) {
					lateHooks.add((LifeCycleHook3) hook);
				}
			}

		}
	}

	public void removeLifeCycleHook(final String hookId) {
		synchronized (hooks) {
			LifeCycleHookWrapper h = (LifeCycleHookWrapper) hooks.get(hookId);
			if (h == null) {
				return;
			}
			hooks.remove(h);
			for (Iterator it = h.pre.iterator(); it.hasNext();) {
				LifeCycleHookWrapper elm = (LifeCycleHookWrapper) it.next();
				elm.post.remove(h);
			}
			for (Iterator it = h.post.iterator(); it.hasNext();) {
				LifeCycleHookWrapper elm = (LifeCycleHookWrapper) it.next();
				elm.pre.remove(h);
			}
			h.hooks.clear();// extra paranoia :-)
		}
	}

	public void removeLifeCycleHook(final LifeCycleHook hook,
			final String hookId) {
		synchronized (hooks) {
			LifeCycleHookWrapper wrapper = (LifeCycleHookWrapper) hooks
					.get(hookId);
			if (wrapper == null) {
				return;
			}
			wrapper.hooks.remove(hook);
			if (wrapper.hooks.isEmpty()) {
				hooks.remove(wrapper);
				for (Iterator it = wrapper.pre.iterator(); it.hasNext();) {
					LifeCycleHookWrapper elm = (LifeCycleHookWrapper) it.next();
					elm.post.remove(wrapper);
				}
				for (Iterator it = wrapper.post.iterator(); it.hasNext();) {
					LifeCycleHookWrapper elm = (LifeCycleHookWrapper) it.next();
					elm.pre.remove(wrapper);
				}
			}
		}
	}

	public void onInit(PrologSession initSession) {
		HashMap<String, LifeCycleHookWrapper> cloned = null;
		synchronized (this) {

			cloned = cloneHooks();
			/*
			 * why clone AND sync? ld: sync to make sure that hooks operation is
			 * serialized clone to make it possible to (un)register hooks in the
			 * hook methods (which do run on the same thread and are not blocked
			 * by the synchronized) - this would not be possible otherwise since
			 * the iterator would throw a ConcurrentModificationException.
			 */

			HashSet<LifeCycleHookWrapper> done = new HashSet<LifeCycleHookWrapper>();
			for (LifeCycleHookWrapper h : cloned.values()) {
				h.onInit(pif, initSession, done);
			}
		}
	}

	public void afterInit() {
		startupThread = new StartupThread("PrologInterface startup Thread");
		startupThread.start();
	}

	public void beforeShutdown(PrologSession s) {

		// ld:gotta get in to get out
		if (startupThread != null) {
			try {
				startupThread.join();
			} catch (InterruptedException e) {
				Debug.rethrow(e);
			}
		}

		if (s != null) {
			HashSet<LifeCycleHookWrapper> done = new HashSet<LifeCycleHookWrapper>();
			HashMap<String, LifeCycleHookWrapper> cloned = null;
			synchronized (this) {

				cloned = cloneHooks();

				for (String id : cloned.keySet()) {

					LifeCycleHookWrapper h = (LifeCycleHookWrapper) cloned
							.get(id);

					try {
						h.beforeShutdown(pif, s, done);
					} catch (Throwable t) {
						Debug
								.error("could not execute 'beforeShutdown' on hook '"
										+ id + "'");
						Debug.report(t);
					}

				}
			}

		}
	}

	public void onError(AbstractPrologInterface interface1) {
		HashMap<String, LifeCycleHookWrapper> cloned = null;
		synchronized (this) {

			cloned = cloneHooks();

			for (LifeCycleHookWrapper h : cloned.values()) {
				h.onError(pif);
			}
		}
	}
}
