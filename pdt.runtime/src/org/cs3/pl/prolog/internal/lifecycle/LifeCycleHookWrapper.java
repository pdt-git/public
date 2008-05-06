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

package org.cs3.pl.prolog.internal.lifecycle;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Vector;
import java.util.concurrent.BlockingQueue;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.LifeCycleHook2;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;

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
				
				public void run() throws PrologInterfaceException {
					hook.beforeShutdown(context.getPrologInterface(),context.getShutdownSession());
				}

			});
		}

	}

	public void onError() {

		for (final LifeCycleHook hook : hooks) {
			if (hook instanceof LifeCycleHook2) {
				context.enqueueWork(new NamedWorkRunnable("onError_on_"+id) {
					
					public void run() throws PrologInterfaceException {
						((LifeCycleHook2) hook).onError(context.getPrologInterface());
					}
				});
			}
		}
	}
}
