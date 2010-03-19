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

package org.cs3.pdt.core;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.ui.util.DefaultErrorMessageProvider;
import org.cs3.pdt.ui.util.ErrorMessageProvider;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class PDTCorePlugin extends AbstractUIPlugin {
	private static PDTCorePlugin plugin;
	
	public PDTCorePlugin() {
		super();
		plugin = this;
		try {
			ResourceBundle.getBundle("prg.cs3.pdt.PDTPluginResources");
		} catch (MissingResourceException x) {
		}
	}
	
	/**
	 * Returns the shared instance.
	 */
	public static PDTCorePlugin getDefault() {
		return plugin;
	}	
	private void projectClosing(IProject project) {
		try {
			IPrologProject prologProject = PDTCoreUtils
					.getPrologProject(project);
			if (prologProject != null) {
				PrologInterfaceRegistry r = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
				r.removeSubscription(prologProject.getMetadataSubscription());
				r.removeSubscription(prologProject.getRuntimeSubscription());
			}
		} catch (CoreException e) {
			;
		}
	}

	private void projectOpened(IProject project) {
		try {
			IPrologProject prologProject = PDTCoreUtils
					.getPrologProject(project);
			if (prologProject != null) {
				PrologInterfaceRegistry r = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
				r.addSubscription(prologProject.getMetadataSubscription());
				r.addSubscription(prologProject.getRuntimeSubscription());
			}
		} catch (CoreException e) {
			;		
		}
	}

	private final class _ResourceDeltaVisitor implements IResourceDeltaVisitor {

		public _ResourceDeltaVisitor(IResourceChangeEvent event) {
			;
		}

		public boolean visit(IResourceDelta delta) throws CoreException {
			switch (delta.getResource().getType()) {
			case IResource.ROOT:
				return true;
			case IResource.PROJECT:
				IProject project = (IProject) delta.getResource();
				
					if (0 < (delta.getFlags() & IResourceDelta.OPEN)
							&& project.isOpen()){
						projectOpened(project);
					}
				return false;
			default:
				return false;
			}
		}
	}

	private final class _ResourceChangeListener implements
			IResourceChangeListener {
		public void resourceChanged(IResourceChangeEvent event) {
			IResourceDelta delta = event.getDelta();
			try {
				if(event.getType()==IResourceChangeEvent.POST_CHANGE&&delta!=null){
					delta.accept(new _ResourceDeltaVisitor(event));	
				}
				else if (event.getType()==IResourceChangeEvent.PRE_CLOSE){
					IProject project = (IProject) event.getResource();
					projectClosing(project);
				}
			} catch (CoreException e) {
				;
			}

		}

	}

	private ErrorMessageProvider errorMessageProvider;

	public ErrorMessageProvider getErrorMessageProvider() {
		if (errorMessageProvider == null) {
			errorMessageProvider = new DefaultErrorMessageProvider(this);
		}
		return errorMessageProvider;
	}

	public void start(BundleContext context) throws Exception {
		super.start(context);
		IWorkspace ws = ResourcesPlugin.getWorkspace();
		ws.addResourceChangeListener(new _ResourceChangeListener());
	}

	/**
	 * look up a preference value.
	 * <p>
	 * will return user settings if available or default settings if not. If a
	 * system property with the given key is defined it will overrule any
	 * existing setting in the preference store. if the key is not defined, this
	 * method returns the given default..
	 * 
	 * @param key
	 * @return the value or specified default if no such key exists..
	 */
	public String getPreferenceValue(String key, String defaultValue) {

		IPreferencesService service = Platform.getPreferencesService();
		String qualifier = getBundle().getSymbolicName();
		String value = service.getString(qualifier, key, defaultValue, null);
		return System.getProperty(key, value);
	}


	public void reconfigure() {
		;
	}

}
