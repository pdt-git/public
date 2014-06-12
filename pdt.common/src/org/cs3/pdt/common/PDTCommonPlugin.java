/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.cs3.pdt.common.internal.ConsultManager;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.registry.PrologInterfaceRegistry;
import org.cs3.pdt.connector.registry.PrologInterfaceRegistryEvent;
import org.cs3.pdt.connector.registry.PrologInterfaceRegistryListener;
import org.cs3.prolog.connector.common.logging.Debug;
import org.cs3.prolog.connector.lifecycle.LifeCycleHook;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologInterfaceException;
import org.cs3.prolog.connector.session.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class PDTCommonPlugin extends AbstractUIPlugin implements BundleActivator {

	public static final QualifiedName ENTRY_POINT_KEY = new QualifiedName("pdt", "entry.point");
	
	private static BundleContext context;

	private static PDTCommonPlugin plugin;
	
	public static final String PLUGIN_ID = "org.cs3.pdt.common";
	
	public static final String LIFE_CYCLE_HOOK_ID = "org.cs3.pdt.common.lifecycle.hook";
	private static final String[] EMPTY_STRING_ARRAY = new String[0];
	private LifeCycleHook lifeCycleHook;
	
	public PDTCommonPlugin() {
		super();
		plugin = this;
	}
	
	static BundleContext getContext() {
		return context;
	}
	
	/**
	 * Returns the shared instance.
	 */
	public static PDTCommonPlugin getDefault() {
		return plugin;
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public void stop(BundleContext bundleContext) throws Exception {
		super.stop(bundleContext);
	}
	
	
	@Override
	public void start(BundleContext bundleContext) throws Exception{
		super.start(bundleContext);
		reconfigureDebugOutput();
		IPropertyChangeListener debugPropertyChangeListener = new IPropertyChangeListener() {
			@Override
			public void propertyChange(PropertyChangeEvent e) {
				try {
					if (!"console.no.focus".equals(e.getProperty())) {
						PDTCommonPlugin.this.reconfigureDebugOutput();
					}
				} catch (FileNotFoundException e1) {
					Debug.report(e1);
				}
			}

		};	
		getPreferenceStore().addPropertyChangeListener(debugPropertyChangeListener);
		lifeCycleHook = new LifeCycleHook() {
			@Override public void setData(Object data) {}
			@Override public void onInit(PrologProcess pif, PrologSession initSession) throws PrologInterfaceException {}
			@Override public void onError(PrologProcess pif) {}
			@Override public void lateInit(PrologProcess pif) {}
			@Override public void beforeShutdown(PrologProcess pif, PrologSession session) throws PrologInterfaceException {}
			
			@Override
			public void afterInit(PrologProcess pif) throws PrologInterfaceException {
				PDTCommonPlugin.this.notifyPifStartHooks(pif);
			}
		};
		PrologInterfaceRegistry registry = PDTConnectorPlugin.getDefault().getPrologInterfaceRegistry();
		registry.addPrologInterfaceRegistryListener(new PrologInterfaceRegistryListener() {
			@Override public void subscriptionRemoved(PrologInterfaceRegistryEvent e) {}
			@Override public void subscriptionAdded(PrologInterfaceRegistryEvent e) {}
			@Override public void prologInterfaceRemoved(PrologInterfaceRegistryEvent e) {}
			
			@Override
			public void prologInterfaceAdded(PrologInterfaceRegistryEvent e) {
				e.pif.addLifeCycleHook(lifeCycleHook, LIFE_CYCLE_HOOK_ID, EMPTY_STRING_ARRAY);
			}
		});
		for (String key : registry.getRegisteredKeys()) {
			registry.getPrologProcess(key).addLifeCycleHook(lifeCycleHook, LIFE_CYCLE_HOOK_ID, EMPTY_STRING_ARRAY);
		}
		ConsultManager consultManager = new ConsultManager();
		registerPifStartListener(consultManager);
		PDTConnectorPlugin.getDefault().getPrologInterfaceService().registerConsultListener(consultManager);
	}
	
	private void reconfigureDebugOutput() throws FileNotFoundException {
		String debugLevel = getPreferenceValue(PDTCommon.PREF_DEBUG_LEVEL, "WARNING");
		String debugOutputTo = getPreferenceValue(PDTCommon.PREF_DEBUG_OUTPUT_TO, "LOGFILE");
		String logFileName = getPreferenceValue(PDTCommon.PREF_CLIENT_LOG_FILE_DIR, System.getProperty("java.io.tmpdir"));
		
		Debug.setDebugLevel(debugLevel);
		Debug.setLogDir(logFileName);	
		Debug.setOutputTo(debugOutputTo);
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

	/**
	 * Returns a section in the Prolog plugin's dialog settings. If the section doesn't exist yet, it is created.
	 *
	 * @param name the name of the section
	 * @return the section of the given name
	 */
	public IDialogSettings getDialogSettingsSection(String name) {
		IDialogSettings dialogSettings= getDialogSettings();
		IDialogSettings section= dialogSettings.getSection(name);
		if (section == null) {
			section= dialogSettings.addNewSection(name);
		}
		return section;
	}
	
	/* 
	 * entry point handling
	 */
	private Set<IFile> entryPoints;

	public void addEntryPoint(IFile f) {
		collectEntryPointsIfNeeded();
		entryPoints.add(f);
	}

	public void removeEntryPoint(IFile f) {
		collectEntryPointsIfNeeded();
		entryPoints.remove(f);
	}

	public Set<IFile> getEntryPoints() {
		collectEntryPointsIfNeeded();
		return entryPoints;
	}

	private void collectEntryPointsIfNeeded() {
		if (entryPoints == null) {
			entryPoints = new HashSet<IFile>();
			try {
				ResourcesPlugin.getWorkspace().getRoot().accept(new IResourceVisitor() {
					@Override
					public boolean visit(IResource resource) throws CoreException {
						if (resource instanceof IFile) {
							IFile file = (IFile) resource;
							if ("true".equalsIgnoreCase(file.getPersistentProperty(ENTRY_POINT_KEY))) {
								entryPoints.add(file);
							}
						}
						return true;
					}
				});
			} catch (CoreException e) {
				Debug.report(e);
			}
		}
	}

	private Set<PDTDecorator> decorators = new HashSet<>();
	
	public void addDecorator(PDTDecorator decorator) {
		decorators.add(decorator);
	}
	
	public void removeDecorator(PDTDecorator decorator) {
		decorators.remove(decorator);
	}
	
	public void notifyDecorators() {
		for (PDTDecorator d : decorators) {
			d.updateDecorator();
		}
	}
	
	private Set<PrologInterfaceStartListener> pifStartListeners = new HashSet<PrologInterfaceStartListener>();

	public void registerPifStartListener(PrologInterfaceStartListener listener) {
		synchronized (pifStartListeners) {
			pifStartListeners.add(listener);
		}
	}

	public void unregisterPifStartListener(PrologInterfaceStartListener listener) {
		synchronized (pifStartListeners) {
			pifStartListeners.remove(listener);
		}
	}
	
	private void notifyPifStartHooks(PrologProcess pif) {
		ArrayList<PrologInterfaceStartListener> listeners;
		synchronized (pifStartListeners) {
			listeners = new ArrayList<PrologInterfaceStartListener>(pifStartListeners);
		}
		for (PrologInterfaceStartListener listener : listeners) {
			listener.prologProcessStarted(pif);
		}
	}

}


