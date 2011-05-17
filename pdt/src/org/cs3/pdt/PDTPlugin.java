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

package org.cs3.pdt;

import java.io.FileNotFoundException;

import org.cs3.pdt.ui.util.DefaultErrorMessageProvider;
import org.cs3.pdt.ui.util.ErrorMessageProvider;
import org.cs3.pl.common.Debug;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class PDTPlugin extends AbstractUIPlugin implements IStartup{

	public static final String MODULEPREFIX = "pdtplugin:";

	// The shared instance.
	private static PDTPlugin plugin;

	/**
	 * Returns the shared instance.
	 */
	public static PDTPlugin getDefault() {
		return plugin;
	}

	private DefaultErrorMessageProvider errorMessageProvider;

	/**
	 * The constructor.
	 */
	public PDTPlugin() {
		super();
		plugin = this;
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

	public void setPreferenceValue(String key, String value) {
		getPreferenceStore().setValue(key, value);
	}

	/**
	 * 
	 */
	public void reconfigure() {
		try {
			reconfigureDebugOutput();

		} catch (Throwable e) {
			Debug.report(e);
		}
	}

	private void reconfigureDebugOutput() throws FileNotFoundException {
		String debugLevel = getPreferenceValue(PDT.PREF_DEBUG_LEVEL, "WARNING");
		String debugOutputTo = getPreferenceValue(PDT.PREF_DEBUG_OUTPUT_TO, "LOGFILE");
		String logFileName = getPreferenceValue(PDT.PREF_CLIENT_LOG_FILE_DIR, System.getProperty("java.io.tmpdir"));
		
		Debug.setDebugLevel(debugLevel);
		Debug.setLogDir(logFileName);	
		Debug.setOutputTo(debugOutputTo);
		
		
	}

	/**
	 * This method is called upon plug-in activation
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		try {
			super.start(context);
			reconfigureDebugOutput();
			IPropertyChangeListener debugPropertyChangeListener = new IPropertyChangeListener() {
				@Override
				public void propertyChange(PropertyChangeEvent e) {
					try {
						PDTPlugin.getDefault().reconfigureDebugOutput();
					} catch (FileNotFoundException e1) {
						Debug.report(e1);
					}
				}

			};	
			getPreferenceStore().addPropertyChangeListener(debugPropertyChangeListener);
		} catch (Throwable t) {
			Debug.report(t);
		}
	}

	public String getId() {
		return getBundle().getSymbolicName();
	}

	public ErrorMessageProvider getErrorMessageProvider() {
		if (errorMessageProvider == null) {
			errorMessageProvider = new DefaultErrorMessageProvider(this);
		};
		return errorMessageProvider;
	}

	public static IWorkbenchPage getActivePage() {
		final IWorkbenchWindow activeWorkbenchWindow = PlatformUI
			.getWorkbench().getActiveWorkbenchWindow();
		if (activeWorkbenchWindow == null) {
				return null;
		}
		return  activeWorkbenchWindow.getActivePage();
	}
	

	@Override
	public void earlyStartup() {
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

}