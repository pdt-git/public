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

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URL;
import java.util.MissingResourceException;
import java.util.ResourceBundle;


import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.ui.util.DefaultErrorMessageProvider;
import org.cs3.pdt.ui.util.ErrorMessageProvider;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.dialogs.MessageDialogWithToggle;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class PDTPlugin extends AbstractUIPlugin  {

	public static final String MODULEPREFIX = "pdtplugin:";

	// The shared instance.
	private static PDTPlugin plugin;

	/**
	 * Returns the shared instance.
	 */
	public static PDTPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns the string from the plugin's resource bundle, or 'key' if not
	 * found.
	 */
	public static String getResourceString(String key) {
		ResourceBundle bundle = PDTPlugin.getDefault().getResourceBundle();
		try {
			return (bundle != null) ? bundle.getString(key) : key;
		} catch (MissingResourceException e) {
			return key;
		}
	}

	// Resource bundle.
	private ResourceBundle resourceBundle;

	private Object root;

	private DefaultResourceFileLocator rootLocator;

	private Option[] options;

	private DefaultErrorMessageProvider errorMessageProvider;

	/**
	 * The constructor.
	 */
	public PDTPlugin() {
		super();
		plugin = this;
		try {
			resourceBundle = ResourceBundle
					.getBundle("prg.cs3.pdt.PDTPluginResources");
		} catch (MissingResourceException x) {
			resourceBundle = null;
		}
	}

	public ResourceFileLocator getResourceLocator(String key) {
		if (rootLocator == null) {
			URL url = PDTPlugin.getDefault().getBundle().getEntry("/");
			File location = null;
			try {
				location = new File(Platform.asLocalURL(url).getFile());
			} catch (IOException t) {
				Debug.report(t);
				throw new RuntimeException(t);
			}

			rootLocator = new DefaultResourceFileLocator(location);
		}
		return rootLocator.subLocator(key);
	}

	/**
	 * Returns the plugin's resource bundle,
	 */
	public ResourceBundle getResourceBundle() {
		return resourceBundle;
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
		Debug.setDebugLevel(debugLevel);
		String logFileName = getPreferenceValue(PDT.PREF_CLIENT_LOG_FILE, null);
		if (logFileName != null && !logFileName.equals("")) {
			System.out.println("debug output is written to: " + logFileName);
			File logFile = new File(logFileName);
			BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(
					new FileOutputStream(logFile, true));
			Debug.setOutputStream(new PrintStream(bufferedOutputStream));
		} else {
			Debug.setOutputStream(System.err);
		}
	}

	/**
	 * This method is called upon plug-in activation
	 */
	public void start(BundleContext context) throws Exception {
		try {
			super.start(context);
			reconfigureDebugOutput();

		} catch (Throwable t) {
			Debug.report(t);
		}
	}

	public Option[] getOptions() {
		if (options == null) {
			initOptions();
		}
		return this.options;
	}

	/**
	 * 
	 */
	private void initOptions() {
		String fileSep = File.separator;
		String location = "";
		try {
			location = getLocation();
		} catch (IOException e) {
			Debug.report(e);
			Debug.error("Could not find plugin installation dir.");
		}

		options = new Option[] {
				new SimpleOption(PDT.PREF_DEBUG_LEVEL, "Debug Level",
						"Determines the verbosity of the debug log file.",
						Option.ENUM, "WARNING", new String[][] {
								{ "error", "ERROR" }, { "warning", "WARNING" },
								{ "info", "INFO" }, { "debug", "DEBUG" } }),
				new SimpleOption(
						PDT.PREF_CLIENT_LOG_FILE,
						"Log file location",
						"A file to which debug output of the PDT will be writen",
						Option.FILE, location + fileSep + "pdt.log"),
				new SimpleOption(
						PDT.PREF_ADD_NATURE_ON_OPEN,
						"Automatically add Prolog Nature when opening pl files",
						"When i open a file in the prolog editor that does not belong to " +
						"a prolog project, ask if i want to add the prolog nature.",
						Option.ENUM, MessageDialogWithToggle.PROMPT, new String[][] {
								{ "always", MessageDialogWithToggle.ALWAYS }, { "never", MessageDialogWithToggle.NEVER },
								{ "ask", MessageDialogWithToggle.PROMPT }}),
				new SimpleOption(
						PDT.PREF_SWITCH_TO_DEFAULT_PIF,
						"Switch to default runtime before consulting",
						"When i consult a prolog file, but the active console view is not connected to the default runtime" +
						"of the respective prolog project, should i switch to the default runtime first?",
						Option.ENUM, MessageDialogWithToggle.PROMPT, new String[][] {
								{ "always", MessageDialogWithToggle.ALWAYS }, { "never", MessageDialogWithToggle.NEVER },
								{ "ask", MessageDialogWithToggle.PROMPT }}),
				new SimpleOption(
						PDT.PREF_OUTLINE_FILTERS,
						"Active Filters for the Prolog Outline",
						"A comma separated list of filter ids that should be activated at startup",
						Option.STRING, "hide_subterms"
				){
					@Override
					public boolean isVisible() {
					
						return false;
					}
					
				},new SimpleOption(
						PDT.PREF_OUTLINE_SORT,
						"Whether the Prolog Outline is to be sorted lexicographical",
						"true or false",
						Option.FLAG, "false"
				){
					@Override
					public boolean isVisible() {
					
						return false;
					}
				}
				

		};

	}

	private String getLocation() throws IOException {
		URL url = PDTPlugin.getDefault().getBundle().getEntry("/");
		String location = null;
		location = new File(Platform.asLocalURL(url).getFile())
				.getAbsolutePath();
		if (location.charAt(location.length() - 1) == File.separatorChar)
			location = location.substring(0, location.length() - 1);
		return location;
	}

	
	public String getId() {		
		return getBundle().getSymbolicName();
	}

	public ErrorMessageProvider getErrorMessageProvider() {
		if(errorMessageProvider==null){
			errorMessageProvider=new DefaultErrorMessageProvider(this);
		}
		return errorMessageProvider;
	}

}