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

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.cs3.pdt.ui.util.DefaultErrorMessageProvider;
import org.cs3.pdt.ui.util.ErrorMessageProvider;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.ui.plugin.AbstractUIPlugin;

public class PDTCorePlugin extends AbstractUIPlugin {

	private ErrorMessageProvider errorMessageProvider;

	public ErrorMessageProvider getErrorMessageProvider() {
		if(errorMessageProvider==null){
			errorMessageProvider=new DefaultErrorMessageProvider(this);
		}
		return errorMessageProvider;
	}
	
	private ResourceBundle resourceBundle;

	private Option[] options;

	// public IMetaInfoProvider getMetaInfoProvider() {
	// if (prologHelper == null) {
	// prologHelper = new
	// DefaultMetaInfoProvider(PrologRuntimePlugin.getDefault().getPrologInterface(),
	// pdtModulePrefix);
	// }
	// return prologHelper;
	// }
	public Option[] getOptions() {
		if (options == null) {
			initOptions();
		}
		return this.options;
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

	private String getLocation() throws IOException {
		URL url = PDTCorePlugin.getDefault().getBundle().getEntry("/");
		String location = null;
		location = new File(Platform.asLocalURL(url).getFile())
				.getAbsolutePath();
		if (location.charAt(location.length() - 1) == File.separatorChar)
			location = location.substring(0, location.length() - 1);
		return location;
	}

	private String ensureDirExists(String value, String label) {
		if (value == null) {
			return label + "must not be null";
		}
		if (value.length() == 0) {
			return label + " must not be empty";
		}
		File f = new File(value);
		if (!f.isAbsolute()) {
			return label + " must be an absolute path";
		}
		if (!f.exists()) {
			if (!f.mkdirs()) {
				return "could not create " + label;
			}
			return "";
		}
		if (!f.isDirectory()) {
			return label + " exists, but is not a directory";
		}
		if (!f.canWrite()) {
			return label + " exists, but is not writable";
		}
		return "";
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
				new SimpleOption(
						PDTCore.PREF_METADATA_ENGINE_DIR,
						"Metadata Engine Dir",
						"Directory containing the prolog implementation of the meta data engine,",
						Option.DIR, location + fileSep + "engine") {

					public String validate(String value) {
						return ensureDirExists(value, "Metadata Engine Dir");
					}

				},
				new SimpleOption(PDTCore.PREF_METADATA_STORE_DIR,
						"Metadata Store Dir",
						"Directory used to store metadata for prolog files.",
						Option.DIR, location + fileSep + "store") {
					public String validate(String value) {
						return ensureDirExists(value, "Metadata Store Dir");
					}
				},
				new SimpleOption(
						PDTCore.PREF_SOURCE_PATH_DEFAULT,
						"Default Source Path",
						"The default value for the source path of prolog projects.",
						Option.STRING, "/"),
				new SimpleOption(
						PDTCore.PREF_METADATA_PIF_KEY_DEFAULT,
						"Default Meta Data PrologInterface",
						"The default value for the Metadata PrologInterface property of prolog projects.",
						Option.STRING, "%project%"),
				new SimpleOption(
						PDTCore.PREF_RUNTIME_PIF_KEY_DEFAULT,
						"Default Runtime PrologInterface",
						"The default value for the Runtime PrologInterface property of prolog projects.",
						Option.STRING, "%project%"),
				new SimpleOption(
						PDTCore.PREF_AUTO_CONSULT,
						"Enable Auto-Consult (EXPERIMENTAL)",
						"If this flag is set, the PDT will automaticaly (re-)consult any source file,"
								+ "unless it is explicitly exluded from Auto-Consult. Note that this is an experimental "
								+ "feature and defaults to \"false\" for 0.1.x",
						Option.FLAG, "false"),
						new SimpleOption(
								PDTCore.PREF_PARSER,
								"Parser Framework",
								"Determines which parser framework the PDT core uses. \n" +
								"\"javacc parser\" is a javacc generated parser. This is the parser" +
								"all versions before 0.2M5 were working with.\n" +
								"\"read_term/3 parser\" is implemented in prolog using the builtin read_term/3." +
								"It has several advantages compared to the traditional javacc parser, but it is" +
								"relatively new and needs further performance optimisation. ",
								Option.ENUM, PDTCore.JAVACC, new String[][] {
										{ "javacc parser", PDTCore.JAVACC}, 
										{ "read_term/3 parser", PDTCore.READ_TERM_3 }})

		};

	}

	/**
	 * 
	 */
	public void reconfigure() {
		;
	}

	public PDTCorePlugin() {
		super();
		plugin = this;
		try {
			resourceBundle = ResourceBundle
					.getBundle("prg.cs3.pdt.PDTPluginResources");
		} catch (MissingResourceException x) {
			resourceBundle = null;
		}
	}

	// The shared instance.
	private static PDTCorePlugin plugin;

	/**
	 * Returns the shared instance.
	 */
	public static PDTCorePlugin getDefault() {
		return plugin;
	}

}
