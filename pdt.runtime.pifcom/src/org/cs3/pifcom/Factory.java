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
package org.cs3.pifcom;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceFactory;

/**
 */
public class Factory extends PrologInterfaceFactory {
	private Option[] options;

	public Factory() {

		options = new Option[] {
				new SimpleOption("prolog.executable", "SWI-Prolog executable",
						"eg. xpce or /usr/bin/xpce", SimpleOption.FILE,
						guessExecutableName()),
				new SimpleOption("prolog.environment",
						"Extra environment variables",
						"A comma-separated list of VARIABLE=VALUE pairs.",
						SimpleOption.STRING, guessEnvironmentVariables()),
				new SimpleOption(PrologInterface.FILE_SEARCH_PATH,
						"Prolog Library Path",
						"Will be passed to prolog via the -p command line option." +
						"The server will try to lookup the module library('pifcom_server').",
						SimpleOption.STRING, null){
					@Override
					public String getDefault() {
						return guessFileSearchPath("pdt.runtime.pifcom.codebase");
					}
				},

				new SimpleOption(
						"prolog.hide_plwin",
						"Hide plwin (windows only)",
						"Usefull for windows users who are tired of plwin windows cluttering their system tray."
								+ "\n Note: this only works with the plwin executable.",
						SimpleOption.FLAG, "true"),

				new SimpleOption(
						"pif.standalone",
						"stand-alone server",
						"If true, the PIF will not try to start and stop its own server process.",
						SimpleOption.FLAG, guessStandAlone()) {
					public boolean isVisible() {
						return false;
					}
				},
				new SimpleOption(
						"pif.host",
						"Server host",
						"The host the PIF server is listening on. Only used in standalone mode.",
						SimpleOption.STRING, "localhost") {
					public boolean isVisible() {
						return false;
					}
				},
				new SimpleOption(
						"pif.port",
						"Server port",
						"The port the PIF server is listening on. Only used in standalone mode.",
						SimpleOption.NUMBER, "9944") {
					public boolean isVisible() {
						return false;
					}
				} ,new SimpleOption(
						"pif.timeout",
						"Timeout",
						"Number of milliseconds to wait for the server to come up.",
						SimpleOption.NUMBER, "2000") {
					public boolean isVisible() {
						return false;
					}
				} 
				
		};

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologInterfaceFactory#create()
	 */
	public PrologInterface create() {
		PIFComPrologInterface pif = new PIFComPrologInterface(this);

		for (int i = 0; i < options.length; i++) {
			pif.setOption(options[i].getId(), System.getProperty(options[i].getId(),options[i].getDefault()));
		}
		return pif;
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologInterfaceFactory#create()
	 */
	public PrologInterface create(String name) {
		PIFComPrologInterface pif = new PIFComPrologInterface(this,name);

		for (int i = 0; i < options.length; i++) {
			pif.setOption(options[i].getId(), System.getProperty(options[i].getId(),options[i].getDefault()));
		}
		return pif;
	}

	/**
	 * @return
	 */
	private static String guessStandAlone() {
		return "false";
	}

	public static String guessEnvironmentVariables() {
		if (Util.isMacOS()) {
			String home = System.getProperty("user.home");
			return "DISPLAY=:0.0, HOME=" + home;
		}
		return "";
	}

	public static String guessExecutableName() {

		if (Util.isWindows()) {
			return "cmd.exe /c start \"cmdwindow\" /min "
					+ findWindowsExecutable();
			// return "plwin";
		}
		// return "xterm -e xpce"; // For Mac and Linux with console
		return findUnixExecutable();
	}

	/**
	 * @author Hasan Abdel Halim
	 * 
	 * Finds the current SWI-Prolog executable for UNIX/BSD-BASED OS
	 * @return
	 * @return the complete path of the executable otherwise it will return xpce
	 */
	public static String findUnixExecutable() {
		String default_exec = "xpce";
		String xpce = default_exec;

		// TODO shall we look for the env. variables as we do for Windows ?
		String[] appendPath = null;

		// Hack to resolve the issue of locating xpce in MacOS
		if (Util.isMacOS()) {
			appendPath = new String[1];
			appendPath[0] = "PATH=PATH:/opt/local/bin";
		}

		try {
			Process process = Runtime.getRuntime().exec(
					"which " + default_exec, appendPath);

			if (process == null)
				return null;

			BufferedReader br = new BufferedReader(new InputStreamReader(
					process.getInputStream()));
			String path = br.readLine();

			if (path == null || path.startsWith("no " + default_exec))
				return default_exec;

			xpce = path;

			return xpce;

		} catch (IOException e) {

			return default_exec;
		}
	}

	/**
	 * @author Hasan Abdel Halim
	 * 
	 * Finds the current SWI-Prolog executable for Windoze OS
	 * @return the complete path of the executable otherwise it will return
	 *         plwin
	 */
	public static String findWindowsExecutable() {
		String default_exec = "plwin";
		String plwin = default_exec;

		String path;
		try {

			Process process = Runtime.getRuntime().exec(
					"cmd.exe /c echo %PATH%");

			if (process == null)
				return default_exec;

			BufferedReader br = new BufferedReader(new InputStreamReader(
					process.getInputStream()));
			path = br.readLine();

			if (path == null)
				return default_exec;

			// TODO just search in case of executable was not found.
			String[] paths = Util.split(path, ";");
			File exeFile = null;

			for (int i = 0; i < paths.length; i++) {

				if (default_exec.indexOf(".exe") == -1)
					default_exec += ".exe";

				String currPath = paths[i] + "\\" + default_exec;
				exeFile = new File(currPath);

				if (exeFile.exists()) {
					plwin = "\"" + currPath + "\"";
					break;
				}
			}

			return plwin;

		} catch (IOException e) {

			return default_exec;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologInterfaceFactory#getOptions()
	 */
	public Option[] getOptions() {
		return options;
	}

}
