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
package org.cs3.pl.prolog.internal.socket;

import java.io.File;
import java.io.IOException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceFactory;

/**
 */
public class Factory extends PrologInterfaceFactory {
	private static final String MAIN_PL = "main_socket.pl";

	public static final String SERVER_PL = "consult_server.pl";

	public static final String FKILL_EXE = "fkill.exe";

	private Option[] options;

	public Factory() {

		options = new Option[] {
				new SimpleOption(SocketPrologInterface.EXECUTABLE,
						"SWI-Prolog executable", "eg. xpce or /usr/bin/xpce",
						SimpleOption.FILE, guessExecutableName()),
				new SimpleOption(SocketPrologInterface.ENVIRONMENT,
								"Extra environment variables", "A comma-separated list of VARIABLE=VALUE pairs.",
								SimpleOption.STRING, guessEnvironmentVariables()),
						
				new SimpleOption(SocketPrologInterface.KILLCOMMAND,
								"command to kill processes", "eg. kill or /usr/bin/kill on most systems",
								SimpleOption.FILE, guessKillCommandName()),						
				new SimpleOption(
						SocketPrologInterface.STANDALONE,
						"stand-alone server",
						"If true, the PIF will not try to start and stop its own server process.",
						SimpleOption.FLAG, guessStandAlone()) {
					public boolean isVisible() {
						return false;
					}
				},
				new SimpleOption(SocketPrologInterface.HOST, "Server host",
						"The host the PIF server is listening on",
						SimpleOption.STRING, "localhost") {
					public boolean isVisible() {
						return false;
					}
				},
				new SimpleOption(SocketPrologInterface.PORT, "Server port",
						"The port the PIF server is listening on",
						SimpleOption.NUMBER, "9944") {
					public boolean isVisible() {
						return false;
					}
				},
				new SimpleOption(
						SocketPrologInterface.TIMEOUT,
						"Connect Timeout",
						"Maximum time in milliseconds to wait for the prolog process to come up.",
						SimpleOption.NUMBER, "15000"),
				new SimpleOption(
						SocketPrologInterface.USE_POOL,
						"Use session pooling",
						"If true, the PIF will try to pool and reuse disposed sessions to reduce connection overhead.",
						SimpleOption.FLAG, guessUsePool()),
				new SimpleOption(
						SocketPrologInterface.HIDE_PLWIN,
						"Hide plwin (windows only)",
						"Usefull for windows users who are tired of plwin windows cluttering their system tray."
								+ "\n Note: this only works with the plwin executable.",
						SimpleOption.FLAG, "true") };
	}

	private String guessKillCommandName() {		
		if(Util.isWindoze()){
			try {
				return getResourceLocator().resolve(Factory.FKILL_EXE).getCanonicalPath();
			} catch (IOException e) {
				Debug.report(e);
				return "kill";
			}
		}
		
		return "kill";
		
	}




	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologInterfaceFactory#create()
	 */
	public PrologInterface create() {
		ensureInstalled(SERVER_PL, Factory.class);
		ensureInstalled(MAIN_PL, Factory.class);
		if (Util.isWindoze()) {
			ensureInstalled(FKILL_EXE, Factory.class);
		}

		SocketPrologInterface pif = new SocketPrologInterface(this);
		pif.getBootstrapLibraries().add(
				Util.prologFileName(getResourceLocator().resolve(SERVER_PL)));
		// pif.getBootstrapLibraries().add(
		// Util.prologFileName(getResourceLocator().resolve(MAIN_PL)));
		pif.setStartAndStopStrategy(new SocketServerStartAndStopStrategy());
		for (int i = 0; i < options.length; i++) {
			pif.setOption(options[i].getId(), options[i].getDefault());
		}
		return pif;
	}

	/**
	 * @return
	 */
	private static String guessUsePool() {
		return "true";
	}

	/**
	 * @return
	 */
	private static String guessStandAlone() {
		return "false";
	}

	private String guessEnvironmentVariables() {
		if (Util.isMacOS()) {
			String home = System.getProperty("user.home");
			return "DISPLAY=:0.0, HOME="+home;
		}
		return "";
	}

	private String guessExecutableName() {

		if (Util.isWindoze()) {
			return "cmd.exe /c start /min plwin";
			// return "plwin";
		}
		return "xpce";
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
