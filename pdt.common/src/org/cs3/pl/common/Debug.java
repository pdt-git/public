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

package org.cs3.pl.common;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.Date;

/**
 * Provides a application-wide mechanism to send debug, or other informational
 * Messages. Unless specified otherwise, all Messages will be send to the
 * standart error stream. This class examines the System Property
 * <u>org.cs3.pl.jtransformer.debug_level </u> which should take one of the
 * following values:
 * <ul>
 * <li>NONE - No console output at all</li>
 * <li>ERROR - Only serious errors should be reported</li>
 * <li>WARNING - Less serious error conditions should be reported</li>
 * <li>INFO - Informational Messages about program operation should be printed</li>
 * <li>DEBUG - Insanely verbose debug output.</li>
 * </ul>
 */
public class Debug {
	final public static int LEVEL_NONE = 0;
	final public static int LEVEL_ERROR = 1;
	final public static int LEVEL_WARNING = 2;
	final public static int LEVEL_INFO = 3;
	final public static int LEVEL_DEBUG = 4;
	static private int debugLevel = LEVEL_ERROR;	
	private static String PREFIX_OUTPUT = "pdt: ";
	static private String outputDir;
	static private PrintStream outputLogFilePrintStream; 
	static private PrintStream out = System.err;

	static public void setDebugLevel(String s) {
		System.out.println(PREFIX_OUTPUT + "set debug level: " + s);
		if (s.equalsIgnoreCase("NONE"))
			debugLevel = LEVEL_NONE;
		else if (s.equalsIgnoreCase("ERROR"))
			debugLevel = LEVEL_ERROR;
		else if (s.equalsIgnoreCase("WARNING"))
			debugLevel = LEVEL_WARNING;
		else if (s.equalsIgnoreCase("INFO"))
			debugLevel = LEVEL_INFO;
		else if (s.equalsIgnoreCase("DEBUG"))
			debugLevel = LEVEL_DEBUG;
		else {
			debugLevel = LEVEL_ERROR;
			error("Invalid debug level specified, set debug level to ERROR");
		}
	}

	static public void setOutputTo(String output) {		
		if (output.equalsIgnoreCase("LOGFILE") ) {
			if (outputLogFilePrintStream!=null) {
				setOutputStream(outputLogFilePrintStream);
				out.println(PREFIX_OUTPUT + "set debug output to " + output);
			} else {
				setOutputStream(System.err);
				out.println(PREFIX_OUTPUT + "set debug output NOT to "+ output+" because LogFileStream/File invalid, please check preferences");
				out.println(PREFIX_OUTPUT + "set debug output to CONSOLE");
			}
		}
		if (output.equalsIgnoreCase("CONSOLE")) {
			setOutputStream(System.err);
			out.println(PREFIX_OUTPUT + "set debug output to " + output);
		}
	}	
	
	static public void setLogDir(String logFileDir) throws FileNotFoundException {
		outputDir = logFileDir;
		if (outputDir != null && !outputDir.equals("")) {
			File logFile = new File(outputDir, "pdt.log");
			System.out.println(PREFIX_OUTPUT + "debug output is written to: " + logFile.getAbsolutePath());
			BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(new FileOutputStream(logFile, true));
			outputLogFilePrintStream = new PrintStream(bufferedOutputStream);
			setOutputTo("LOGFILE");
		} else {
			setOutputTo("CONSOLE");
		}
	}


	/**
	 * sets the level of reporting to the specified constant. Only the LEVEL_*
	 * constants declared in this class are valid argument.
	 * 
	 * @param level
	 *            a LEVEL_* constant
	 */
	static public void setDebugLevel(int level) {
		if (level < LEVEL_NONE || level > LEVEL_DEBUG)
			throw new IllegalArgumentException("Bad LEVEL");
		debugLevel = level;
	}

	/**
	 * Sends an informational message to be considered for output. It is printed
	 * if the current debug level is equal or greater to LEVEL_INFO
	 * 
	 * @param msg
	 *            the message
	 */
	public static void info(String msg) {
		write(LEVEL_INFO, msg);
	}

	/**
	 * Sends an warning message to be considered for output. It is printed if
	 * the current debug level is equal or greater to LEVEL_WARNING
	 * 
	 * @param msg
	 *            the message
	 */
	public static void warning(String msg) {
		write(LEVEL_WARNING,msg);
	}

	/**
	 * sends a debug-level Message to be considered for output. It is printed if
	 * the current debug level is equal to LEVEL_DEBUG
	 * 
	 * @param msg
	 *            the message
	 */
	public static void debug(String msg) {
		write(LEVEL_DEBUG, msg);
	}

	/**
	 * Sends an error message to be considered for output. It is printed if the
	 * current debug level is not set to LEVEL_NONE
	 * 
	 * @param msg
	 *            the message
	 */
	public static void error(String msg) {
		write(LEVEL_ERROR,msg);
	}

	/**
	 * reports an Error or an Exception. Exceptions are treated as
	 * Warning-level, while Errors are considered just that, Errors.
	 * 
	 * @param t
	 *            a throwable object
	 */
	public static void report(Throwable t) {
		if (debugLevel == LEVEL_NONE)
			return;
		if (t instanceof Error && debugLevel != LEVEL_NONE) {
			write(LEVEL_ERROR, PREFIX_OUTPUT + "The following Error was caught:");
			t.printStackTrace(out);
		} else if (debugLevel >= LEVEL_ERROR) {
			write(LEVEL_WARNING, PREFIX_OUTPUT + "The following Exception was caught:");
			t.printStackTrace(out);
		}
	}

	private static void setOutputStream(PrintStream out) {
		if (out == null)
			throw new IllegalArgumentException("pdt: output stream is null");
		Debug.out = out;
		out.println(PREFIX_OUTPUT + "----------------------------------");
		out.println(PREFIX_OUTPUT + new Date());
		out.println(PREFIX_OUTPUT + "----------------------------------");
	}

	private static void write(int level, String msg) {
		if (level > debugLevel)
			return;
		String prefix;
		switch (level) {
		case LEVEL_DEBUG:
			prefix = "DEBUG";
			break;
		case LEVEL_WARNING:
			prefix = "WARNING";
			break;
		case LEVEL_ERROR:
			prefix = "ERROR";
			break;
		case LEVEL_INFO:
			prefix = "INFO";
			break;
		default:
			throw new IllegalStateException(PREFIX_OUTPUT + "Bad level value");
		}
		Thread currentThread = Thread.currentThread();
		String nameOfCurrentThread = currentThread.getName();
		out.println(PREFIX_OUTPUT + prefix + ":" + nameOfCurrentThread + ": "+ msg);
		out.flush();	
	}

	public static void rethrow(String string, Throwable e) {
		warning("wrapping an exception:" + string);
		report(e);
		throw new RuntimeException(string, e);

	}

	public static void rethrow(Throwable e) {
		warning("wrapping an exception");
		report(e);
		throw new RuntimeException(e);

	}
}
