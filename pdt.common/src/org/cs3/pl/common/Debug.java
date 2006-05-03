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

import java.io.PrintStream;
import java.io.PrintWriter;
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
 * <li>INFO - Informational Messages about program operation should be printed
 * </li>
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

    static private PrintStream out = System.err;

    static public void setDebugLevel(String s) {

        out.println("initial debug level: " + s);
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
            error("Invalid debug level specified");
        }

    }

    /**
     * sets the level of reporting to the specified constant. Only the LEVEL_*
     * constants declared in this class are valid argument.
     * 
     * @param level
     *                    a LEVEL_* constant
     */

    static public void setDebugLevel(int level) {
        if (level < LEVEL_NONE || level > LEVEL_DEBUG)
            throw new IllegalArgumentException("Bad LEVEL");
        debugLevel = level;
    }

    /**
     * returns to current verbosity of debug output. Returns the value of one of
     * the constants defined in this class.
     * 
     * @return a value from LEVEL_NONE to LEVEL_DEBUG
     */

    static public int getDebugLevel() {
        return debugLevel;
    }

    /**
     * Sends an informational message to be considered for output. It is printed
     * if the current debug level is equal or greater to LEVEL_INFO
     * 
     * @param msg
     *                    the message
     */

    public static void info(String msg) {
        write(LEVEL_INFO, msg);
    }

    /**
     * Sends an warning message to be considered for output. It is printed if
     * the current debug level is equal or greater to LEVEL_WARNING
     * 
     * @param msg
     *                    the message
     */

    public static void warning(String msg) {
        write(LEVEL_WARNING, msg);
    }

    /**
     * sends a debug-level Message to be considered for output. It is printed if
     * the current debug level is equal to LEVEL_DEBUG
     * 
     * @param msg
     *                    the message
     */

    public static void debug(String msg) {
        write(LEVEL_DEBUG, msg);
    }

    /**
     * Sends an error message gto be considered for output. It is printed if the
     * current debug level is not set to LEVEL_NONE
     * 
     * @param msg
     *                    the message
     */

    public static void error(String msg) {
        write(LEVEL_ERROR, msg);
    }

    /**
     * reports an Error or an Exception. Exceptions are treated as
     * Warning-level, while Errors are considered just that, Errors.
     * 
     * @param t
     *                    a throwable object
     */    
    
    public static void report(Throwable t) {
        if (debugLevel == LEVEL_NONE)
            return;
        if (t instanceof Error && debugLevel != LEVEL_NONE) {
            write(LEVEL_ERROR, "The following Error was caught:");
            t.printStackTrace(out);
        } else if (debugLevel >= LEVEL_WARNING) {
            write(LEVEL_WARNING, "The following Exception was caught:");
            t.printStackTrace(out);
        }
    }

    
    /**
     * redirects debug output to another PrintStream. The parameter stream may
     * not be null, and must not be closed by an external class. Use this method
     * to save debug output to a file.
     * 
     * @param out
     *                    the new output stream
     */

    public static void setOutputStream(PrintStream out) {
        if (out == null)
            throw new IllegalArgumentException("null invalid");
        Debug.out = out;
        out.println("\n---8<------------------------8<---\n");
        out.println(new Date());
        out.println("\n---8<------------------------8<---\n");
    }

	public static PrintStream getOutputStream(){
		return out;
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
            throw new IllegalStateException("Bad level value");
        }
        Thread currentThread = Thread.currentThread();
		String tn = currentThread.getName();
        //StackTraceElement stackFrame = new Throwable().getStackTrace()[2];//currentThread.getStackTrace()[4];
		//String loc = "("+stackFrame.getFileName()+":"+stackFrame.getLineNumber()+")";
        //String loc = stackFrame.toString();
        Date d = new Date();
        out.println(prefix+":"+tn+": "/*+ loc+": "*/+ msg);
        out.flush();

    }
    public static void dumpStackTrace() {
        try{
            throw new RuntimeException("just to produce a stack trace");
        }catch(RuntimeException e){                               
            Debug.report(e);    
        }
    }

    public static class _RuntimeException extends RuntimeException{

		private Throwable cause;
		private String message;

		public _RuntimeException(Throwable e) {
			this.cause = e;
			message="wrapped exception";
		}

		public _RuntimeException(String message,Throwable e) {
			this.cause = e;
			this.message = message;
		}
		
		public _RuntimeException() {
			cause=new RuntimeException();
			message="wrapped exception";
		}

		public _RuntimeException(String message) {
			cause=new RuntimeException();
			this.message=message;
		}

		public Throwable fillInStackTrace() {
			if(cause!=null){
				cause.fillInStackTrace();
			}
			return this;
		}

		public String getLocalizedMessage() {
			return message + " ("+cause.getLocalizedMessage()+")";
		}

		public String getMessage() {
			return message + " ("+cause.getMessage()+")";
		}

		public void printStackTrace() {
			cause.printStackTrace();
		}

		public void printStackTrace(PrintStream arg0) {
			cause.printStackTrace(arg0);
		}

		public void printStackTrace(PrintWriter arg0) {
			cause.printStackTrace(arg0);
		}

		public String toString() {
			return cause.toString();
		}
    	
    }
	public static void rethrow(String string, Throwable e) {
		warning("wrapping an exception:"+string);
		report(e);
		throw new _RuntimeException(string, e);
		
	}
	
	public static void rethrow( Throwable e) {
		warning("wrapping an exception");
		report(e);
		throw new _RuntimeException(e);
		
	}
	
}
