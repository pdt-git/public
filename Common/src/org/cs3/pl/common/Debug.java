package org.cs3.pl.common;

import java.io.PrintStream;

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
    }

    private static void write(int level, String msg) {
        if (level > debugLevel)
            return;
        String prefix;
        switch (level) {
        case LEVEL_DEBUG:
            prefix = "DEBUG: ";
            break;
        case LEVEL_WARNING:
            prefix = "WARNING: ";
            break;
        case LEVEL_ERROR:
            prefix = "ERROR: ";
            break;
        case LEVEL_INFO:
            prefix = "INFO: ";
            break;
        default:
            throw new IllegalStateException("Bad level value");
        }

        out.println(prefix + msg);
        out.flush();

    }

}
