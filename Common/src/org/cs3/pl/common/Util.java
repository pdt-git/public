package org.cs3.pl.common;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.ServerSocket;
import java.util.Iterator;
import java.util.Map;

/**
 * contains static methods that do not quite fit anywhere else :-)=
 */
public class Util {
    public static boolean probePort(int port) {
        try {
            ServerSocket ss = new ServerSocket(port);
            ss.close();
        } catch (IOException e1) {
            return true;
        }
        return false;
    }

    public static String prettyPrint(Map r) {
        if (r != null) {
            StringBuffer sb = new StringBuffer();
            for (Iterator it = r.keySet().iterator(); it.hasNext();) {
                String key = (String) it.next();
                String val = (String) r.get(key);
                sb.append(key + "-->" + val + "\n");

            }
            return sb.toString();
        }
        return "";
    }

    public static String prettyPrint(Object[] a) {
        if (a == null) {
            return "";
        }
        StringBuffer sb = new StringBuffer();
        sb.append("{");
        for (int i = 0; i < a.length; i++) {
            if (i > 0) {
                sb.append(", ");
            }
            sb.append(a[i].toString());
        }
        sb.append("}");
        return sb.toString();
    }

    public static File getLogFile(String name) throws IOException {
        File logFile = new File(System.getProperty("java.io.tmpdir")
                + File.separator + name);
        if (!logFile.exists()) {
            logFile.getParentFile().mkdirs();
            logFile.createNewFile();
        }
        return logFile.getCanonicalFile();
    }

    /**
     * 
     * @param cmd
     * @return an array of two strings: th 0th one contains process output, the
     *               1th one error.
     * @throws IOException
     * @throws InterruptedException
     */
    public static String[] exec(String cmd) throws IOException,
            InterruptedException {
        Process process = null;
        try {

            process = Runtime.getRuntime().exec(cmd);
        } catch (Throwable t) {
            Debug.report(t);
            return new String[] { "ERROR", "" };
        }

        class _InputStreamPump extends InputStreamPump {
            StringBuffer sb = new StringBuffer();

            public _InputStreamPump(InputStream s) {
                super(s);
            }

            protected void dataAvailable(char[] buffer, int length) {
                String string = new String(buffer, 0, length);
                sb.append(string);

            }

        }
        _InputStreamPump errPump = new _InputStreamPump(process
                .getErrorStream());
        _InputStreamPump outPump = new _InputStreamPump(process
                .getInputStream());
        errPump.start();
        outPump.start();
        process.waitFor();
        outPump.join();
        errPump.join();
        return new String[] { outPump.sb.toString(), errPump.sb.toString() };
    }

    public static void copy(InputStream in, OutputStream out)
            throws IOException {
        BufferedInputStream bIn = null;
        BufferedOutputStream bOut = null;
        try {
            bIn = new BufferedInputStream(in);
            bOut = new BufferedOutputStream(out);
            byte[] buf = new byte[255];
            int read = -1;
            while ((read = bIn.read(buf)) > -1) {
                out.write(buf, 0, read);
            }
        } finally {
            bOut.close();
            bIn.close();
        }
    }
    public static boolean unescape(CharSequence input, StringBuffer sb){
        boolean escape=false;
        boolean quote = false;        
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            if(escape){
                switch(c){
                	default:
                	    return false;
                	case '\\':
                	case '\'':
                	case '\"':
                	    sb.append(c);
                	    break;
                	 case 'b':
                	     sb.append('\b');
                	     break;
                	 case 't':
                	     sb.append('\t');
                	     break;
                	 case 'n':
                	     sb.append('\n');
                	     break;
                	 case 'f':
                	     sb.append('\f');
                	     break;
                	 case 'r':
                	     sb.append('\r');
                	     break;
                	 
                }
            }
                
        }
        return !quote && ! escape;
    }
    
    public static String toString(InputStream in) throws IOException{
        ByteArrayOutputStream out = new ByteArrayOutputStream();
		byte[] buf = new byte[1024];
		int read = in.read(buf);
		while (read > 0) {
			out.write(buf, 0, read);
			read = in.read(buf);
		}
		return out.toString();
    }
}