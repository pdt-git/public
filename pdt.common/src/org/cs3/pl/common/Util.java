package org.cs3.pl.common;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Vector;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import sun.misc.Unsafe;

/**
 * contains static methods that do not quite fit anywhere else :-)=
 */
public class Util {
	
	
	
	public static String generateFingerPrint(){
		long l = System.currentTimeMillis();
		double m = Math.random();
		return "fp_"+l+"_"+m;		
	}
	
	public static File getLockFile(){
		String tmpdir = System.getProperty("java.io.tmpdir");
		return new File(tmpdir,generateFingerPrint());
	}
	
	/**
	 * @deprecated this does not work correctly e.g. on systems
	 * that have separate ip4 and ip6 stacks (MacOS X,...)
	 */
    public static boolean probePort(int port) {
        try {
            ServerSocket ss = new ServerSocket(port);
//            Socket cs = ss.accept();
//            
//            new InputStreamPump(cs.getInputStream()){
//            	protected void dataAvailable(char[] buffer, int length) {
//            		System.out.print(String.copyValueOf(buffer,0,length));
//            	}
//            };
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
     *             1th one error.
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
            bOut.flush();
        }
    }

    public static String normalizeOnWindoze(String s) {
        boolean windowsPlattform = isWindoze();
        if (windowsPlattform) {
            s = s.replace('\\', '/');
        }
        return s;
    }

    /**
     * @return
     */
    public static boolean isWindoze() {
        boolean windowsPlattform = System.getProperty("os.name").indexOf(
                "Windows") > -1;
        return windowsPlattform;
    }

    public static String prologFileName(File f) {
        try {
            return normalizeOnWindoze(f.getCanonicalPath());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static String toString(InputStream in) throws IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        byte[] buf = new byte[1024];
        int read = in.read(buf);
        while (read > 0) {
            out.write(buf, 0, read);
            read = in.read(buf);
        }
        return out.toString();
    }

    //  specify buffer size for extraction
    static final int BUFFER = 2048;

    /*
     * the body of this method was taken from here.
     * 
     * http://www.devshed.com/c/a/Java/Zip-Meets-Java/2/
     * 
     * Many thanks to the author, Kulvir Singh Bhogal.
     * 
     * I could not find any license or copyright notice If there are any legal
     * problems please let me: know degenerl_AT_cs_DOT_uni-bonn_DOT_de
     * 
     * --lu
     */
    public static void unzip(File sourceZipFile, File unzipDestinationDirectory) {
        try {

            // Open Zip file for reading
            ZipFile zipFile = new ZipFile(sourceZipFile, ZipFile.OPEN_READ);

            // Create an enumeration of the entries in the zip file
            Enumeration zipFileEntries = zipFile.entries();

            // Process each entry
            while (zipFileEntries.hasMoreElements()) {
                // grab a zip file entry
                ZipEntry entry = (ZipEntry) zipFileEntries.nextElement();

                String currentEntry = entry.getName();
                System.out.println("Extracting: " + entry);

                File destFile = new File(unzipDestinationDirectory,
                        currentEntry);

                // grab file's parent directory structure
                File destinationParent = destFile.getParentFile();

                // create the parent directory structure if needed
                destinationParent.mkdirs();

                // extract file if not a directory
                if (!entry.isDirectory()) {
                    BufferedInputStream is = new BufferedInputStream(zipFile
                            .getInputStream(entry));
                    int currentByte;
                    // establish buffer for writing file
                    byte data[] = new byte[BUFFER];

                    // write the current file to disk
                    FileOutputStream fos = new FileOutputStream(destFile);
                    BufferedOutputStream dest = new BufferedOutputStream(fos,
                            BUFFER);

                    // read and write until last byte is encountered
                    while ((currentByte = is.read(data, 0, BUFFER)) != -1) {
                        dest.write(data, 0, currentByte);
                    }
                    dest.flush();
                    dest.close();
                    is.close();
                }
            }
            zipFile.close();
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
    }

    /**
     * @param file
     */
    public static void unzip(File file) {
        unzip(file, file.getParentFile());
    }

    /**
     * @param file
     */
    public static void deleteRecursive(File file) {
        if (file.isDirectory()) {
            File[] files = file.listFiles();
            for (int i = 0; i < files.length; i++) {
                deleteRecursive(files[i]);
            }
        }
        file.delete();
    }

    /**
     * @param line
     * @param start
     * @param end
     * @return
     */
    public static String unescape(CharSequence line, int start, int end) {
        StringBuffer sb = new StringBuffer();
        boolean escape = false;
        StringBuffer escBuf = new StringBuffer();
        for (int i = start; i < end; i++) {
            char c = line.charAt(i);
            switch (c) {
            case '&':
                escape = true;
                break;
            case ';':
                if (escape) {
                    escape = false;
                    String escSeq = escBuf.toString();
                    escBuf.setLength(0);
                    if ("lt".equals(escSeq)) {
                        sb.append('<');
                    } else if ("gt".equals(escSeq)) {
                        sb.append('>');
                    } else if ("cbo".equals(escSeq)) {
                        sb.append('{');
                    } else if ("cbc".equals(escSeq)) {
                        sb.append('}');
                    } else if ("amp".equals(escSeq)) {
                        sb.append('&');
                    } else if ("apos".equals(escSeq)) {
                        sb.append('\'');
                    } else if ("quot".equals(escSeq)) {
                        sb.append('\"');
                    }
                } else {
                    sb.append(c);
                }
                break;
            default:
                if (escape) {
                    escBuf.append(c);
                } else {
                    sb.append(c);
                }
                break;
            }
        }
        return sb.toString();
    }

    private static HashMap startTimes = new HashMap();

    public static void startTime(String key) {
        String prefix = Thread.currentThread().toString();
        startTimes.put(prefix + key, new Long(System.currentTimeMillis()));
    }

    public static long time(String key) {
        String prefix = Thread.currentThread().toString();
        Long startTime = (Long) startTimes.get(prefix + key);
        return startTime == null ? -1 : System.currentTimeMillis()
                - startTime.longValue();
    }

    static PrintStream logStream = null;

    public static void printTime(String key) {
        if (logStream == null) {
            try {
                File f = getLogFile("times.txt");
                logStream = new PrintStream(new BufferedOutputStream(
                        new FileOutputStream(f)));
                Runtime.getRuntime().addShutdownHook(new Thread() {
                    public void run() {
                        logStream.close();
                    }
                });

            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        logStream.println(key + " took " + time(key) + " millis.");
    }

    /**
     * parse an association list.
     * 
     * @param l
     *                  A list containing strings of the form <code>key->value</code>.
     * @return A map that represents the association. If the list contains
     *             multiple mappings for a single key, the map will contain a List
     *             of this values. Otherwise, the value type will bs String.
     */
    public static Map parseAssociation(List l) {
        HashMap map = new HashMap();
        for (Iterator it = l.iterator(); it.hasNext();) {
            String elm = (String) it.next();
            String[] s = elm.split("->");
            String key = s[0];
            String val = s[1];
            Object o = map.get(key);
            if (o == null) {
                map.put(key, val);
            } else if (o instanceof List) {
                List ll = (List) o;
                ll.add(val);
            } else {
                List ll = new Vector();
                ll.add(o);
                ll.add(val);
                map.put(key, ll);
            }
        }
        return map;
    }

	public static long parsePrologTimeStamp(String input){
	    long l = 1000* (long) Double.parseDouble(input);
		return l;
	}

	public static int findFreePort() throws IOException {
		ServerSocket ss = new ServerSocket(0);
		int port = ss.getLocalPort();
		ss.close();
		return port;
	}
}