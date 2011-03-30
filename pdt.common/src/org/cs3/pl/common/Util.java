/*****************************************************************************
ex * This file is part of the Prolog Development Tool (PDT)
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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.ServerSocket;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import javax.management.RuntimeErrorException;

/**
 * contains static methods that do not quite fit anywhere else :-)=
 */
public class Util {
	/**
	 * converts a logical character offset to a physical character offset. E.g.
	 * prolog uses logical offsets in the sense that it counts any line
	 * delimiter as a single character, even if it is CRLF, etc.
	 * 
	 * Eclipse documents and views however seem to count physical characters,
	 * i.e. the CRLF line delimiter would count as two characters.
	 * 
	 */
	public static int logicalToPhysicalOffset(String data, int logical) {
		int physical = 0;
		int nextPos = data.indexOf("\r\n");
		while (nextPos >= 0 && nextPos < logical) {
			physical += (nextPos + 2);
			logical -= (nextPos + 1);
			data = data.substring(nextPos + 2);
			nextPos = data.indexOf("\r\n");
		}
		return physical + logical;
	}
	
	public static int physicalToLogicalOffset(String data, int physical) {
		int logical = 0;
		int nextPos = data.indexOf("\r\n");
		while (nextPos >= 0 && nextPos < logical) {
			physical -= (nextPos + 2);
			logical += (nextPos + 1);
			data = data.substring(nextPos + 2);
			nextPos = data.indexOf("\r\n");
		}
		return physical + logical;
	}

	public static String generateFingerPrint() {
		long l = System.currentTimeMillis();
		double m = Math.random();
		return "fp_" + l + "_" + m;
	}

	public static File getLockFile() {
		String tmpdir = System.getProperty("java.io.tmpdir");
		return new File(tmpdir, generateFingerPrint());
	}

	public static String prettyPrint(Map<String, ?> input) {
		if (input != null) {
			boolean first = true;
			StringBuffer result = new StringBuffer();
			Set<String> keys = input.keySet();
			Iterator<String> it = keys.iterator();
			while (it.hasNext()) {
				if (!first) {
					result.append(", ");
				}
				String key = it.next();
				Object value = input.get(key);
				String valueAsString = value.toString();
				result.append(key + "-->" + valueAsString);
				first = false;

			}
			return result.toString();
		}
		return "";
	}

	public static String prettyPrint(Object[] a) {
		if (a == null) {
			return "";
		}
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < a.length; i++) {
			if (i > 0) {
				sb.append(", ");
			}
			sb.append(a[i].toString());
		}
		return sb.toString();
	}
	
	public static String prettyPrint(double[] a) {
		if (a == null) {
			return "";
		}
		StringBuffer sb = new StringBuffer();
		// sb.append("{");
		for (int i = 0; i < a.length; i++) {
			if (i > 0) {
				sb.append(", ");
			}
			sb.append(a[i]);
		}
		// sb.append("}");
		return sb.toString();
	}
	
	public static String prettyPrint(Collection<?> input) {
		if (input != null && !input.isEmpty()) {
			Iterator<?> it = input.iterator();
			return concatinateElements(it);
		}
		return "";
	}

	
	private static String concatinateElements(Iterator<?> it) {
		StringBuffer sb = new StringBuffer();
		boolean first = true;
		while ( it.hasNext()) {
			if (!first) {
				sb.append(", ");
			}
			Object next = it.next();
			String elm = next == null ? "<null>" : next.toString();
			sb.append(elm);
			first = false;
		}
		return sb.toString();
	}

	public static File getLogFile(String dir, String name) throws IOException {
		File logFile = new File(dir,name);
				
		if (!logFile.exists()) {
			logFile.getParentFile().mkdirs();
			logFile.createNewFile();
		}
		return logFile.getCanonicalFile();
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

	

	public static String normalizeOnWindows(String s) {
		boolean windowsPlattform = isWindows();
		if (windowsPlattform) {
			s = s.replace('\\', '/').toLowerCase();
		}
		return s;
	}

	/**
	 * @return
	 */
	public static boolean isWindows() {
		boolean windowsPlattform = System.getProperty("os.name").indexOf(
				"Windows") > -1;
		return windowsPlattform;
	}

	/**
	 * @return
	 */
	public static boolean isMacOS() {
		boolean mac = System.getProperty("os.name").indexOf("Mac") > -1;
		return mac;
	}

	public static String prologFileName(File f) {
		try {
			return normalizeOnWindows(f.getCanonicalPath());
		} catch (IOException e) {
			throw new RuntimeException(e.getMessage());
		}
	}

	public static String prologCharsetName(String javaName) {
		return null;
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

	// specify buffer size for extraction
	static final int BUFFER = 2048;

	/**
	 * @param file
	 */
	public static void unzip(File file) {
		unzip(file, file.getParentFile());
	}

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
	@SuppressWarnings("rawtypes")
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
	public static void deleteRecursive(File file) {
		if (file.isDirectory()) {
			File[] files = file.listFiles();
			for (int i = 0; i < files.length; i++) {
				deleteRecursive(files[i]);
			}
		}
		file.delete();
	}

	public static String escape(String s) {
		StringBuffer result = new StringBuffer(s.length() + 10);
		for (int i = 0; i < s.length(); ++i) {
			char c = s.charAt(i);
			switch (c) {
			case '<':
				result.append("&lt;"); //$NON-NLS-1$
				break;
			case '>':
				result.append("&gt;"); //$NON-NLS-1$
				break;
			case '"':
				result.append("&quot;"); //$NON-NLS-1$
				break;
			case '\'':
				result.append("&apos;"); //$NON-NLS-1$
				break;
			case '&':
				result.append("&amp;"); //$NON-NLS-1$
				break;
			case '{':
				result.append("&cbo;"); //$NON-NLS-1$
				break;
			case '}':
				result.append("&cbc;"); //$NON-NLS-1$
				break;
			default:
				result.append(c);
				break;
			}
		}
		return result.toString();
	}

	public static String unescapeBuffer(StringBuffer line) {
		return unescape(line.toString(),0,line.length());
	}
	
	/**
	 * @param line
	 * @param start
	 * @param end
	 * @return
	 */
	public static String unescape(String line, int start, int end) {
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





//	/**
//	 * parse an association list.
//	 * 
//	 * @param l
//	 *            A list containing strings of the form <code>key->value</code>.
//	 * @return A map that represents the association. If the list contains
//	 *         multiple mappings for a single key, the map will contain a List
//	 *         of this values. Otherwise, the value type will bs String.
//	 */
//	public static Map<String, Object> parseAssociation(List<String> l) {
//		HashMap<String, Object> map = new HashMap<String, Object>();
//		for (Iterator<String> it = l.iterator(); it.hasNext();) {
//			String elm = it.next();
//			String[] s = splitKeyValue(elm);
//			String key = s[0];
//			String val = s[1];
//			Object o = map.get(key);
//			if (o == null) {
//				map.put(key, val);
//			} else if (o instanceof List<?>) {
//				List<String> ll = (List<String>) o;
//				ll.add(val);
//			} else {
//				List<Object> ll = new Vector<Object>();
//				ll.add(o);
//				ll.add(val);
//				map.put(key, ll);
//			}
//		}
//		return map;
//	}
//
//	private static String[] splitKeyValue(String elm) {
//		final int LEN = elm.length();
//		int l = 0;
//		StringBuffer key = new StringBuffer();
//		String value;
//		while (l < LEN - 1) {
//			if (elm.charAt(l) == '-' && elm.charAt(l + 1) == '>') {
//				value = elm.substring(l + 2, LEN);
//				return new String[] { key.toString(), value };
//			} else {
//				key.append(elm.charAt(l));
//			}
//			l++;
//		}
//
//		throw new IllegalArgumentException(elm);
//	}

	public static long parsePrologTimeStamp(String input) {
		long l = 1000 * (long) Double.parseDouble(input);
		return l;
	}

	public static int findFreePort() throws IOException {
		ServerSocket ss = new ServerSocket(0);
		int port = ss.getLocalPort();
		ss.close();
		return port;
	}

	

	public static String quoteAtom(String term) {

		return "'" + replaceAll(term, "'", "\\'") + "'";
	}

	public static String replaceAll(String string, String search, String replace) {
		int i = -1;
		StringBuffer sb = new StringBuffer();
		while ((i = string.indexOf(search, 0)) >= 0) {
			sb.append(string.substring(0, i));
			sb.append(replace);
			string = string.substring(i + search.length());
		}
		sb.append(string);
		return sb.toString();
	}

	public static String splice(Collection<String> c, String delim) {
		if (c != null && !c.isEmpty()) {
			StringBuffer sb = new StringBuffer();
			for (Iterator<String> it = c.iterator(); it.hasNext();) {
				Object next = it.next();
				sb.append(next);
				if (it.hasNext()) {
					sb.append(delim);
				}
			}
			return sb.toString();
		}
		return "";

	}

	public static String unquoteAtom(String image) {
		image = image.trim();
		if (image.length() == 0 || image.charAt(0) != '\'') {
			return image;
		}
		image = image.substring(1, image.length() - 1);
		StringBuffer sb = new StringBuffer();

		for (int i = 0; i < image.length(); i++) {
			char c = image.charAt(i);
			if (c == '\\') {
				int len = appendUnescapedChar(image, i, sb);
				i += len - 1;
			} else {
				sb.append(c);
			}
		}
		return sb.toString();
	}

	public static String unquoteStringOrAtom(String image) {

		image = image.trim();
		if (image.length() == 0){
			return image;
		}
		if( image.charAt(0) == '\"' || image.charAt(0) == '\'') {
			image = image.substring(1, image.length() - 1);
		}
		StringBuffer sb = new StringBuffer();

		for (int i = 0; i < image.length(); i++) {
			char c = image.charAt(i);
			if (c == '\\') {
				int len = appendUnescapedChar(image, i, sb);
				i += len - 1;
			} else {
				sb.append(c);
			}
		}
		return sb.toString();
	}

	private static int appendUnescapedChar(String image, int i, StringBuffer sb) {
		char c = image.charAt(i + 1);
		if (Character.isDigit(c)) {
			return appendUnescapedOctalCharSpec(image, i, sb);
		}
		switch (c) {
		case 'a':
			// sb.append('\a'); there is no bell char in java
			return 2;
		case 'b':
			sb.append('\b');
			return 2;
		case 'c':
			// sb.append('\c'); not supported
			return 2;
		case '\n':
			// ignoring
			return 2;
		case 'f':
			sb.append('\f');
			return 2;
		case 'n':
			sb.append('\n');
			return 2;
		case 'r':
			sb.append('\r');
			return 2;
		case 't':
			sb.append('\t');
			return 2;
		case 'v':
			// sb.append('\v'); vertical tabs are not supported in java
			return 2;
		case 'x':
			return appendUnescapedHexCharSpec(image, i, sb);
		default:
			sb.append(c);
			return 2;
		}
	}

	private static int appendUnescapedOctalCharSpec(String image, int i,
			StringBuffer sb) {
		String val = "";
		int j = i + 2;
		while (j < image.length() && j < i + 4 && isOctDigit(image.charAt(j))) {
			val += image.charAt(j);
			j++;
		}
		sb.append((char) Integer.parseInt(val, 8));
		if (j < image.length() && image.charAt(j) == '\\') {
			return 1 + j - i;
		}
		return j - i;
	}

	private static int appendUnescapedHexCharSpec(String image, int i,
			StringBuffer sb) {

		String val = "";
		int j = i + 2;
		while (j < image.length() && j < i + 4 && isHexDigit(image.charAt(j))) {
			val += image.charAt(j);
			j++;
		}
		sb.append((char) Byte.parseByte(val));
		if (j < image.length() && image.charAt(j) == '\\') {
			return 1 + j - i;
		}
		return j - i;
	}

	private static boolean isHexDigit(char c) {

		return Character.isDigit(c) || 'a' <= Character.toLowerCase(c)
				&& Character.toLowerCase(c) <= 'f';
	}

	private static boolean isOctDigit(char c) {

		return Character.isDigit(c) || '0' <= c && c <= '7';
	}

	public static void split(String string, String search, Collection<String> results) {
		if (string == null) {
			return;
		}
		int i = -1;
		while ((i = string.indexOf(search, 0)) >= 0) {
			results.add(string.substring(0, i).trim());
			string = string.substring(i + search.length());
		}
		String rest = string.trim();
		if (rest.length() > 0) {
			results.add(rest);
		}

	}

	public static String[] split(String string, String search) {
		Vector<String> v = new Vector<String>();
		split(string, search, v);
		return v.toArray(new String[v.size()]);

	}

	/**
	 * fascilate testing by replacing things like $stream(1760696) with
	 * <replace>
	 * 
	 * @param message
	 * @return
	 */
	public static String hideStreamHandles(String string, String replace) {
		int i = -1;
		String search = "$stream(";
		StringBuffer sb = new StringBuffer();
		while ((i = string.indexOf(search, 0)) >= 0) {
			sb.append(string.substring(0, i));
			sb.append(replace);
			int j = string.indexOf(')', i + search.length());
			string = string.substring(j + 1);
		}
		sb.append(string);
		return sb.toString();

	}

	public static String splice(Object[] c, String delim) {
		if (c != null && c.length > 0) {
			StringBuffer sb = new StringBuffer();
			for (int i = 0; i < c.length; i++) {
				if (i > 0) {
					sb.append(delim);
				}
				Object next = c[i];
				sb.append(next);

			}
			return sb.toString();
		}
		return "";
	}

	/**
	 * To be deleted once Java 6 is supported on EVERY(?) target platform.
	 * Currently MacOS <= 10.4. is not supported.
	 * 
	 *  name.getBytes(Charset.forName("UTF-8"))
	 * 
	 * @param name
	 * @return
	 */
	public static byte[] getUTF8Bytes(String name) {
		try {
			return name.getBytes(Charset.forName("UTF-8").toString());
		} catch (UnsupportedEncodingException e) {
			Debug.rethrow(e);
			return null;
		}
	}

	/**
	 * To be deleted once Java 6 is supported on EVERY(?) target platform.
	 * Currently MacOS <= 10.4. is not supported.
	 * 
	 *  new String(byte[], Charset.forName("UTF-8"))
	 * @param bytes
	 * @return
	 */
	static public String encodeUTF8String(byte[] bytes) {
		try {
			return new String(bytes,Charset.forName("UTF-8").toString());
		} catch (UnsupportedEncodingException e) {
			Debug.rethrow(e);
			return null;
		}
	}

	public static boolean flagsSet(int flags, int set) {

		return (flags & set) == set;
	}
	
	
	public static String guessEnvironmentVariables() {
		if (isMacOS()) {
			String home = System.getProperty("user.home");
			return "DISPLAY=:0.0, HOME=" + home;
		}
		return "";
	}

	public static String guessExecutableName() {

		String guessedExecutable = guessExecutableName__();
		System.out.println("Guessed Prolog executable with GUI: " + guessedExecutable);
		return guessedExecutable;

	}


	private static String stackCommandLineParameters = null;
	
	public static String getStackCommandLineParameters() {
//		return PDTConstants.STACK_COMMMAND_LINE_PARAMETERS;	
		if (stackCommandLineParameters == null) {
		
			String swiExecutable;
			
			if (isWindows()) {
				swiExecutable = findWindowsExecutable(PDTConstants.WINDOWS_COMMAND_LINE_EXECUTABLES);			
			} else {
				swiExecutable = findUnixExecutable(PDTConstants.UNIX_COMMAND_LINE_EXECUTABLES);
			}
			
			String bits = "";
			try {
				Process p = Runtime.getRuntime().exec(new String[]{
						swiExecutable,
						"-g",
						"current_prolog_flag(address_bits,A),writeln(A),halt."});
				BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
				bits = reader.readLine();
				p.waitFor();
			} catch (Exception e) {
				e.printStackTrace();
			}
	
			if (bits.equals("64")) {
				// no parameters for SWI-Prolog 64bit
				stackCommandLineParameters = "";
			} else {
				stackCommandLineParameters = PDTConstants.STACK_COMMMAND_LINE_PARAMETERS;
			}

		}
		return stackCommandLineParameters;
	}

	public static String getCurrentSWIVersionFromCommandLine() throws IOException{
//		return "51118_64";// TEMPversion +"_"+bits;
		
			String swiExecutable;
			if (isWindows()) {
				swiExecutable = findWindowsExecutable(PDTConstants.WINDOWS_COMMAND_LINE_EXECUTABLES);			
			} else {
				swiExecutable = findUnixExecutable(PDTConstants.UNIX_COMMAND_LINE_EXECUTABLES);
			}
			
			String bits = "";
			String version ="";
			Process p = Runtime.getRuntime().exec(new String[]{
					swiExecutable,
					"-g",
					"current_prolog_flag(version,V),writeln(V),current_prolog_flag(address_bits,A),writeln(A),halt."});
			BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
			version = reader.readLine();
			bits = reader.readLine();
			try {
				p.waitFor();
			} catch (InterruptedException e) {
				// TR: fatal anyway:
				throw new RuntimeException(e);
			}
	

		return version +"_"+bits;
	}

	private static String guessExecutableName__() {

		if (isWindows()) {
			return "cmd.exe /c start \"cmdwindow\" /min "
				+ findWindowsExecutable(PDTConstants.WINDOWS_EXECUTABLES) + " " + getStackCommandLineParameters();
		}
		// return "xterm -e xpce"; // For Mac and Linux with console
		return findUnixExecutable(PDTConstants.UNIX_COMMAND_LINE_EXECUTABLES) + " " + getStackCommandLineParameters();

	}

	public static String guessCommandLineExecutableName() {

		String guessedExecutable = guessCommandLineExecutableName__();
		System.out.println("Guessed Prolog executable WITHOUT GUI: " + guessedExecutable);
		return guessedExecutable;

	}
	
	private static String guessCommandLineExecutableName__() {

		if (isWindows()) {
			return //"cmd.exe /c start \"cmdwindow\" /min "
			findWindowsExecutable(PDTConstants.WINDOWS_COMMAND_LINE_EXECUTABLES) + " " + getStackCommandLineParameters();
		}
		// return "xterm -e xpce"; // For Mac and Linux with console
		return findUnixExecutable(PDTConstants.UNIX_COMMAND_LINE_EXECUTABLES) + " " + getStackCommandLineParameters();

	}

	/**
	 * @author Hasan Abdel Halim
	 * 
	 * Finds the current SWI-Prolog executable for UNIX/BSD-BASED OS
	 * @param unixCommandLineExecutables 
	 * @return the complete path of the executable otherwise it will return xpce
	 */
	private static String findUnixExecutable(String unixCommandLineExecutables) {
		String[] default_exec = unixCommandLineExecutables.split(",");
		
		// TODO shall we look for the env. variables as we do for Windows ?
		String[] appendPath = null;

		// Hack to resolve the issue of locating xpce in MacOS
		if (isMacOS()) {
			appendPath = new String[1];
			appendPath[0] = "PATH=PATH:/opt/local/bin";
		}

		try {
			
			for (String exec : default_exec) {

				Process process = Runtime.getRuntime().exec(
						"which " + exec, appendPath);
	
				if (process == null)
					return null;
	
				BufferedReader br = new BufferedReader(new InputStreamReader(
						process.getInputStream()));
				String path = br.readLine();
	
				if (path == null || path.startsWith("no " + default_exec))
					continue;
				else {
					return path;
				}
			}
			return default_exec[0];

		} catch (IOException e) {

			return default_exec[0];
		}
	}

	/**
	 * @author Hasan Abdel Halim
	 * 
	 * Finds the current SWI-Prolog executable for Windoze OS
	 * @param executables 
	 * @return the complete path of the executable otherwise it will return
	 *         plwin
	 */
	private static String findWindowsExecutable(String executables) {
		String[] default_exec = executables.split(",");
		String plwin = null;

		String path;
		try {

			Process process = Runtime.getRuntime().exec(
					"cmd.exe /c echo %PATH%");

			if (process == null)
				return default_exec[0];

			BufferedReader br = new BufferedReader(new InputStreamReader(
					process.getInputStream()));
			path = br.readLine();

			if (path == null)
				return default_exec[0];

			// TODO just search in case of executable was not found.
			String[] paths = split(path, ";");
			File exeFile = null;

			for (int i = 0; i < paths.length; i++) {
				
				for (String exec : default_exec) {
					if (exec.indexOf(".exe") == -1)
						exec += ".exe";

					String currPath = paths[i] + "\\" + exec;
					exeFile = new File(currPath);

					if (exeFile.exists()) {
						plwin = "\"" + currPath + "\"";
						break;
					}
				}
				if(plwin!=null){
					break;
				}
			}
			if(plwin== null){
				return default_exec[0];
			}
			return plwin;

		} catch (IOException e) {

			return default_exec[0];
		}
	}

	/**
	 * @param prefix
	 * @return
	 */
	public static boolean isVarChar(char c) {
		if (c == '_')
			return true;
		if (c >= 'A' && c <= 'Z')
			return true;
		if (c >= 'a' && c <= 'z')
			return true;
		if (c >= '0' && c <= '9')
			return true;
		return false;
	}

	/**
	 * @param prefix
	 * @return
	 */
	public static boolean isVarPrefix(char c) {
		if (c == '_')
			return true;
		if (c >= 'A' && c <= 'Z')
			return true;
		return false;
	}

	/**
	 * @param prefix
	 * @return
	 */
	public static boolean isFunctorPrefix(String prefix) {
		if (prefix == null | prefix.length() == 0)
			return false;
		if (prefix.charAt(0) >= 'a' && prefix.charAt(0) <= 'z')
			return true;
	
		return false;
	}

	/**
	 * @param prefix
	 * @return
	 */
	public static boolean isVarPrefix(String prefix) {
		if (prefix.length() == 0)
			return false;
		return isVarPrefix(prefix.charAt(0));
	}

	/**
	 * @param c
	 * @return
	 */
	static public boolean isPredicatenameChar(char c) {
		if (c >= 'a' && c <= 'z')
			return true;
		if (c >= '0' && c <= '9')
			return true;
		if (c >= 'A' && c <= 'Z')
			return true;
		if (c == ':' || c == '_' || c == '+' || c == '-' || c == '\\'
				|| c == '*')
			return true;
		return false;
	}

	static public boolean isNonQualifiedPredicatenameChar(char c) {
		return isPredicatenameChar(c) && c != ':';
	}

	static public boolean isFunctorChar(char c) {
		if (c >= 'a' && c <= 'z')
			return true;
		if (c >= '0' && c <= '9')
			return true;
		if (c >= 'A' && c <= 'Z')
			return true;
		if (c == '_')
			return true;
	
		return false;
	}



	public static boolean isSingleSecondChar(char c) {
		if (c >= '0' && c <= '9')
			return true;
		if (c >= 'A' && c <= 'Z')
			return true;
		return false;
	}

}