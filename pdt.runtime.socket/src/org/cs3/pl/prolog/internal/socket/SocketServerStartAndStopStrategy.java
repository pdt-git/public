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

import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.runtime.BootstrapPrologContribution;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.InputStreamPump;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.ServerStartAndStopStrategy;

public class SocketServerStartAndStopStrategy implements ServerStartAndStopStrategy {
private static JackTheProcessRipper processRipper;
	
	public SocketServerStartAndStopStrategy() {
		processRipper=JackTheProcessRipper.getInstance();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.cs3.pl.prolog.ServerStartAndStopStrategy#startServer(org.cs3.pl.prolog
	 * .IPrologInterface)
	 */
	@Override
	public  Process startServer(PrologInterface pif) {
		if (pif.isStandAloneServer()) {
			Debug.warning("Will not start server; the option " + PrologInterface.PREF_STANDALONE + " is set.");
			return null;
		}
		if (!(pif instanceof SocketPrologInterface)) {
			throw new ClassCastException("SocketPrologInterface needed but got another PrologInterface");
		}
		SocketPrologInterface socketPif = (SocketPrologInterface) pif;
		return startSocketServer(socketPif);
	}

	private Process startSocketServer(SocketPrologInterface socketPif) {
		socketPif.setLockFile(Util.getLockFile());
		int port = getFreePort(socketPif);
		Process process = getNewProcess(socketPif, port);
		try {			
			initializeBuffers(socketPif, process);
			waitForProcessToGetRunning(socketPif, process);
			return process;
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}

	private static Process getNewProcess(SocketPrologInterface socketPif, int port) {
		String[] commandArray = getCommandArray(socketPif, port);
		String[] envarray = getEnvironmentAsArray(socketPif);
		Process process = null;
		try {
			Debug.info("Starting server with " + Util.prettyPrint(commandArray));
			// Temporary safety code to ensure that the command array contains no empty strings:
			List<String> commands = new ArrayList<String>();
			// keep this until its clear why there is an empty string elements in the array
			for (String string : commandArray) {
				if(string != null && string.length()>0){
					commands.add(string);
				}
			}
			commandArray=commands.toArray(new String[0]);
			
			if (envarray.length == 0) {
				Debug.info("inheriting system environment");
				process = Runtime.getRuntime().exec(commandArray);
			} else {
				Debug.info("using environment: " + Util.prettyPrint(envarray));
				process = Runtime.getRuntime().exec(commandArray, envarray);
			}
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
		return process;
	}

	private static void initializeBuffers(SocketPrologInterface socketPif,
			Process process) throws IOException {
		BufferedWriter writer = initializeCorrespondingLogFile(socketPif);
		new InputStreamPump(process.getErrorStream(), writer).start();
		new InputStreamPump(process.getInputStream(), writer).start();
	}

	private static BufferedWriter initializeCorrespondingLogFile(
			SocketPrologInterface socketPif) throws IOException {
		File logFile = Util.getLogFile(socketPif.getServerLogDir(),"pdt.server.log");
		BufferedWriter writer = new BufferedWriter(new FileWriter(logFile.getAbsolutePath(), true));
		writer.write("---------------------------------\n");
		writer.write(new Date().toString() + "\n");
		writer.write("---------------------------------\n");
		return writer;
	}
	
	private  void waitForProcessToGetRunning(SocketPrologInterface socketPif,
			Process process) {
		long timeout = socketPif.getTimeout();
		long startTime = System.currentTimeMillis();
		while (!isRunning(socketPif)) {
			try {
				long now = System.currentTimeMillis();
				if (now - startTime > timeout) {
					throw new RuntimeException("Timeout exceeded while waiting for peer to come up.");
				}
				Thread.sleep(50);
			} catch (InterruptedException e1) {
				Debug.report(e1);
			}
			try {
				if (process.exitValue() != 0) {
					throw new RuntimeException("Failed to start server. Process exited with err code " + process.exitValue());
				}
			} catch (IllegalThreadStateException e) {
				; // nothing. the process is still coming up.
			}
		}
	}



	private static String[] getEnvironmentAsArray(SocketPrologInterface socketPif) {
		String environment = socketPif.getEnvironment();
		Map<String, String> env = new HashMap<String, String>();
		String[] envarray = Util.split(environment, ",");
		for (int i = 0; i < envarray.length; i++) {
			String[] mapping = Util.split(envarray[i], "=");
			env.put(mapping[0], mapping[1]);
		}
		envarray = new String[env.size()];
		int i = 0;
		for (Iterator<String> it = env.keySet().iterator(); it.hasNext();) {
			String key = it.next();
			String value = env.get(key);
			envarray[i++] = key + "=" + value;
		}
		return envarray;
	}

	private static String[] getCommandArray(SocketPrologInterface socketPif, int port) {
		String[] command = getCommands(socketPif);
		String[] args = getArguments(socketPif, port);
		String[] commandArray = new String[command.length + args.length];
		System.arraycopy(command, 0, commandArray, 0, command.length);
		System.arraycopy(args, 0, commandArray, command.length, args.length);
		return commandArray;
	}

	private static String[] getArguments(SocketPrologInterface socketPif, int port) {
		File tmpFile = null;
		try {
			tmpFile = File.createTempFile("socketPif", null);
			writeInitialisationToTempFile(socketPif, port, tmpFile);
		} catch (IOException e) {
			Debug.report(e);
			throw new RuntimeException(e.getMessage());
		}
		String[] args = buildArguments(socketPif, tmpFile);
		return args;
	}

	private static String[] getCommands(SocketPrologInterface socketPif) {
		String executable = socketPif.getExecutable();
		if (!executable.contains(" -L")) {
			if(Util.getStackCommandLineParameters().length()>0) {
				executable += " " + Util.getStackCommandLineParameters();// PDTConstants.STACK_COMMMAND_LINE_PARAMETERS;
			}
		}
		String[] command = Util.split(executable, " ");
		return command;
	}

	private static String[] buildArguments(SocketPrologInterface socketPif,
			File tmpFile) {
		String fileSearchPath = socketPif.getFileSearchPath();
		String[] args;
		if (fileSearchPath != null && !(fileSearchPath.trim().length() == 0)) {
			args = new String[] { "-p", fileSearchPath, "-g", "['" + Util.prologFileName(tmpFile) + "']" };
		} else {
			args = new String[] { "-g", "['" + Util.prologFileName(tmpFile) + "']" };
		}
		return args;
	}

	private static void writeInitialisationToTempFile(SocketPrologInterface socketPif,
			int port, File tmpFile) throws FileNotFoundException {
		PrintWriter tmpWriter = new PrintWriter(new BufferedOutputStream(new FileOutputStream(tmpFile)));
		tmpWriter.println(":- guitracer.");
//		tmpWriter.println(":- FileName='/tmp/dbg_marker1.txt',open(FileName,write,Stream),writeln(FileName),write(Stream,hey),close(Stream).");
		tmpWriter.println(":- doc_collect(false).");
		if (socketPif.isHidePlwin()) {
			tmpWriter.println(":- (  (current_prolog_flag(windows, true))" + "->win_window_pos([show(false)])" + ";true).");
//			tmpWriter.println(":- (  (current_prolog_flag(executable,_A),atom_concat(_,'plwin.exe',_A))" + "->win_window_pos([show(false)])" + ";true).");
		}

		if (socketPif.isCreateLogs()) {
			tmpWriter.println(":- debug(consult_server).");

		}
		List<BootstrapPrologContribution> bootstrapLibraries = socketPif.getBootstrapLibraries();
		for (Iterator<BootstrapPrologContribution> it = bootstrapLibraries.iterator(); it.hasNext();) {
			BootstrapPrologContribution contribution = it.next();
			tmpWriter.println(":- "+contribution.getPrologInitStatement()+".");
		}
//		tmpWriter.println(":- FileName='/tmp/dbg_marker2.txt',open(FileName,write,Stream),writeln(FileName),write(Stream,hey),close(Stream).");
		tmpWriter.println(":- [library(consult_server)].");
//		tmpWriter.println(":- FileName='/tmp/dbg_marker3.txt',open(FileName,write,Stream),writeln(FileName),write(Stream,hey),close(Stream).");
		tmpWriter.println(":-consult_server(" + port + ",'" + Util.prologFileName(socketPif.getLockFile()) + "').");
//		tmpWriter.println(":- FileName='/tmp/dbg_marker4.txt',open(FileName,write,Stream),writeln(FileName),write(Stream,hey),close(Stream).");
		tmpWriter.close();
	}

	private static int getFreePort(SocketPrologInterface socketPif) {
		int port;
		try {
			port = Util.findFreePort();
			socketPif.setPort(port);
		} catch (IOException e) {
			Debug.report(e);
			throw new RuntimeException(e.getMessage());
		}
		return port;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.cs3.pl.prolog.ServerStartAndStopStrategy#stopServer(org.cs3.pl.prolog
	 * .IPrologInterface, boolean)
	 */
	@Override
	public void stopServer(PrologInterface pif) {
		if (pif.isStandAloneServer()) {
			Debug.warning("Will not stop server; the option " + PrologInterface.PREF_STANDALONE + " is set.");
			return;
		}
		if (!(pif instanceof SocketPrologInterface)) {
			throw new ClassCastException("SocketPrologInterface needed but got another PrologInterface");
		}
		try {
			if (!isRunning(pif)) {
				Debug.info("There is no server running. I do not stop anything.");
				return;
			}
			SocketPrologInterface socketPif = (SocketPrologInterface) pif;
			stopSocketServer(socketPif);
		} catch (Throwable e) {
			Debug.report(e);
			throw new RuntimeException(e.getMessage());
		}
	}
	
	private static void stopSocketServer(SocketPrologInterface socketPif){
		try {
			SocketClient client = getSocketClient(socketPif);
			sendClientShutdownCommand(client);
			long pid = client.getServerPid();
			client.close();
			socketPif.getLockFile().delete();
			Debug.info("server process will be killed in about a second.");
			processRipper.markForDeletion(pid);
		} catch (Exception e) {
			Debug.warning("There was a problem during server shutdown.");
			Debug.report(e);
		}
	}

	private static SocketClient getSocketClient(SocketPrologInterface socketPif)
			throws UnknownHostException, IOException {
		int port = socketPif.getPort();
		SocketClient client = new SocketClient(socketPif.getHost(), port);
		return client;
	}

	private static void sendClientShutdownCommand(SocketClient client) 
			throws UnknownHostException, IOException {
		client.readUntil(SocketCommunicationConstants.GIVE_COMMAND);
		client.writeln(SocketCommunicationConstants.SHUTDOWN);
		client.readUntil(SocketCommunicationConstants.BYE);
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.cs3.pl.prolog.ServerStartAndStopStrategy#isRunning(org.cs3.pl.prolog
	 * .IPrologInterface)
	 */
	@Override
	public  boolean isRunning(PrologInterface pif) {
		File lockFile = ((SocketPrologInterface) pif).getLockFile();
		return lockFile != null && lockFile.exists();
	}
}
