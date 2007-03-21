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
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.InputStreamPump;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.ServerStartAndStopStrategy;

/**
 */
public class SocketServerStartAndStopStrategy implements
		ServerStartAndStopStrategy {

	private ProcessWrapper serverProcess;

	public class _InputStreamPump extends InputStreamPump {

		private Writer log;

		public _InputStreamPump(InputStream s, Writer writer) {
			super(s);
			this.log = writer;
		}

		protected void dataAvailable(char[] buffer, int length) {
			try {
				log.write(buffer, 0, length);
				log.flush();
			} catch (IOException e) {
				throw new RuntimeException(e.getMessage());
			}
		}

	}

	/**
	 * 	Checks whether SWI-Prolog's executable exists or not, If it was found then it returns
	 *  the absolute path of the executable.
	 * 
	 * @author Hasan Abdel Halim
	 * @param command
	 * @return The first path that contains the executable name.
	 * @throws IOException
	 */
	
	public static String[] findAbsolutePath(String[] command) throws IOException{
		int execPos = 0;
		Runtime runtime = Runtime.getRuntime();
		String execName = "";
		String[] execCommand = (String[]) command.clone();
		
		
		if ( execCommand.length == 0)
			return null;
		
		/*
		 *  Locate the executable's name within the command array.
		 *  It is needed in order to replace it with its absolute path.
		 */ 
		
		
		/*
		 * TODO right now executable names are hardcoded for all platforms. 
		 * Make them more configurable. 
		 */
		String match = "xpce"; 		// We assume the rest to be Unix/Linux/BSD based.
				
		if (Util.isWindoze())
			match = "plwin";
		
		boolean found = false;
		
		for (int i = 0; i < execCommand.length; i++) {
				if(execCommand[i].indexOf(match) != -1){
					found = true;
					execName = execCommand[i];
					execPos = i;
					break;
				}
			}
			
		if (!found) 
			return null;
		
		// Check the executable whether it is absolute or simple
		String pathSep = System.getProperty("file.separator");
		if (execName.indexOf(pathSep) != -1) {
			//Absolute 
			
			// executable was found. Hence, there is no need to replace it.
			if(new File(execName).exists())
				return execCommand;
			else{
				// was not found. We extract its simple name then try to locate it.
				int index = execName.lastIndexOf(pathSep);
				
				if (index != -1)
					execName = execName.substring(index);
				else
					//It should never happen
					return null;
			}
		}
		
		/*
		 * Locate the executable within PATH 
		 */
		
		//TODO find an easier way to find the current path of WindowsOS
		if ( Util.isWindoze() ) {
		//WINDOWS	
			Process process = runtime.exec("cmd.exe /c echo %PATH%");
			
			if (process == null)
				return null;
				
			BufferedReader br = new BufferedReader( 
				new InputStreamReader(process.getInputStream()));
			String path = br.readLine();
			
			if (path == null) 
				return null;

			// Looks for the executable within the current path
			
			//TODO just search in case of executable was not found.
			String[] paths = Util.split(path, ";");
			File exeFile = null;
			found = false;
			
			for (int i = 0; i < paths.length; i++) {
				
				if (execName.indexOf(".exe")== -1)
					execName += ".exe";
				
				String currPath = paths[i]+ "\\" + execName;
				exeFile = new File(currPath);
				
				if(exeFile.exists()){
					execCommand[execPos] = "\""+currPath +"\"";
					found = true;
					break;
				}
			}
			
			if(!found) 
				return null;
		
		} else {
		//OTHERS ( LINUX / UNIX / MACOS /BSD )
			
			//TODO shall we look for the env. variables as we do for Windows ?
			Process process = runtime.exec("which "+ execName);
			
			if (process == null)
				return null;
			
			BufferedReader br = new BufferedReader( 
					new InputStreamReader(process.getInputStream()));
			String path = br.readLine();
			
			if ( path == null || path.startsWith("no "+execName)) 
				return null;
			
			execCommand[execPos] = path;
		}
		
		return execCommand;
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.ServerStartAndStopStrategy#startServer(org.cs3.pl.prolog.IPrologInterface)
	 */
	public Process startServer(PrologInterface pipi) {
		SocketPrologInterface pif = (SocketPrologInterface) pipi;
		pif.setLockFile(Util.getLockFile());
		if (Boolean.valueOf(pif.getOption(SocketPrologInterface.STANDALONE))
				.booleanValue()) {
			Debug.warning("Will not start server; the option "
					+ SocketPrologInterface.STANDALONE + " is set.");
			return null;
		}
		int port;
		try {
			port = Util.findFreePort();
			pif.setPort(port);
		} catch (IOException e) {
			Debug.report(e);
			throw new RuntimeException(e.getMessage());
		}
		String executable = pif.getOption(SocketPrologInterface.EXECUTABLE);
		String envstring = pif.getOption(SocketPrologInterface.ENVIRONMENT);
		String engineDir = Util.prologFileName(pif.getFactory()
				.getResourceLocator().resolve("/"));

		File tmpFile = null;
		try {
			tmpFile = File.createTempFile("socketPif", null);
			PrintWriter p = new PrintWriter(new BufferedOutputStream(
					new FileOutputStream(tmpFile)));
			if (pif.isHidePlwin()) {
				p
						.println(":- (  (current_prolog_flag(executable,_A),atom_concat(_,'plwin.exe',_A))"
								+ "->win_window_pos([show(false)])" + ";true).");
			}
			List bootstrapLIbraries = pif.getBootstrapLibraries();
			for (Iterator it = bootstrapLIbraries.iterator(); it.hasNext();) {
				String s = (String) it.next();
				p.println(":- ['" + s + "'].");
			}
			p.println("file_search_path(library,'" + engineDir + "').");
			p.println(":-consult_server(" + port + ",'"
					+ Util.prologFileName(pif.getLockFile()) + "').");
			p.close();
		} catch (IOException e) {
			Debug.report(e);
			throw new RuntimeException(e.getMessage());
		}

		String[] command = Util.split(executable, " ");
		String[] args = new String[] { "-g",
				"['" + Util.prologFileName(tmpFile) + "']",

		};

		/*
		 * Checks whether the SWI-Prolog exists 
		 * @author Hasan Abdel Halim
		 * 
		 */
		try {
			command = findAbsolutePath(command);
			if(command==null)
				throw new RuntimeException("SWI-Prolog's executable was not found.");
			
		} catch (IOException e2) {
			e2.printStackTrace();
			return null;
		}
		
	
		String[] commandArray = new String[command.length + args.length];
		System.arraycopy(command, 0, commandArray, 0, command.length);
		System.arraycopy(args, 0, commandArray, command.length, args.length);
		
		Map env=new HashMap();
//		if(Util.isJava5()){
//			env.putAll(System.getenv());
//		}
//		
		String[] envarray = Util.split(envstring, ",");
		for (int i = 0; i < envarray.length; i++) {
			String[] mapping = Util.split(envarray[i],"=");
			env.put(mapping[0],mapping[1]);
		}
		envarray = new String[env.size()];
		int i=0;
		for (Iterator it = env.keySet().iterator(); it.hasNext();) {
			String key = (String) it.next();
			String value = (String) env.get(key);
			envarray[i++]=key+"="+value;
		}
		Process process=null;
		try {
			Debug.info("Starting server with " + Util.prettyPrint(commandArray));
			if(envarray.length==0){
				Debug.info("inheriting system environment");		
				process = Runtime.getRuntime().exec(commandArray);
			}else{
				Debug.info("using environment: " + Util.prettyPrint(envarray));		
				process = Runtime.getRuntime().exec(commandArray, envarray);
			}
			
			
			File logFile = Util.getLogFile("org.cs3.pdt.server.log");
			// TR: Do not change this constructor call!
			// J2ME requirement: FileWriter(File,boolean) -> FileWriter(String, boolean)
			BufferedWriter writer = new BufferedWriter(new FileWriter(
					logFile.getAbsolutePath(),
					true));
			writer.write("\n---8<-----------------------8<---\n");
			writer.write(new Date().toString() + "\n");
			writer.write("---8<-----------------------8<---\n\n");
			
			new _InputStreamPump(process.getErrorStream(), writer).start();
			new _InputStreamPump(process.getInputStream(), writer).start();

			long timeout = pif.getTimeout();
			long startTime = System.currentTimeMillis();
			while (!pif.getLockFile().exists()) {
				try {
					long now = System.currentTimeMillis();
					if (now - startTime > timeout) {
						throw new RuntimeException(
								"Timeout exceeded while waiting for peer to come up.");
					}
					Thread.sleep(50);
				} catch (InterruptedException e1) {
					Debug.report(e1);
				}
				try {
					if (process.exitValue() != 0) {
						throw new RuntimeException(
								"Failed to start server. Process exited with err code "
										+ process.exitValue());
					}
				} catch (IllegalThreadStateException e) {
					; // nothing. the process is still running.
				}
				try {
					if (process.exitValue() != 0) {
						throw new RuntimeException(
								"Failed to start server. Process exited with err code "
										+ process.exitValue());
					}
				} catch (IllegalThreadStateException e) {
					; // nothing. the process is still running.
				}
			}
			// The process should be up now.
			SocketClient c = new SocketClient(pif.getOption(SocketPrologInterface.HOST), port);
			long pid = c.getServerPid();
			c.close();
			String cmd = pipi.getOption(SocketPrologInterface.KILLCOMMAND)
					+ " " + pid;
			// an experiment
			this.serverProcess = new ExternalKillProcessWrapper(process, cmd);
			JackTheProcessRipper.getInstance().registerProcess(serverProcess);
			return process;
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.ServerStartAndStopStrategy#stopServer(org.cs3.pl.prolog.IPrologInterface,
	 *      boolean)
	 */
	public void stopServer(PrologInterface pipi) {
		SocketPrologInterface pif = (SocketPrologInterface) pipi;
		try {
			if (Boolean
					.valueOf(pif.getOption(SocketPrologInterface.STANDALONE))
					.booleanValue()) {
				Debug.warning("Will not stop server; the option "
						+ SocketPrologInterface.STANDALONE + " is set.");
				return;
			}
			int port = pif.getPort();
			if (!pif.getLockFile().exists()) {
				Debug
						.info("There is no server running, afaics. So i wont stop anything.");
				return;
			}
			
			try {
				SocketClient c = new SocketClient(pif.getOption(SocketPrologInterface.HOST), port);
				c.readUntil(SocketClient.GIVE_COMMAND);
				c.writeln(SocketClient.SHUTDOWN);
				c.readUntil(SocketClient.BYE);
				c.close();
			} catch (Exception e) {
				Debug.warning("There was a problem during server shutdown.");
				Debug.report(e);
				
			}

			pif.getLockFile().delete();
			Debug.info("server process will be killed in 5 seconds.");
			JackTheProcessRipper.getInstance().enqueue(serverProcess, 5000);
			serverProcess = null;

		} catch (Throwable e) {
			Debug.report(e);
			throw new RuntimeException(e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.ServerStartAndStopStrategy#isRunning(org.cs3.pl.prolog.IPrologInterface)
	 */
	public boolean isRunning(PrologInterface pif) {
		File lockFile = ((SocketPrologInterface) pif).getLockFile();
		return lockFile != null && lockFile.exists();
	}
}
