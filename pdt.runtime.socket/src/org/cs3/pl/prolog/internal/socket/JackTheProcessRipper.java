/**
 * 
 */
package org.cs3.pl.prolog.internal.socket;

import java.io.IOException;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.TimeUnit;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;

public class JackTheProcessRipper extends Thread {

	private static final String LINUX_AND_OTHER_KILL_COMMAND = "kill -KILL ";
	private static final String WINDOWS_KILL_COMMAND = "taskkill /F /PID ";
	private static final Long NONVALID_PID = Long.valueOf(-1);
	private static final long TIMEOUT_WAITING_FOR_AN_PID = 1000L;
	
	static JackTheProcessRipper instance;
	static private SynchronousQueue<Long> toBeDestroyed = new SynchronousQueue<Long>();
	static private boolean shuttingDown = false;

	private JackTheProcessRipper() {
		super("Jack the Process Ripper");
		setDaemon(false);
		Runtime theRuntime = Runtime.getRuntime();
		Thread shutdownHook = new Thread("Jack The Process Ripper Shutdown Hook") {
			public void run() {
				try {
					Thread.sleep(TIMEOUT_WAITING_FOR_AN_PID);
				} catch(Exception E) {
					;
				}
				shuttingDown = true;
			}
		};
		theRuntime.addShutdownHook(shutdownHook);
		start();
	}

	public static JackTheProcessRipper getInstance() {
		if (instance == null) {
			instance = new JackTheProcessRipper();
		}
		return instance;
	}

	/**
	 * Runs until the Shutdown Hook is activated and destroys every
	 * Prolog-processed referenced by a PID that is given via
	 * JackTheProcessRipper.markForDeletion(long).
	 */
	public void run() {
		Long processId = NONVALID_PID;
		while (!shuttingDown) {
			try {
				processId=toBeDestroyed.poll(TIMEOUT_WAITING_FOR_AN_PID,TimeUnit.MILLISECONDS);
				if (isValidProcessId(processId)) {
					killRuntimeProcess(processId);
				}
			} catch (Throwable t) {
				Debug.report(t);
			} finally {
				processId = NONVALID_PID;
			}
		}
	}

	private static boolean isValidProcessId(Long processId) {
		if (processId==null)
			return false;
		return processId.compareTo(NONVALID_PID)>0;
	}

	private static void killRuntimeProcess(long processId) throws IOException, InterruptedException {
		String killCommand;
		if(Util.isWindows()){
			killCommand= WINDOWS_KILL_COMMAND + processId;
		} else {
			killCommand= LINUX_AND_OTHER_KILL_COMMAND + processId;
		}		
		Runtime.getRuntime().exec(killCommand);
	}

	public void markForDeletion(long processId) throws InterruptedException{
		if (shuttingDown) {
			throw new IllegalStateException(
			"you cannot register processes for deletion during shutdown");
		}
		toBeDestroyed.put(processId);
	}
}