package org.cs3.prolog.connector;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.cs3.prolog.connector.common.ProcessUtils;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.internal.process.socket.SocketPrologProcess;
import org.cs3.prolog.connector.process.DefaultStartupStrategy;
import org.cs3.prolog.connector.process.PrologProcess;

public class Connector {

    // Preferences
    public static final String EP_TRACKERS = "prologContextTracker";
	public static final String PREF_HIDE_PLWIN = "process.hide_plwin";
	public static final String PREF_SERVER_LOGDIR = "process.server_log_dir";
	
	public static final String PREF_INVOCATION = "process.invocation";
	public static final String PREF_EXECUTABLE = "prolog.connector.executable";
	public static final String PREF_COMMAND_LINE_ARGUMENTS = "process.command.line.arguments";
	public static final String PREF_ENVIRONMENT = "process.environment";
	public static final String PREF_ADDITIONAL_STARTUP = "process.additional.startup";
	public static final String PREF_TIMEOUT = "process.timeout";
	public static final String PREF_HOST = "process.host";
	public static final String PREF_PORT = "process.port";
	
	public static PrologProcess newUninitializedPrologProcess() {
		return newUninitializedPrologProcess(null);
	}

	public static PrologProcess newUninitializedPrologProcess(String name) {
		SocketPrologProcess socketPrologProcess = new SocketPrologProcess(name);
		try {
			socketPrologProcess.setConsultServerLocation(QueryUtils.prologFileName(Connector.getConsultServerFile()));
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return socketPrologProcess;
	}

	/**
	 * 
	 * @return a new standalone Prolog Interface
	 * @throws IOException
	 */
	public static PrologProcess newPrologProcess() throws IOException {
		return Connector.newPrologProcess(null);
	}

	/**
	 * 
	 * @param executable
	 * @return a new standalone Prolog Interface
	 * @throws IOException
	 */
	public static PrologProcess newPrologProcess(String executable) throws IOException {
		SocketPrologProcess process = new SocketPrologProcess(null);
		process.setStartupStrategy(new DefaultStartupStrategy());
		process.setOSInvocation(ProcessUtils.getInvocationCommand());
		if (executable == null) {
			process.setExecutablePath(ProcessUtils.getExecutablePreference());
		} else {
			process.setExecutablePath(executable);
		}
		process.setConsultServerLocation(QueryUtils.prologFileName(Connector.getConsultServerFile()));
		process.setHost("localhost");
		process.setTimeout("15000");
		process.setStandAloneServer("false");
		process.setHidePlwin(true);
		process.setUseSessionPooling(true);
		return process;
	}

	private static File consultServerFile = null;
	
	private static File getConsultServerFile() throws IOException {
		if (consultServerFile == null) {
			String tempDir = System.getProperty("java.io.tmpdir");
			InputStream resourceAsStream;
			resourceAsStream = SocketPrologProcess.class.getResourceAsStream("consult_server.pl");
			if (resourceAsStream == null) {
				throw new RuntimeException("Cannot find consult_server.pl!");
			}
			consultServerFile = new File(tempDir, "consult_server.pl");
			if (consultServerFile.exists()) {
				consultServerFile.delete();
			}
			Util.copy(resourceAsStream, new FileOutputStream(consultServerFile));
		}
		return consultServerFile;
	}
}
