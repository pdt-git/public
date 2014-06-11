package org.cs3.prolog.connector;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.cs3.prolog.connector.common.ProcessUtils;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.internal.process.socket.SocketPrologInterface;
import org.cs3.prolog.connector.process.DefaultStartupStrategy;
import org.cs3.prolog.connector.process.PrologInterface;

public class Connector {

    // Preferences
    public static final String EP_TRACKERS = "prologContextTracker";
	public static final String PREF_HIDE_PLWIN = "pif.hide_plwin";
	public static final String PREF_SERVER_LOGDIR = "pif.server_log_dir";
	
	public static final String PREF_INVOCATION = "pif.invocation";
	public static final String PREF_EXECUTABLE = "prolog.connector.executable";
	public static final String PREF_COMMAND_LINE_ARGUMENTS = "pif.command.line.arguments";
	public static final String PREF_ENVIRONMENT = "pif.environment";
	public static final String PREF_ADDITIONAL_STARTUP = "pif.additional.startup";
	public static final String PREF_TIMEOUT = "pif.timeout";
	public static final String PREF_HOST = "pif.host";
	public static final String PREF_PORT = "pif.port";
	
	public static PrologInterface newUninitializedPrologProcess() {
		return newUninitializedPrologProcess(null);
	}

	public static PrologInterface newUninitializedPrologProcess(String name) {
		SocketPrologInterface socketPrologInterface = new SocketPrologInterface(name);
		try {
			socketPrologInterface.setConsultServerLocation(QueryUtils.prologFileName(Connector.getConsultServerFile()));
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return socketPrologInterface;
	}

	/**
	 * 
	 * @return a new standalone Prolog Interface
	 * @throws IOException
	 */
	public static PrologInterface newPrologProcess() throws IOException {
		return Connector.newPrologProcess(null);
	}

	/**
	 * 
	 * @param executable
	 * @return a new standalone Prolog Interface
	 * @throws IOException
	 */
	public static PrologInterface newPrologProcess(String executable) throws IOException {
		SocketPrologInterface pif = new SocketPrologInterface(null);
		pif.setStartupStrategy(new DefaultStartupStrategy());
		pif.setOSInvocation(ProcessUtils.getInvocationCommand());
		if (executable == null) {
			pif.setExecutablePath(ProcessUtils.getExecutablePreference());
		} else {
			pif.setExecutablePath(executable);
		}
		pif.setConsultServerLocation(QueryUtils.prologFileName(Connector.getConsultServerFile()));
		pif.setHost("localhost");
		pif.setTimeout("15000");
		pif.setStandAloneServer("false");
		pif.setHidePlwin(true);
		pif.setUseSessionPooling(true);
		return pif;
	}

	private static File consultServerFile = null;
	
	private static File getConsultServerFile() throws IOException {
		if (consultServerFile == null) {
			String tempDir = System.getProperty("java.io.tmpdir");
			InputStream resourceAsStream;
			resourceAsStream = SocketPrologInterface.class.getResourceAsStream("consult_server.pl");
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
