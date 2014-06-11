package org.cs3.prolog.connector;

import java.io.IOException;

import org.cs3.prolog.common.Util;
import org.cs3.prolog.internal.pif.socket.SocketPrologInterface;
import org.cs3.prolog.pif.PrologInterface;

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
	
	public static PrologInterface newPrologInterface() {
		return newPrologInterface(null);
	}

	public static PrologInterface newPrologInterface(String name) {
		SocketPrologInterface socketPrologInterface = new SocketPrologInterface(name);
		try {
			socketPrologInterface.setConsultServerLocation(Util.prologFileName(Util.getConsultServerFile()));
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return socketPrologInterface;
	}
}
