/*
 */
package org.cs3.pl.prolog.internal.socket;

import java.io.File;

import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceFactory;

/**
 */
public class Factory extends PrologInterfaceFactory {
	private static final String MAIN_PL = "main_socket.pl";

	private static final String SERVER_PL = "consult_server.pl";

	private Option[] options;

	public Factory() {

		options = new Option[] {
				new SimpleOption(SocketPrologInterface.EXECUTABLE,
						"SWI-Prolog executable", "eg. xpce or /usr/bin/xpce",
						SimpleOption.FILE, guessExecutableName()),
				// new SimpleOption(
				// SocketPrologInterface.ENGINE_DIR,
				// "PIF engine directory",
				// "There are a couple of prolog files that need to be
				// temporarily" +
				// "stored somewhere during bootstrapping of the prolog
				// interface." +
				// "Any directory to which you have write permissions will be
				// fine.",
				// SimpleOption.DIR, null) {
				// public String getDefault() {
				// File f =
				// getResourceLocator().resolve("");
				// return Util.prologFileName(f);
				// }
				// },
				new SimpleOption(
						SocketPrologInterface.STANDALONE,
						"stand-alone server",
						"If true, the PIF will not try to start and stop its own server process.",
						SimpleOption.FLAG, guessStandAlone()) {
					public boolean isVisible() {
						return false;
					}
				},
				new SimpleOption(SocketPrologInterface.HOST, "Server host",
						"The host the PIF server is listening on",
						SimpleOption.STRING, null) {
					public boolean isVisible() {
						return false;
					}
				},
				new SimpleOption(SocketPrologInterface.PORT, "Server port",
						"The port the PIF server is listening on",
						SimpleOption.NUMBER, "9944") {
					public boolean isVisible() {
						return false;
					}
				},
				new SimpleOption(
						SocketPrologInterface.TIMEOUT,
						"Connect Timeout",
						"Maximum time in milliseconds to wait for the prolog process to come up.",
						SimpleOption.NUMBER, "15000"),
				new SimpleOption(
						SocketPrologInterface.USE_POOL,
						"Use session pooling",
						"If true, the PIF will try to pool and reuse disposed sessions to reduce connection overhead.",
						SimpleOption.FLAG, guessUsePool()),
				new SimpleOption(
						SocketPrologInterface.HIDE_PLWIN,
						"Hide plwin (windows only)",
						"Usefull for windows users who are tired of plwin windows cluttering their system tray."
								+ "\n Note: this only works with the plwin executable.",
						SimpleOption.FLAG, "true") };
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologInterfaceFactory#create()
	 */
	public PrologInterface create() {
		ensureInstalled(SERVER_PL, Factory.class);
		ensureInstalled(MAIN_PL, Factory.class);

		SocketPrologInterface pif = new SocketPrologInterface(this);
		pif.getBootstrapLibraries().add(
				Util.prologFileName(getResourceLocator().resolve(SERVER_PL)));
		pif.getBootstrapLibraries().add(
				Util.prologFileName(getResourceLocator().resolve(MAIN_PL)));
		pif.setStartAndStopStrategy(new SocketServerStartAndStopStrategy());
		for (int i = 0; i < options.length; i++) {
			pif.setOption(options[i].getId(), options[i].getDefault());
		}
		return pif;
	}

	/**
	 * @return
	 */
	private static String guessUsePool() {
		return "true";
	}

	/**
	 * @return
	 */
	private static String guessStandAlone() {
		return "false";
	}

	private String guessExecutableName() {

		if (Util.isWindoze()) {
			return "cmd.exe /c start /min plwin";
		}
		return "xpce";
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologInterfaceFactory#getOptions()
	 */
	public Option[] getOptions() {
		return options;
	}
}
