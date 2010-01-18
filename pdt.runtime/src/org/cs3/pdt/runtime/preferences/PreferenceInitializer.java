package org.cs3.pdt.runtime.preferences;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;

import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	protected PrologRuntimePlugin plugin;
	protected IPreferenceStore store;
	
	
	/*
	 * (non-Javadoc)
	 * 
	 * @seeorg.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#
	 * initializeDefaultPreferences()
	 */
	public void initializeDefaultPreferences() {
		plugin = PrologRuntimePlugin.getDefault();
		store = plugin.getPreferenceStore();

		
		store.setDefault(PrologInterface.PREF_FILE_SEARCH_PATH, plugin.guessFileSearchPath("pdt.runtime.socket.codebase"));
		
//		store.setDefault(PrologRuntime.PREF_PROLOGIF_IMPLEMENTATION, AbstractPrologInterface.PL_INTERFACE_DEFAULT);
		store.setDefault(PrologRuntime.PREF_PIF_BOOTSTRAP_DIR, System.getProperty("java.io.tmpdir"));
		
		store.setDefault(PrologInterface.PREF_EXECUTABLE, guessExecutableName());
		store.setDefault(PrologInterface.PREF_ENVIRONMENT, guessEnvironmentVariables());
		store.setDefault(PrologInterface.PREF_STANDALONE, guessStandAlone());
		store.setDefault(PrologInterface.PREF_HOST, "localhost");
		
		store.setDefault(PrologInterface.PREF_TIMEOUT,15000 );
		
		

	}

	
	private static String guessStandAlone() {
		return "false";
	}

	private String guessEnvironmentVariables() {
		if (Util.isMacOS()) {
			String home = System.getProperty("user.home");
			return "DISPLAY=:0.0, HOME=" + home;
		}
		return "";
	}
	
	public static String guessExecutableName() {

		if (Util.isWindows()) {
			return "cmd.exe /c start \"cmdwindow\" /min "
					+ findWindowsExecutable() + " " + PrologInterface.STACK_COMMMAND_LINE_PARAMETERS;
			// return "plwin";
		}
		// return "xterm -e xpce"; // For Mac and Linux with console
		return findUnixExecutable() + " " + PrologInterface.STACK_COMMMAND_LINE_PARAMETERS;
	}
	
	/**
	 * @author Hasan Abdel Halim
	 * 
	 * Finds the current SWI-Prolog executable for UNIX/BSD-BASED OS
	 * @return the complete path of the executable otherwise it will return xpce
	 */
	private static String findUnixExecutable() {
		String default_exec = "xpce";
		String xpce = default_exec;

		// TODO shall we look for the env. variables as we do for Windows ?
		String[] appendPath = null;

		// Hack to resolve the issue of locating xpce in MacOS
		if (Util.isMacOS()) {
			appendPath = new String[1];
			appendPath[0] = "PATH=PATH:/opt/local/bin";
		}

		try {
			Process process = Runtime.getRuntime().exec(
					"which " + default_exec, appendPath);

			if (process == null)
				return null;

			BufferedReader br = new BufferedReader(new InputStreamReader(
					process.getInputStream()));
			String path = br.readLine();

			if (path == null || path.startsWith("no " + default_exec))
				return default_exec;

			xpce = path;

			return xpce;

		} catch (IOException e) {

			return default_exec;
		}
	}

	/**
	 * @author Hasan Abdel Halim
	 * 
	 * Finds the current SWI-Prolog executable for Windoze OS
	 * @return the complete path of the executable otherwise it will return
	 *         plwin
	 */
	private static String findWindowsExecutable() {
		String default_exec = PrologInterface.WINDOWS_EXECUTABLE;
		String plwin = default_exec;

		String path;
		try {

			Process process = Runtime.getRuntime().exec(
					"cmd.exe /c echo %PATH%");

			if (process == null)
				return default_exec;

			BufferedReader br = new BufferedReader(new InputStreamReader(
					process.getInputStream()));
			path = br.readLine();

			if (path == null)
				return default_exec;

			// TODO just search in case of executable was not found.
			String[] paths = Util.split(path, ";");
			File exeFile = null;

			for (int i = 0; i < paths.length; i++) {

				if (default_exec.indexOf(".exe") == -1)
					default_exec += ".exe";

				String currPath = paths[i] + "\\" + default_exec;
				exeFile = new File(currPath);

				if (exeFile.exists()) {
					plwin = "\"" + currPath + "\"";
					break;
				}
			}

			return plwin;

		} catch (IOException e) {

			return default_exec;
		}
	}
	
	
}
