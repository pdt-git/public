package org.cs3.prolog.connector.common;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;

public class ProcessUtils {

	public static String guessEnvironmentVariables() {
		if (Util.isMacOS()) {
			String home = System.getProperty("user.home");
			return "DISPLAY=:0.0, HOME=" + home;
		}
		return "";
	}

	public static String getInvocationCommand() {
		if (Util.isWindows()) {
			return "cmd.exe /c start \"cmdwindow\" /min ";
		} else {
			return "";
		}
	}

	public static String getExecutablePreference() {
		return ProcessUtils.getExecutablePreference(PDTConstants.DIALECT_SWI);
	}

	public static String getExecutablePreference(String dialect) {
		if (PDTConstants.DIALECT_SWI.equals(dialect)) {
			if (Util.isWindows()) {
				return ProcessUtils.getWindowsExecutable(PDTConstants.WINDOWS_EXECUTABLES_SWI);
			} else {
				return ProcessUtils.getUnixExecutable(PDTConstants.UNIX_COMMAND_LINE_EXECUTABLES_SWI);
			}
		} else if (PDTConstants.DIALECT_YAP.equals(dialect)) {
			if (Util.isWindows()) {
				return ProcessUtils.getWindowsExecutable(PDTConstants.WINDOWS_EXECUTABLES_YAP);
			} else {
				return ProcessUtils.getUnixExecutable(PDTConstants.UNIX_COMMAND_LINE_EXECUTABLES_YAP);
			}
		} else {
			return "";
		}
	}

	/**
	 * Finds the current SWI-Prolog executable for UNIX/BSD-BASED OS
	 * @param unixCommandLineExecutables 
	 * @return the complete path of the executable otherwise it will return xpce
	 */
	public static String getUnixExecutable(String unixCommandLineExecutables) {
		String[] default_exec = unixCommandLineExecutables.split(",");
		
		// TODO shall we look for the env. variables as we do for Windows ?
		String[] appendPath = null;
	
		// Hack to resolve the issue of locating xpce in MacOS
		if (Util.isMacOS()) {
			appendPath = new String[1];
			appendPath[0] = "PATH=$PATH:" + System.getProperty("user.home") + "/bin:/opt/local/bin";
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
	 * Finds the current SWI-Prolog executable for Windows OS
	 * @param executables 
	 * @return the complete path of the executable otherwise it will return plwin
	 */
	public static String getWindowsExecutable(String executables) {
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
			String[] paths = Util.split(path, ";");
			File exeFile = null;
	
			for (int i = 0; i < paths.length; i++) {
				
				for (String exec : default_exec) {
					if (exec.indexOf(".exe") == -1)
						exec += ".exe";
	
					String currPath = paths[i] + "\\" + exec;
					exeFile = new File(currPath);
	
					if (exeFile.exists()) {
						plwin = currPath;
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

	public static String createExecutable(String invocation, String execution, String commandLineArguments, String startupFiles) {
		StringBuilder executable = new StringBuilder();
		if (invocation != null) {
			executable.append(invocation);
			executable.append(" ");
		}
		if (Util.isWindows()) {
			executable.append("\"");
		}
		executable.append(execution);
		if (Util.isWindows()) {
			executable.append("\"");
		}
		
		if (commandLineArguments != null && !commandLineArguments.isEmpty() && !commandLineArguments.trim().isEmpty()) {
			executable.append(" ");
			executable.append(commandLineArguments);
		}
		if (startupFiles != null && !startupFiles.isEmpty() && !startupFiles.trim().isEmpty()) {
			executable.append(" -s ");
			executable.append(startupFiles);
		}
		return executable.toString();
	}

	public static String getLogtalkStartupFile() {
		if (Util.isWindows()) {
			return "\"%LOGTALKHOME%\\integration\\logtalk_swi.pl\"";
		} else {
			String logtalkHome = System.getenv("LOGTALKHOME");
			if (logtalkHome != null) {
				return new File(logtalkHome, "integration/logtalk_swi.pl").getAbsolutePath();
			} else {
				return "";
			}
		}
	}

	public static String getLogtalkEnvironmentVariables() {
		if (Util.isWindows()) {
			return "";
		} else {
			StringBuffer buf = new StringBuffer();
			String guessedEnvironmentVariables = guessEnvironmentVariables();
			if (!guessedEnvironmentVariables.isEmpty()) {
				buf.append(guessedEnvironmentVariables);
				buf.append(", ");
			}
			buf.append("LOGTALKHOME=");
			buf.append(System.getenv("LOGTALKHOME"));
			buf.append(", ");
			buf.append("LOGTALKUSER=");
			buf.append(System.getenv("LOGTALKUSER"));
			return buf.toString();
		}
	}

}
