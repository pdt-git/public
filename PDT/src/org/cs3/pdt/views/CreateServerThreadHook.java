package org.cs3.pdt.views;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.InitHook;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.SessionException;

/**
 * An InitHook that takes the neccesary steps to open
 * an io socket on the server process to which a 
 * Console can be connected.
 */
public class CreateServerThreadHook implements InitHook {
	

	
	public void onInit(PrologSession s) {
		if (Util.probePort(5567, "end_of_file.\n")) {
			Debug
					.info("Console server thread seems to be running, so i will not start a new one.");
			return;
		}
		try {
			s
					.query("use_module(library(prolog_server)), prolog_server(5567, [])");
		} catch (SessionException e) {
			Debug.report(e);
		}
		Debug.debug("Server thread created");
	}

	

}