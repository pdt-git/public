/*
 * Created on 11.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.InitHook;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.SessionException;

/**
 * @author schulzs1
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
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