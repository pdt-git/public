package org.cs3.pdt.internal.hooks;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.PDT;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.PrologException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;

public class ConsultServerHook implements LifeCycleHook{
	
	
	public static final String HOOK_ID = "org.cs3.pdt.internal.hooks.ConsultServerHook";

	 private static int getPort() {
	        IPreferencesService service = Platform.getPreferencesService();
	        String qualifier = PDTPlugin.getDefault().getBundle().getSymbolicName();
	        int port= service.getInt(qualifier,PDT.PREF_CONSULT_PORT,-1,null);       
	    	if(port==-1){
	    		throw new NullPointerException("Required property \""+PDT.PREF_CONSULT_PORT+"\" was not specified.");
	    	}
			return port;
		}
	
    public void onInit(PrologSession s) {
        
        int port = getPort();
		if (Util.probePort(port)) {
			Debug.info("Consult server thread seems to be running, so i will not start a new one.");			
		}else{
		    String queryString = "consult_server("+port+")";
		    Debug.info("starting consult server using: "+queryString);
		   		
                try {
                    s.query(queryString);
                }catch (PrologException e) {
                    Debug.report(e);
                    throw new RuntimeException(e);
                }			
			while(!Util.probePort(port)){
	             try {
	                 Thread.sleep(50);
	             } catch (InterruptedException e1) {
	                 Debug.report(e1);
	                 throw new RuntimeException(e1);
	             }
	         }
			Debug.debug("Server thread created");
		}
		
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.ShutdownHook#beforeShutDown(org.cs3.pl.prolog.PrologSession)
	 */
	public void beforeShutdown(PrologSession session) {
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
	 */
	public void afterInit() {

		
	}

	

	

}