import java.io.FileNotFoundException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;

/**
 * this hool consults pl of the jtransofmer engine.
 */
public class MetaDataEngineHook implements LifeCycleHook {
    public static final String HOOK_ID = "org.cs3.pdt.hooks.MetaDataEngineHook";
        public void onInit(PrologInterface pif,PrologSession s) {
        try {
            s.consult(getEngineDir()+"/main.pl");
        } catch (FileNotFoundException e) {
            Debug.report(e);
        }

    }

    private static String getEngineDir() throws FileNotFoundException {
        
        String engine = System.getProperty(PDT.PREF_METADATA_ENGINE_DIR,"");       
    	if(engine==null){
    		throw new NullPointerException("Required property \""+PDT.PREF_CONSULT_PATH+"\" was not specified.");
    	}
		return engine;
	}
 

 

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
	 */
	public void afterInit(PrologInterface pif) {
		
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
	 */
	public void beforeShutdown(PrologInterface pif,PrologSession session) {
		
	} 
}
