package org.cs3.pdt.internal.hooks;
import java.io.FileNotFoundException;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.PDT;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;

/**
 * this hool consults pl of the jtransofmer engine.
 */
public class MetaDataEngineHook implements LifeCycleHook {
    public static final String HOOK_ID = "org.cs3.pdt.internal.hooks.MetaDataEngineHook";
        public void onInit(PrologSession s) {
        try {
            s.consult(getEngineDir()+"/main.pl");
        } catch (FileNotFoundException e) {
            Debug.report(e);
        }

    }

    private static String getEngineDir() throws FileNotFoundException {
        IPreferencesService service = Platform.getPreferencesService();
        String qualifier = PDTPlugin.getDefault().getBundle().getSymbolicName();
        String engine = service.getString(qualifier,PDT.PREF_METADATA_ENGINE_DIR,null,null);       
    	if(engine==null){
    		throw new NullPointerException("Required property \""+PDT.PREF_METADATA_ENGINE_DIR+"\" was not specified.");
    	}
		return engine;
	}
 

 

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
	 */
	public void afterInit() {
		
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
	 */
	public void beforeShutdown(PrologSession session) {
		
	} 
}
