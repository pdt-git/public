import java.io.FileNotFoundException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Properties;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologSession;

/**
 * this hool consults pl of the jtransofmer engine.
 */
public class ConsultEngineDirHook implements LifeCycleHook {
        public void onInit(PrologSession s) {
        try {
            s.consult(getEngineDir()+"/main.pl");
        } catch (FileNotFoundException e) {
            Debug.report(e);
        }

    }

    private static String getEngineDir() throws FileNotFoundException {
		
    	String engine = System.getProperty(Properties.ENGINE_DIR);
    	if(engine==null){
    		throw new NullPointerException("Required property \""+Properties.ENGINE_DIR+"\" was not specified.");
    	}
		return engine;
	}
 /*   
    private static String getProjectDir() throws FileNotFoundException {
		try {
			return PDTPlugin.getDefault().getLocation().replace('/',
					File.separatorChar);
		} catch (NullPointerException npe) {
			Debug.warning("i cought a NullPointerException in getProjectDir(), propably because" +
					"this code still excpects eclipse to be running, while it actualy should not depend on " +
					"it."); 
		}
		String resName = "org/cs3/pl/prolog/PrologServer.class";
		URL url = ClassLoader.getSystemClassLoader().getResource(resName);
		String path = url.getPath();
		
		String pdtDir;
		if(System.getProperty("os.name").startsWith("Windows")){ 
		    pdtDir = path.substring(1, path.length() - resName.length());
		}
		else{
			pdtDir = path;
		}
		//now we should have a absolute filesystem path to the PrologServer class file.
		//we have to strip away a substring with the same length of resName + "bin/".length 
		//from the end of pdtDir to reconstruct the project dir.
		pdtDir=pdtDir.substring(0,pdtDir.length()-resName.length()-"bin/".length());
				
		
		//pdtDir = pdtDir.substring(0, pdtDir.length() - "bin".length() - 1);
		//pdtDir = pdtDir.replace('/',File.separatorChar);
		
		return pdtDir;
	}

   */

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
