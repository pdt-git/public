/*
 */
package org.cs3.pdt.preferences;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Properties;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;

/**
 * this class tries to guess sensible defaults for most  property values.
 */
public class Initializer extends AbstractPreferenceInitializer {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
     */
    public void initializeDefaultPreferences() {
        String fileSep = File.separator;
        String pathSep = File.pathSeparator;
        String location="";
        try {
            location = getLocation();            
        } catch (IOException e) {
            Debug.report(e);
            Debug.error("Could not find plugin installation dir.");
        }
        
        
        //System Properties may override our hardcoded defaults.
        Integer.getInteger(Properties.CONSOLE_PORT,-1);
        int consolePort=Integer.getInteger(Properties.CONSOLE_PORT,4711).intValue();
        int serverPort=Integer.getInteger(Properties.SERVER_PORT,4143).intValue();
        String consultPath=System.getProperty(Properties.CONSULT_PATH,"");
        boolean serverStandAlone=Boolean.getBoolean(Properties.SERVER_STANDALONE);        
        boolean sessionPooling=Boolean.getBoolean(Properties.USE_SESSION_POOLING);        
        boolean consultRecursive=Boolean.getBoolean(Properties.CONSULT_RECURSIVE);
        String debugLevel=System.getProperty(Properties.DEBUG_LEVEL);        
        String metadataEngineDir = System.getProperty(Properties.METADATA_ENGINE_DIR,location+ fileSep+"engine");
        String swiplDir=System.getProperty(Properties.SWIPL_DIR,location +fileSep +"swipl");
        String serverClasspath = System.getProperty(Properties.SERVER_CLASSPATH,
                  getLibDir()+fileSep+"PrologInterface.jar"+pathSep
                +getLibDir()+fileSep+"RaPlaRPC.jar"+pathSep
                +getLibDir()+fileSep+"Common.jar"+pathSep
                +swiplDir+fileSep+"lib"+fileSep+"jpl.jar");
        String qualifier = PDTPlugin.getDefault().getBundle().getSymbolicName();      

        IScopeContext context = new DefaultScope();

        IEclipsePreferences node = context.getNode(qualifier);
        if(node==null){
            Debug.error("Häh?!");
        }else {
            node.putInt(Properties.CONSOLE_PORT,consolePort);
            node.put(Properties.CONSULT_PATH,consultPath);
            node.putBoolean(Properties.CONSULT_RECURSIVE,consultRecursive);
            node.put(Properties.DEBUG_LEVEL,debugLevel);
            node.put(Properties.METADATA_ENGINE_DIR,metadataEngineDir);
            node.put(Properties.SERVER_CLASSPATH,serverClasspath);
            node.putInt(Properties.SERVER_PORT,serverPort);
            node.putBoolean(Properties.SERVER_STANDALONE,serverStandAlone);
            node.put(Properties.SWIPL_DIR,swiplDir);
            node.putBoolean(Properties.USE_SESSION_POOLING,sessionPooling);
        }

    }

    
    
    private String getLocation() throws IOException {
		URL url = PDTPlugin.getDefault().getBundle().getEntry("/");
		String location = null;
			location = new File(Platform.asLocalURL(url).getFile()).getAbsolutePath();
		if (location.charAt(location.length()-1)== File.separatorChar)
			location = location.substring(0,location.length()-1);
		return location;
	}
    
    private  String getLibDir() {
		String proj;
		String cp;
				
		String sep = System.getProperty("file.separator");
		try {
			if (getLocation() != null){
				return getLocation() +sep+ "lib";
			}
		} catch (IOException e){
			// ok, then the other way
		}
		
		
		String resName = "org/cs3/pdt/PDTPlugin.class";
		URL url = ClassLoader.getSystemClassLoader().getResource(resName);
		String path = url.getFile();
		
		return path.substring(0, path.indexOf(resName))+sep+ "lib";
	}
}