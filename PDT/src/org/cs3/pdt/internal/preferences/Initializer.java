/*
 */
package org.cs3.pdt.internal.preferences;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URL;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.osgi.service.prefs.BackingStoreException;

/**
 * this class tries to guess sensible defaults for most property values.
 */
public class Initializer extends AbstractPreferenceInitializer {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
     */
    public void initializeDefaultPreferences() {
        try {
            initializeDefaultPreferences_impl();
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t.getMessage(), t);
        }
    }

    /**
     * @throws BackingStoreException
     * @throws IOException
     * @throws InterruptedException
     *  
     */
    private void initializeDefaultPreferences_impl() throws BackingStoreException, IOException, InterruptedException {
//        File logFile = Util.getLogFile("org.cs3.pdt.client.log");        
//        System.out.println("The client debug output is safed in "
//                + logFile.getAbsolutePath());
//        BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(
//                new FileOutputStream(logFile));
//        Debug.setOutputStream(new PrintStream(bufferedOutputStream));
        Debug.setDebugLevel(Debug.LEVEL_DEBUG);

        String fileSep = File.separator;
        String pathSep = File.pathSeparator;
        String location = "";
        try {
            location = getLocation();
        } catch (IOException e) {
            Debug.report(e);
            Debug.error("Could not find plugin installation dir.");
        }

        //System Properties may override our hardcoded defaults.
        Integer.getInteger(PDT.PREF_CONSOLE_PORT, -1);
        int consolePort = Integer.getInteger(PDT.PREF_CONSOLE_PORT, 4711)
                .intValue();
        int serverPort = Integer.getInteger(PDT.PREF_SERVER_PORT, 4143)
                .intValue();
      
        String consultPath = System.getProperty(PDT.PREF_CONSULT_PATH, "");
        boolean serverStandAlone = Boolean
                .getBoolean(PDT.PREF_SERVER_STANDALONE);
//        boolean sessionPooling = Boolean
//                .getBoolean(PDT.PREF_USE_SESSION_POOLING);
        boolean sessionPooling = Boolean.valueOf(System.getProperty(PDT.PREF_USE_SESSION_POOLING,"true")).booleanValue();
        boolean consultRecursive = Boolean
                .getBoolean(PDT.PREF_CONSULT_RECURSIVE);
        String debugLevel = System.getProperty(PDT.PREF_DEBUG_LEVEL,"DEBUG");
        String metadataEngineDir = System.getProperty(
                PDT.PREF_METADATA_ENGINE_DIR, location + fileSep + "engine");
        String metadataStoreDir = System.getProperty(
                PDT.PREF_METADATA_STORE_DIR, location + fileSep + "store");
//        String swiplDir = System.getProperty(PDT.PREF_SWIPL_DIR, location
//              
        String swiplDir = System.getProperty(PDT.PREF_SWIPL_DIR, guessSwiLocation());
        String serverClasspath = System.getProperty(PDT.PREF_SERVER_CLASSPATH,
                getLibDir() + fileSep + "PrologInterface.jar" + pathSep
                        + getLibDir() + fileSep + "RaPlaRPC.jar" + pathSep
                        + getLibDir() + fileSep + "Common.jar" + pathSep
                        + swiplDir + fileSep + "lib" + fileSep + "jpl.jar");
        String sourcePathDefault = System.getProperty(
                PDT.PREF_SOURCE_PATH_DEFAULT, "/pl");

        String qualifier = PDTPlugin.getDefault().getBundle().getSymbolicName();

        String executable = System.getProperty(PDT.PREF_SWIPL_EXECUTABLE,guessExecutableName());
        
        IScopeContext context = new DefaultScope();

        IEclipsePreferences node = context.getNode(qualifier);
        if (node == null) {
            Debug.error("Häh?!");
        } else {
            node.putInt(PDT.PREF_CONSOLE_PORT, consolePort);            
            node.put(PDT.PREF_CONSULT_PATH, consultPath);            
            node.putBoolean(PDT.PREF_CONSULT_RECURSIVE, consultRecursive);            
            node.put(PDT.PREF_DEBUG_LEVEL, debugLevel);            
            node.put(PDT.PREF_METADATA_ENGINE_DIR, metadataEngineDir);
            node.put(PDT.PREF_METADATA_STORE_DIR, metadataStoreDir);
            node.put(PDT.PREF_SERVER_CLASSPATH, serverClasspath);
            node.putInt(PDT.PREF_SERVER_PORT, serverPort);
            node.putBoolean(PDT.PREF_SERVER_STANDALONE, serverStandAlone);
            node.put(PDT.PREF_SWIPL_DIR, swiplDir);
            node.putBoolean(PDT.PREF_USE_SESSION_POOLING, sessionPooling);
            node.put(PDT.PREF_SOURCE_PATH_DEFAULT, sourcePathDefault);
            node.put(PDT.PREF_SWIPL_EXECUTABLE, executable);
            String[] strings = node.keys();
            for (int i = 0; i < strings.length; i++) {
                Debug.info(strings[i]+" --> "+node.get(strings[i],"n.a."));
            }
        }
        debugLevel = node.get(PDT.PREF_DEBUG_LEVEL, "ERROR");
        System.out
                .println("Hi. I'm the PDT's Preferences Initializer, and i am now setting the Debug Level to "
                        + debugLevel);
                
                //Debug.setDebugLevel(debugLevel);
        Debug.setDebugLevel("DEBUG");
    }

    private String guessExecutableName(){
        String osname = System.getProperty("os.name");
        if(osname.indexOf("Windows")>-1){
            return "plwin";
        }
        return "xpce";
    }

    /**
     *only implemented for linux right now. other platforms should be easy.
     *other platforms will be added. Until then they assume an swi location
     *in the install dir of the plugin.
     * @return
     * @throws InterruptedException
     * @throws InterruptedException
     * @throws IOException
     */
    private String guessSwiLocation() throws IOException, InterruptedException{
        String sep = System.getProperty("file.separator");
        String osname = System.getProperty("os.name");
        if("Linux".equals(osname)){
            String location = Util.exec("xpce -g source_file(A),concat(X,'/library/system.pl',A),format(\"~a~n\",[X]),halt.")[0].trim();
            if(! "".equals(location)){
                return location;
            }
        }
        return getLocation()+sep+"swipl";
    }

    private String getLocation() throws IOException {
        URL url = PDTPlugin.getDefault().getBundle().getEntry("/");
        String location = null;
        location = new File(Platform.asLocalURL(url).getFile())
                .getAbsolutePath();
        if (location.charAt(location.length() - 1) == File.separatorChar)
            location = location.substring(0, location.length() - 1);
        return location;
    }

    private String getLibDir() {
        String proj;
        String cp;

        String sep = System.getProperty("file.separator");
        try {
            if (getLocation() != null) {
                return getLocation() + sep + "lib";
            }
        } catch (IOException e) {
            // ok, then the other way
        }

        String resName = "org/cs3/pdt/PDTPlugin.class";
        URL url = ClassLoader.getSystemClassLoader().getResource(resName);
        String path = url.getFile();

        return path.substring(0, path.indexOf(resName)) + sep + "lib";
    }
}