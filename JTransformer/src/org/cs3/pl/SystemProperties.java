/*
 * Created on 26.09.2003
 *
 * Code copied from: http://www.rgagnon.com/javadetails/java-0150.html
 */
package org.cs3.pl;

import java.io.*;
import java.util.*;

/**
 * @author xproot
 *
 * To change the template for this generated type comment go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class SystemProperties {

    
    public final Properties properties;
    private boolean WINDOWSPLATFORM;
	private boolean LINUXPLATFORM;

    public boolean isWindowsPlattform() {
        return WINDOWSPLATFORM;
    }
    /**
	 * @return
	 */
	public boolean isLinuxPlatform() {
		return LINUXPLATFORM;
	}
    

    public SystemProperties() {
        Process p = null;
        Properties envVars = new Properties();
        Runtime r = Runtime.getRuntime();
        String OS = System.getProperty("os.name");
        // System.out.println(OS);
        
        if (OS.startsWith("Windows")){
        	WINDOWSPLATFORM = true;
        	LINUXPLATFORM = false;
        } else if (OS.startsWith("Linux")){
        	LINUXPLATFORM = true;
			WINDOWSPLATFORM = false;
        } else {
        	WINDOWSPLATFORM = false;
        	LINUXPLATFORM = false;
        }
        
        try {
            p = getProcess(r, OS);
            readProperties(p, envVars);
        } catch (IOException e) {
            throw new RuntimeException("Cannot read environment variables. Your plattform is unknown or an I/O Error occured.");
        }
        
        

        properties = envVars;
    }

    private void readProperties(Process p, Properties envVars) throws IOException {
        BufferedReader br =
            new BufferedReader(new InputStreamReader(p.getInputStream()));
        String line;
        while ((line = br.readLine()) != null) {
            int idx = line.indexOf('=');
            String key = line.substring(0, idx);
            String value = line.substring(idx + 1);
            envVars.setProperty(key, value);
            //System.out.println( key + " = " + value );
        }
    }

    private Process getProcess(Runtime r, String OS) throws IOException {
        Process p;
        if (WINDOWSPLATFORM)
        	if (OS.indexOf("ows 9") > -1)
        		p = r.exec("cmd.com /c set");
        	else 
        		p = r.exec("cmd.exe /c set");
        else if (LINUXPLATFORM){
            p = r.exec("env");
        } else
        	p = null;
        
        return p;
    }

}
