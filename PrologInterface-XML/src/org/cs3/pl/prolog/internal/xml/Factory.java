/*
 */
package org.cs3.pl.prolog.internal.xml;

import java.io.File;

import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceFactory;

/**
 */
public class Factory extends PrologInterfaceFactory {
    
    private static final String SERVER_PL = "prolog_interface.pl";

    private Option[] options;

    public Factory() {

        options = new Option[] {
                new SimpleOption(XMLPrologInterface.EXECUTABLE,
                        "SWI-Prolog executable", "eg. xpce or /usr/bin/xpce",
                        SimpleOption.FILE, guessExecutableName()),
               
                new SimpleOption(XMLPrologInterface.PORT, "Server port",
                        "The port the PIF server is listening on",
                        SimpleOption.NUMBER, guessPort())};
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologInterfaceFactory#create()
     */
    public PrologInterface create() {
        ensureInstalled(SERVER_PL, Factory.class);
        

        XMLPrologInterface pif = new XMLPrologInterface(this);
        pif.getBootstrapLibraries().add(Util.prologFileName(getResourceLocator().resolve(SERVER_PL)));        
        pif.setStartAndStopStrategy(new XMLServerStartAndStopStrategy());
        for (int i = 0; i < options.length; i++) {
            pif.setOption(options[i].getId(), options[i].getDefault());
        }
        return pif;
    }

   

    /**
     * @return
     */
    private static String guessPort() {
        return "9944";
    }

    private String guessExecutableName() {
        String osname = System.getProperty("os.name");
        if (osname.indexOf("Windows") > -1) {
            return "plwin";
        }
        return "xpce";
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologInterfaceFactory#getOptions()
     */
    public Option[] getOptions() {
        return options;
    }
}
