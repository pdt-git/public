/*
 */
package org.cs3.pl.prolog.internal.xml;

import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;
import org.cs3.pl.prolog.internal.DefaultConsultService;
import org.cs3.pl.prolog.internal.ReusablePool;

/**
 */
public class XMLPrologInterface extends AbstractPrologInterface {

    private ReusablePool pool = null;
    
    private ResourceFileLocator locator;

    private PrologInterfaceFactory factory;

    public static final String EXECUTABLE = "pif.executable";
    private String executable=null;

    public static final String PORT = "pif.port";

    private int port=12345;

    

    /**
     *  
     */
    public XMLPrologInterface(PrologInterfaceFactory factory) {
        this.factory=factory;
        this.locator=factory.getResourceLocator();
        pool=new ReusablePool( );
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#getSession_impl()
     */
    public PrologSession getSession_impl() throws Throwable {
       XMLClient client = (XMLClient) pool.findInstance(XMLClient.class);
       if(client==null){
           client = new XMLClient(Integer.parseInt(getOption(PORT)));
           client.connect();
       }
        return new XMLSession(pool,client,this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologInterface#getConsultService(java.lang.String)
     */
    public ConsultService getConsultService(String prefix) {
        return new DefaultConsultService(this, locator.subLocator(prefix));
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologInterface#getFactory()
     */
    public PrologInterfaceFactory getFactory() {
        return factory;
    }

    /**
     * @return Returns the locator.
     */
    public ResourceFileLocator getLocator() {
        return locator;
    }

    /**
     * @param locator
     *                  The locator to set.
     */
    public void setLocator(ResourceFileLocator locator) {
        this.locator = locator;
    }
    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologInterface#getOption(java.lang.String)
     */
    public String getOption(String opt) {
       if(PORT.equals(opt)){
           return ""+port;
       }
       if(EXECUTABLE.equals(opt)){
        return executable;
       }
        return super.getOption(opt);
    }
    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologInterface#setOption(java.lang.String, java.lang.String)
     */
    public void setOption(String opt, String value) {
        if(PORT.equals(opt)){
           port=Integer.parseInt(value);
        }
        else if(EXECUTABLE.equals(opt)){
            executable=value;
        }
        else{
            super.setOption(opt, value);
        }
    }
}
