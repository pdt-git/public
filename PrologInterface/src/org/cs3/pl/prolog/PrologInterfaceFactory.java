/*
 */
package org.cs3.pl.prolog;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;


public abstract class PrologInterfaceFactory {
   
    public final static String DEFAULT="org.cs3.pl.prolog.internal.socket.Factory";

    private ResourceFileLocator locator= new DefaultResourceFileLocator().subLocator(".PrologInterface");

    public static PrologInterfaceFactory newInstance(){
        return newInstance(DEFAULT);
    }
    
    public static PrologInterfaceFactory newInstance(String fqn) {
        try{
            Class impl = Class.forName(fqn);
        
        if(!PrologInterfaceFactory.class.isAssignableFrom(impl)){
            throw new IllegalArgumentException("not a valid factory class");
        }
        return (PrologInterfaceFactory) impl.newInstance();
        }
        catch(Throwable t){
            Debug.report(t);
            throw new RuntimeException(t);
        }
    }
    
    public abstract PrologInterface create();
    
    public abstract Option[] getOptions();
    
    public void setResourceLocator(ResourceFileLocator locator){
        this.locator=locator;
    }
    
    public ResourceFileLocator getResourceLocator(){
        return locator;
    }

    
    public File ensureInstalled(String res, Class clazz){
        File f = locator.resolve(res);
        //XXX only temporarily! this should be removed for better performance. --lu
        if(f.exists()){
            f.delete();
        }
        if (!f.exists()){
            f.getParentFile().mkdirs();
            try {
                BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(f));
                InputStream in = clazz.getResourceAsStream(res);
                Util.copy(in,out);
                in.close();
                out.close();            
            } catch (IOException e) {
                Debug.report(e);
              throw new RuntimeException(e);
            }
        }
        return f;
    }
    
    
    private List bootstrapLibraries = new Vector(); 
    private List getBootstrapLIbraries(){
        return bootstrapLibraries;
    }
    
   
}
