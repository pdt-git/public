/*
 */
package org.cs3.pl.common;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;

/**
 * Canonical implementation.
 * This implementation uses a given directory URI as root
 * of its resource tree. By default it uses the user home directory.
 */
public class DefaultResourceFileLocator implements ResourceFileLocator {
    URI root = new File(System.getProperty("user.home")).toURI();
   public DefaultResourceFileLocator(){
       this(new File(System.getProperty("user.home")));
   }
    
   
    /**
     * @param root
     */
    public DefaultResourceFileLocator(File root) {
        String rootString = root .toURI().toString();
        try {
            this.root=new URI(rootString.endsWith("/")? rootString:rootString+"/");
        } catch (URISyntaxException e) {
            Debug.report(e);
            throw new RuntimeException(e);
        }
    }
    public ResourceFileLocator subLocator(String subdir){
        return new DefaultResourceFileLocator(resolve(subdir));
    }
    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.common.ResourceLocator#resolve(java.lang.String)
     */
    public File resolve(String rel) {
       
            File resolved = new File(root.resolve(rel));
            return resolved;

       
    }

    public static void main(String[] args) {
        File file = new DefaultResourceFileLocator().resolve(".hallo");        
        System.out.println(file.toString());
    }

    
}
