/*
 * Created on 15.09.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.prolog;

import java.io.IOException;

import org.cs3.pl.Debug;
import org.cs3.pl.extension.IJTransformerObserver;
import org.cs3.pl.fileops.MetaDataManagerFactory;
import org.cs3.pl.fileops.PrologMetaDataManager;
import org.eclipse.core.runtime.CoreException;

/**
 * @author rho
 *
 */
public class PreloadDocumentationFacts implements IJTransformerObserver{
    
    public void update(int kind,Object[] info) {
        if(kind == IJTransformerObserver.JT_ENGINE_STARTUP){
            IPrologClient client = (IPrologClient)info[0];
        PrologMetaDataManager manager;
        try {
            manager = MetaDataManagerFactory.getPrologMetaDataManager(
                    client, PrologMetaDataManager.MODEL);
                    		manager.readFactbaseMetaData();
        } catch (IOException e) {
            Debug.report(e);
        } catch (CoreException e) {
            Debug.report(e);
        }
        
        }
    }
}
