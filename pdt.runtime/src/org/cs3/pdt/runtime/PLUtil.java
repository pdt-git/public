package org.cs3.pdt.runtime;

import java.util.Stack;

import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;

/**
 * some frequently used code fragments related to the setup
 * of prolog runtimes.
 *
 */
public class PLUtil {
	public static void configureLibraries(PrologInterface pif, String[] libIds){
		PrologLibraryManager mgr=PrologRuntimePlugin.getDefault().getLibraryManager();
		Stack todo = new Stack();
		
		for (int i = 0; i < libIds.length; i++) {
			todo.add(libIds[i]);			
		}
		
		while(!todo.isEmpty()){
			
		}
		PrologSession s = pif.getSession();
		try{
			
		}finally{
			if(s!=null){
				s.dispose();
			}
		}
	}
}

