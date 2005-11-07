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
	public static void configureFileSearchPath(PrologLibraryManager mgr,PrologSession session, String[] libIds){
		
		Stack todo = new Stack();
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < libIds.length; i++) {
			if(mgr.resolveLibrary(libIds[i])==null){
				throw new IllegalArgumentException("library id "+libIds[i]+" is unresolved");
			}
			if(mgr.getBrokenLibraries().contains(libIds[i])){
				throw new IllegalArgumentException("library id "+libIds[i]+" is broken");
			}
			todo.add(libIds[i]);			
		}
		
		while(!todo.isEmpty()){
			String key = (String) todo.pop();
			PrologLibrary lib= mgr.resolveLibrary(key);
			if(lib==null){
				//this should not happen
				throw new IllegalStateException("unresoved: "+key+". Bug in LibraryManager?");
			}
			if(sb.length()>0){
				sb.append(',');
			}
			sb.append("(	user:file_search_path("+lib.getAlias()+", '"+lib.getPath()+"')" +
					  "->	true" +
					  ";	user:assert(file_search_path("+lib.getAlias()+", '"+lib.getPath()+"'))" +
					  ")");
		}
		
		
		session.queryOnce(sb.toString());
		
	}
}

