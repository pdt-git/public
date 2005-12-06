package org.cs3.pdt.runtime;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

import org.cs3.pl.prolog.PrologSession;



/**
 * some frequently used code fragments related to the setup
 * of prolog runtimes.
 *
 */
public class PLUtil {
	

	public static void configureFileSearchPath(PrologLibraryManager mgr,PrologSession session, String[] libIds){
		

		StringBuffer sb = new StringBuffer();
		PrologLibrary[] required = getRequiredLibs(mgr,libIds);
		for (int i = 0; i < required.length; i++) {
			PrologLibrary lib = required[i];
			
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
	
public static PrologLibrary[] getRequiredLibs(PrologLibraryManager mgr, String[] libIds){
		
		Stack todo = new Stack();
		Set required = new HashSet();
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
			if(!required.contains(lib)){
				required.add(lib);
				todo.addAll(lib.getDependencies());
			}
			
		}
		
		return (PrologLibrary[]) required.toArray(new PrologLibrary[required.size()]);
	}
}

