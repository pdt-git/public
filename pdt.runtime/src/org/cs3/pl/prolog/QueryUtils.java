/**
 * 
 * @partOf PDT Framework
 * @url http://sewiki.iai.uni-bonn.de/research/pdt/
 * @author Patrick Rypalla - patrick@rypalla.de
 */
package org.cs3.pl.prolog;

import java.util.ArrayList;
import java.util.List;

public class QueryUtils {
	
	/*
	/****************************************************************************************************
	 *.-~:~-..-~:~-..-~:~-..-~:~-...-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-.
	 *									  -= Global Constants =-
	 *.-~:~-..-~:~-..-~:~-..-~:~-...-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-.
	 ****************************************************************************************************/	
	
	public final static String USER_MODULE = "user";
	
	/*
	/****************************************************************************************************
	 *.-~:~-..-~:~-..-~:~-..-~:~-...-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-.
	 *									 	 -= SWI-Prolog =-
	 *.-~:~-..-~:~-..-~:~-..-~:~-...-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-.
	 ****************************************************************************************************/		
	
	/**
	 * Returns the version of SWI-Prolog which is currently used by a given Factbase 
	 * 
	 * @param PrologInterface 
	 * @return String - Version of SWI-Prolog
	 * @throws PrologInterfaceException 
	 */
	public static String getSWIVersion(PrologInterface pif) throws PrologInterfaceException {
		
		return (String) pif.queryOnce("current_prolog_flag(version,B)").get("B");
	}
	
	
	/*
	/****************************************************************************************************
	 *.-~:~-..-~:~-..-~:~-..-~:~-...-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-.
	 *									  	-= Modules =-
	 *.-~:~-..-~:~-..-~:~-..-~:~-...-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-.
	 ****************************************************************************************************/	
	
	/**
	 * Tests if the module name equals the default user name
	 * 
	 * @param moduleName - variable to be tested
	 * @return true - yes USER_MODULE - else false
	 */
	public static boolean isDefaultModule(String moduleName) {
		return (moduleName == null) || ("".equals(moduleName)) || (USER_MODULE.equals(moduleName));
	}
	
	/*
	/****************************************************************************************************
	 *.-~:~-..-~:~-..-~:~-..-~:~-...-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-.
	 *								 	-= Predicate Builder =-
	 *.-~:~-..-~:~-..-~:~-..-~:~-...-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-..-~:~-.
	 ****************************************************************************************************/	
	
	/**
	 * Shortcut for:
	 * public static String buildTerm(String functor, Object...args)
	 */
	public static String bT(String functor, Object...args) {
		return buildTerm(functor, args);
	}
	
	/**
	 * 
	 * @param functor - String
	 * @param args - Object[]
	 * @return String - query String ready to handover to the queryOnce or queryAll
	 */
	public static String buildTerm(String functor, Object...args) {
		
		if(args == null || (args.length == 1 && args.toString().equals(""))) {
			return functor;
		}
		
		StringBuffer puffer = new StringBuffer();
		puffer.append(functor);
		puffer.append("(");
		
		if(args.length == 1) {
			puffer.append(args[0].toString());
		} else {
			for(Object arg:args) {
				if(arg instanceof Iterable<?>) {
					puffer.append(listToArgList((Iterable<?>)arg));
				} else {
					puffer.append(arg.toString());
				}
				puffer.append(",");
			}
			puffer.delete(puffer.length()-1, puffer.length());
		}
		puffer.append(")");
		return puffer.toString();
	}
	
	/**
	 * Converts an Iterable to a String in prologstyle like "[ , , ]" 
	 * 
	 * @param listToString - Iterable or subclass of Iterable
	 * @return String - Iterable wrapped in "[]" and separated by ","
	 */
	public static String listToArgList(Iterable<?> listToString) {
		
		String listString = null;
		StringBuffer puffer = new StringBuffer();
		puffer.append("[");
		
		if(listToString == null) {
			return puffer.append("]").toString();
		} 
		
		for (Object unknown:listToString) {
			puffer.append(unknown);
			puffer.append(",");
		}
		puffer.replace(puffer.length()-1, puffer.length(), "]");
		listString = puffer.toString();
		
		if(listString == null) {
			//TODO: error correction
		}
		
		return listString;
	}
	
	/**
	 * Converts a given amount of Objects to a single String
	 * representing a list in prolog notation
	 * 
	 * @param objects
	 * @return - String - wrapped in "[]" and separated by ","
	 */
	public static String objectsToArgList(Object...objects) {
		
		List<Object> temp = new ArrayList<Object>();
		
		for(Object obj:objects) {
			temp.add(obj);
		}
		
		return listToArgList(temp);
	}


}
