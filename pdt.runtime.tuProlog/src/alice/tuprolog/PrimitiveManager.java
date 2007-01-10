/*
 * tuProlog - Copyright (C) 2001-2002  aliCE team at deis.unibo.it
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package alice.tuprolog;

import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;


/**
 * Administration of primitive predicates
 * @author Alex Benini
 */
public class PrimitiveManager {
	
	private IdentityHashMap libHashMap;
	private HashMap directiveHashMap;
	private HashMap predicateHashMap;
	private HashMap functorHashMap;
	
	public PrimitiveManager() {
		libHashMap        = new IdentityHashMap();
		directiveHashMap  = new HashMap();
		predicateHashMap  = new HashMap();
		functorHashMap    = new HashMap();
	}
	
	
	/**
	 * Config this Manager
	 */
	void initialize(Prolog vm) {
		createPrimitiveInfo(new BuiltIn(vm));
	}
	
	
	
	void createPrimitiveInfo(IPrimitives src) {
		List[] prims = src.getPrimitives();
		Iterator it = prims[PrimitiveInfo.DIRECTIVE].iterator();
		while(it.hasNext()) {
			PrimitiveInfo p = (PrimitiveInfo)it.next();
			directiveHashMap.put(p.getKey(),p);
		}
		it = prims[PrimitiveInfo.PREDICATE].iterator();
		while(it.hasNext()) {
			PrimitiveInfo p = (PrimitiveInfo)it.next();
			predicateHashMap.put(p.getKey(),p);
		}
		it = prims[PrimitiveInfo.FUNCTOR].iterator();
		while(it.hasNext()) {
			PrimitiveInfo p = (PrimitiveInfo)it.next();
			functorHashMap.put(p.getKey(),p);
		}
		List primOfLib = new LinkedList(prims[PrimitiveInfo.DIRECTIVE]);
		primOfLib.addAll(prims[PrimitiveInfo.PREDICATE]);
		primOfLib.addAll(prims[PrimitiveInfo.FUNCTOR]);
		libHashMap.put(src,primOfLib);
	}
	
	
	void deletePrimitiveInfo(IPrimitives src) {
		Iterator it = ((List)libHashMap.remove(src)).iterator();
		while(it.hasNext()) {
			String k = ((PrimitiveInfo)it.next()).invalidate();
			directiveHashMap.remove(k);
			predicateHashMap.remove(k);
			functorHashMap.remove(k);
		}
	}
	
	
	/**
	 * Identifies the term passed as argument.
	 *
	 * This involves identifying structs representing builtin
	 * predicates and functors, and setting up related structures and links
	 *
	 * @parm term the term to be identified
	 */
	public void identifyDirective(Term term) {
		identify(term,PrimitiveInfo.DIRECTIVE);
	}
	
	public void identifyPredicate(Term term) {
		identify(term,PrimitiveInfo.PREDICATE);
	}
	
	public void identifyFunctor(Term term) {
		identify(term,PrimitiveInfo.FUNCTOR);
	}
	
	private void identify(Term term, int typeOfPrimitive) {
		if (term == null) {
			return;
		}
		term = term.getTerm();
		if (!term.isStruct()) {
			return;
		}
		Struct t = (Struct) term;
		
		int arity = t.getArity();
		String name = t.getName();
		//------------------------------------------
		if (name.equals(",") || name.equals("':-'") || name.equals(":-")) {
			for (int c = 0; c < arity; c++) {
				identify( t.getArg(c), PrimitiveInfo.PREDICATE);
			}
		} else {
			for (int c = 0; c < arity; c++) {
				identify( t.getArg(c), PrimitiveInfo.FUNCTOR);
			}	    		    	
		}
		//------------------------------------------
		//log.debug("Identification "+t);	
		PrimitiveInfo prim = null;
		String key = name + "/" + arity;
		
		switch (typeOfPrimitive) {
		case PrimitiveInfo.DIRECTIVE :
			prim = (PrimitiveInfo)directiveHashMap.get(key);	    		
			//log.debug("Assign predicate "+prim+" to "+t);
			break;
		case PrimitiveInfo.PREDICATE :
			prim = (PrimitiveInfo)predicateHashMap.get(key);	    		
			//log.debug("Assign predicate "+prim+" to "+t);
			break;
		case PrimitiveInfo.FUNCTOR :
			prim = (PrimitiveInfo)functorHashMap.get(key);
			//log.debug("Assign functor "+prim+" to "+t);
			break;
		}
		t.setPrimitive(prim);
	}
	
	
	Library getLibraryDirective(String name, int nArgs) {
		try {
			return (Library)( (PrimitiveInfo)directiveHashMap.get(name + "/" + nArgs)).getSource();			
		} catch(NullPointerException e) {
			return null;
		}
	}
	
	Library getLibraryPredicate(String name, int nArgs) {
		try {
			return (Library)( (PrimitiveInfo)predicateHashMap.get(name + "/" + nArgs)).getSource();			
		} catch(NullPointerException e) {
			return null;
		}
	}
	
	Library getLibraryFunctor(String name, int nArgs) {
		try {
			return (Library)( (PrimitiveInfo)functorHashMap.get(name + "/" + nArgs)).getSource();
		} catch(NullPointerException e) {
			return null;
		}
	}
	
	
}