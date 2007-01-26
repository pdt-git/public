package org.cs3.pl.tuprolog.internal;

import java.util.HashMap;
import java.util.Vector;

import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;

import alice.tuprolog.InvalidTermException;
import alice.tuprolog.Term;

public class TuPrologEventDispatcher {
	private ObserverLibrary lib = null;
	private TuProlog engine = null;
	
	private HashMap listenersList = new HashMap();
	
	public TuPrologEventDispatcher(TuPrologPrologInterface pif) {
		
		engine = pif.getEngine();
		lib = (ObserverLibrary) engine.getLibrary("ObserverLibrary");
	};
	
	
	public void addPrologInterfaceListener(String subject, PrologInterfaceListener listener) throws InvalidTermException{

/*		synchronized (listenersList) {
			Vector list = (Vector) listenersList.get(subject);
			if (list == null) {
				list = new Vector();
				listenersList.put(subject, list);
				enableSubject(subject);
			}
			
			if (!list.contains(listener)) {
				list.add(listener);
			}
		}
		*/
		
		if ( lib != null)
			lib.addListener(subject, listener);
	}
	
	public void removePrologInterfaceListener(String subject, PrologInterfaceListener listener){
		if ( lib != null)
			lib.removeListener(subject, listener);
	}
	
	
	private void enableSubject(String subject) throws InvalidTermException  {
		lib.observe_1(Term.parse(subject));
	}
}
