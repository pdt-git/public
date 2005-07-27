package org.cs3.pdt.console.internal;

import java.util.HashSet;
import java.util.Vector;

import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.console.prolog.PrologConsoleEvent;
import org.cs3.pl.console.prolog.PrologConsoleListener;
import org.cs3.pl.console.prolog.PrologConsoleService;

public class DefaultPrologConsoleService implements PrologConsoleService, PrologConsoleListener {

	private HashSet visibleConsoles=new HashSet();
	private Vector consoles=new Vector();
	private PrologConsole activeConsole;
	
	public void registerPrologConsole(PrologConsole console) {
		
		synchronized (consoles) {
			if(!consoles.contains(console)){
				consoles.add(console);
				console.addPrologConsoleListener(this);
			}			
		}
		
	}

	public void unregisterPrologConsole(PrologConsole console) {
		synchronized (consoles) {
			if(consoles.contains(console)){
				consoles.remove(console);
				visibleConsoles.remove(console);
				console.removePrologConsoleListener(this);
				if(console==activeConsole){
					activeConsole=null;
				}
			}			
		}		
	}

	public PrologConsole[] getRegisteredPrologConsoles() {		
		return (PrologConsole[]) consoles.toArray(new PrologConsole[consoles.size()]);
	}

	public PrologConsole getActivePrologConsole() {
		if(activeConsole!=null){
			return activeConsole;
		}
		if(visibleConsoles.size()==1){
			return (PrologConsole) visibleConsoles.iterator().next();
		}
		return null;
	}

	
	public void consoleRecievedFocus(PrologConsoleEvent e) {
		activeConsole=(PrologConsole) e.getSource();
		
	}

	public void consoleLostFocus(PrologConsoleEvent e) {
		activeConsole=null;		
	}


	public void consoleVisibilityChanged(PrologConsoleEvent e) {
		PrologConsole c = (PrologConsole) e.getSource();
		if(c.isVisible()){
			visibleConsoles.add(c);
		}
		else{
			visibleConsoles.remove(c);
		}
		
	}

}
