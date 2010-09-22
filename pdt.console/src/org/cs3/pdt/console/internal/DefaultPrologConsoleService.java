/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pdt.console.internal;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.console.prolog.PrologConsoleEvent;
import org.cs3.pl.console.prolog.PrologConsoleListener;
import org.cs3.pl.console.prolog.PrologConsoleService;

public class DefaultPrologConsoleService implements PrologConsoleService, PrologConsoleListener {

	private Vector<PrologConsoleListener> listeners = new Vector<PrologConsoleListener>();

	private HashSet<PrologConsole> visibleConsoles=new HashSet<PrologConsole>();
	private Vector<PrologConsole> consoles=new Vector<PrologConsole>();
	private PrologConsole activeConsole;
	
	public DefaultPrologConsoleService() {
		addPrologConsoleListener(this);
	}
	
	@Override
	public void registerPrologConsole(PrologConsole console) {
		
		synchronized (consoles) {
			if(!consoles.contains(console)){
				consoles.add(console);
			}			
		}
		
	}

	@Override
	public void unregisterPrologConsole(PrologConsole console) {
		synchronized (consoles) {
			if(consoles.contains(console)){
				consoles.remove(console);
				visibleConsoles.remove(console);
//				removePrologConsoleListener(this);
				if(console==activeConsole){
					activeConsole=null;
				}
			}			
		}		
	}

	@Override
	public PrologConsole[] getRegisteredPrologConsoles() {		
		return consoles.toArray(new PrologConsole[consoles.size()]);
	}

	@Override
	public PrologConsole getActivePrologConsole() {
		if(activeConsole!=null){
			return activeConsole;
		}
		if(visibleConsoles.size()==1){
			return visibleConsoles.iterator().next();
		}
		return null;
	}

	
	@Override
	public void consoleRecievedFocus(PrologConsoleEvent e) {
		activeConsole=(PrologConsole) e.getSource();
		
	}

	@Override
	public void consoleLostFocus(PrologConsoleEvent e) {
		activeConsole=null;		
	}


	@Override
	public void consoleVisibilityChanged(PrologConsoleEvent e) {
		PrologConsole c = (PrologConsole) e.getSource();
		if(c.isVisible()){
			visibleConsoles.add(c);
		}
		else{
			visibleConsoles.remove(c);
		}
		
	}

	
	@Override
	public void activePrologInterfaceChanged(PrologConsoleEvent e) {
		
	}

	@Override
	public void addPrologConsoleListener(PrologConsoleListener l) {
		synchronized (listeners) {
			if (!listeners.contains(l)) {
				listeners.add(l);
			}
		}

	}

	@Override
	public void removePrologConsoleListener(PrologConsoleListener l) {
		synchronized (listeners) {
			if (listeners.contains(l)) {
				listeners.remove(l);
			}
		}
	}

	public void fireConsoleRecievedFocus(PrologConsole console) {
		Vector<PrologConsoleListener> clone = null;
		synchronized (listeners) {
			clone = (Vector<PrologConsoleListener>) listeners.clone();
		}
		PrologConsoleEvent e = new PrologConsoleEvent(console);
		for (Iterator<PrologConsoleListener> iter = clone.iterator(); iter.hasNext();) {
			PrologConsoleListener l = iter.next();
			l.consoleRecievedFocus(e);
		}
	}
	

	public void fireActivePrologInterfaceChanged(PrologConsole console) {
		Vector<PrologConsoleListener> clone = null;
		synchronized (listeners) {
			clone = (Vector<PrologConsoleListener>) listeners.clone();
		}
		PrologConsoleEvent e = new PrologConsoleEvent(console);
		for (Iterator<PrologConsoleListener> iter = clone.iterator(); iter.hasNext();) {
			PrologConsoleListener l = iter.next();
			l.activePrologInterfaceChanged(e);
		}		
	}

	public void fireConsoleLostFocus(PrologConsole console) {
		Vector<PrologConsoleListener> clone = null;
		synchronized (listeners) {
			clone = (Vector<PrologConsoleListener>) listeners.clone();
		}
		PrologConsoleEvent e = new PrologConsoleEvent(console);
		for (Iterator<PrologConsoleListener> iter = clone.iterator(); iter.hasNext();) {
			PrologConsoleListener l = iter.next();
			l.consoleLostFocus(e);
		}
	}

	public void fireConsoleVisibilityChanged(PrologConsole console) {
		Vector<PrologConsoleListener> clone = null;
		synchronized (listeners) {
			clone = (Vector<PrologConsoleListener>) listeners.clone();
		}
		PrologConsoleEvent e = new PrologConsoleEvent(console);
		for (Iterator<PrologConsoleListener> iter = clone.iterator(); iter.hasNext();) {
			PrologConsoleListener l = iter.next();
			l.consoleVisibilityChanged(e);
		}
	}

}
