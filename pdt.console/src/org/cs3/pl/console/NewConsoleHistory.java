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

package org.cs3.pl.console;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Iterator;
import java.util.Vector;


/*
 * 
 * the only allowed modifications of the history are 
 * 	history.add(Obj),
 *  history.clear()
 * 
 * every operation that modifies the history MUST have the following
 * post conditions: 
 *   pointer=history.size(); //after mod.
 *   lastline == null;
 *   
 * 
 * invarianz: 
 * lastline==null 
 *  <-> currently visible linebuffer is not in history
 *  <-> pointer==history.size()
 *
 *  lastline==null or 0<=pointer<history.size()
 *  
 *  0<=pointer<=history.size()
 */
public class NewConsoleHistory implements ConsoleHistory, ConsoleModelListener {
	Vector<String> history = new Vector<String>();
	int pointer=0;
	String lastLine;
	
	private ConsoleModel model;
	@Override
	public void setConsoleModel(ConsoleModel consoleModel) {
		if (model == consoleModel) {
			return;
		}
		if (model != null) {
			model.removeConsoleListener(this);
			
		}
		this.model = consoleModel;
		if (model != null) {
			model.addConsoleListener(this);
			
		}

	}

	@Override
	public ConsoleModel getConsoleModel() {		
		return model;
	}

	
	
	@Override
	public void previous() {		
		if(model==null||history.isEmpty()||pointer<=0){
			return;
		}
		String current = model.getLineBuffer();
		if(lastLine==null){	//eq to pointer==history.size()		
			lastLine=current;			
		}	
		pointer--;
		model.setLineBuffer(history.get(pointer));
	}

	@Override
	public void next() {
		
		if(model==null){
			return;
		}
		if(lastLine==null){//eq to pointer==history.size()
			return;
		}
		pointer++;
		if(pointer==history.size()){
			model.setLineBuffer(lastLine);
			lastLine=null;			
		}
		else {			
			  model.setLineBuffer(history.get(pointer));
		}
		
	}

	@Override
	public void clearHistory() {
		if(lastLine!=null){
			model.setLineBuffer(lastLine);
			lastLine=null;
			pointer=0;
		}
		history.clear();
		
	}

	
	@Override
	public void onCommit(ConsoleModelEvent e) {		
		lastLine=null;
		history.add(e.getCommitText());
		pointer=history.size();
	
	}

	@Override
	public void onModeChange(ConsoleModelEvent e) {	}
	
	@Override
	public void onEditBufferChanged(ConsoleModelEvent e) {	}
	
	@Override
	public void onOutput(ConsoleModelEvent e) {	}

	@Override
	public void afterConnect(ConsoleModelEvent e) {}

	@Override
	public void beforeDisconnect(ConsoleModelEvent e) {}
	
	public void saveHistory(OutputStream os) throws IOException{
		PrintStream ps = new PrintStream(os);
		for (Iterator<String> iter = history.iterator(); iter.hasNext();) {
			String line = iter.next();
			ps.println(line);
		}
	}

	public void loadHistory(InputStream is) throws IOException{
		clearHistory();
		BufferedReader reader = new BufferedReader(new InputStreamReader(is));
		String line = reader.readLine();
		while(line!=null){
			history.add(line);
			line = reader.readLine();
		}
		pointer=history.size();
	}
	
}
