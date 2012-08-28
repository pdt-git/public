/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.console.internal.views;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Iterator;
import java.util.Vector;

import org.cs3.pdt.console.ConsoleModel;
import org.cs3.pdt.console.ConsoleModelEvent;
import org.cs3.pdt.console.ConsoleModelListener;


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
public class ConsoleHistory implements ConsoleModelListener {
	Vector<String> history = new Vector<String>();
	int pointer=0;
	String lastLine;
	
	private ConsoleModel model;
	
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

	public ConsoleModel getConsoleModel() {		
		return model;
	}

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
		} else {			
			model.setLineBuffer(history.get(pointer));
		}
	}

	public void clearHistory() {
		if (lastLine!=null){
			model.setLineBuffer(lastLine);
			lastLine=null;
			pointer=0;
		}
		history.clear();
	}

	@Override
	public void onCommit(ConsoleModelEvent e) {		
		lastLine=null;
		String value = e.getCommitText();
	    //ignore commits like ";", " ", "" + pdt_reload(

		if(! ( value.equals(" ")
				|| value.equals("")
				|| value.equals(";")
				|| value.startsWith("pdt_reload:pdt_reload("))
				) {
			history.add(e.getCommitText());
		}
		pointer = history.size();
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


