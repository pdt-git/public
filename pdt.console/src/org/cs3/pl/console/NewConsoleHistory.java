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
	Vector history = new Vector();
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
		model.setLineBuffer((String) history.get(pointer));
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
		}
		else {			
			  model.setLineBuffer((String) history.get(pointer));
		}
		
	}

	public void clearHistory() {
		if(lastLine!=null){
			model.setLineBuffer(lastLine);
			lastLine=null;
			pointer=0;
		}
		history.clear();
		
	}

	
	public void onCommit(ConsoleModelEvent e) {		
		lastLine=null;
		history.add(e.getCommitText());
		pointer=history.size();
	
	}

	public void onModeChange(ConsoleModelEvent e) {	}
	
	public void onEditBufferChanged(ConsoleModelEvent e) {	}
	
	public void onOutput(ConsoleModelEvent e) {	}

	public void afterConnect(ConsoleModelEvent e) {}

	public void beforeDisconnect(ConsoleModelEvent e) {}
	
	public void saveHistory(OutputStream os) throws IOException{
		PrintStream ps = new PrintStream(os);
		for (Iterator iter = history.iterator(); iter.hasNext();) {
			String line = (String) iter.next();
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
