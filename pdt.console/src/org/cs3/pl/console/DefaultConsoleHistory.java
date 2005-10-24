package org.cs3.pl.console;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Iterator;
import java.util.LinkedList;

import org.cs3.pl.common.Debug;


/**
 * uses two stacks to model the list.
 * head is the past. 
 * tail is the future.
 * now is the line buffer of the model.
 */
public class DefaultConsoleHistory implements ConsoleHistory,
		ConsoleModelListener {

	private ConsoleModel model = null;
	
	private LinkedList head = new LinkedList();
	private LinkedList tail = new LinkedList();

	//should history be modifieable?
	private boolean changeHistory=false;
	/*
	 * when cycling through history, this will
	 * contain the original state of the current line,
	 * so that the past can be made imutable (the default)
	 */
	private String original=null;
	

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleHistory#setConsoleModel(org.cs3.pl.views.ConsoleModel)
	 */
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
		initStacks();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleHistory#getConsoleModel()
	 */
	public ConsoleModel getConsoleModel() {
		return model;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleHistory#previous()
	 */
	public void previous() {
		Debug.debug("previous");
		if(model==null) {
			return;
		}
		if(!head.isEmpty()){
			tail.addFirst(changeHistory||original==null? model.getLineBuffer() : original);
			original=(String) head.getLast();
			model.setLineBuffer((String) original);
			head.removeLast();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleHistory#next()
	 */
	public void next() {
		if(model==null){
			return;
		}
		if(!tail.isEmpty()){
			head.addLast(changeHistory||original==null? model.getLineBuffer() : original);
			original=(String) tail.getFirst();
			model.setLineBuffer((String) original);
			tail.removeFirst();
		}
	}

	private void initStacks(){
		head.clear();
		tail.clear();	
		original=null;
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleHistory#clearHistory()
	 */
	public void clearHistory() {
		initStacks();
	}

	
		
	
	

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleModelListener#onCommit(org.cs3.pl.views.ConsoleModelEvent)
	 */
	public void onCommit(ConsoleModelEvent e) {
	    //ignore commits like ";" or ""
	    if(e.getCommitText().trim().equals(";")
	            ||e.getCommitText().trim().length()==0){
	        return;
	    }
		if(!tail.isEmpty()){
			if(original==null){
				Debug.error("Lukas says: I have made a mistake. Sorry.");
			}
			head.addLast(original);
			head.addAll(tail);
			head.removeLast();	
			tail.clear();
		}
		head.addLast(e.getCommitText());
		
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleModelListener#onModeChange(org.cs3.pl.views.ConsoleModelEvent)
	 */
	public void onModeChange(ConsoleModelEvent e) {	}
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleModelListener#onEditBufferChanged(org.cs3.pl.views.ConsoleModelEvent)
	 */
	public void onEditBufferChanged(ConsoleModelEvent e) {	}
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleModelListener#onOutput(org.cs3.pl.views.ConsoleModelEvent)
	 */
	public void onOutput(ConsoleModelEvent e) {	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.console.ConsoleModelListener#afterConnect(org.cs3.pl.console.ConsoleModelEvent)
	 */
	public void afterConnect(ConsoleModelEvent e) {
		// TODO should we handle this?		
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.console.ConsoleModelListener#beforeDisconnect(org.cs3.pl.console.ConsoleModelEvent)
	 */
	public void beforeDisconnect(ConsoleModelEvent e) {
		// TODO should we handle this?		
	}
	
	public void saveHistory(OutputStream os) throws IOException{
		PrintStream ps = new PrintStream(os);
		for (Iterator iter = head.iterator(); iter.hasNext();) {
			String line = (String) iter.next();
			ps.println(line);
		}
		for (Iterator iter = tail.iterator(); iter.hasNext();) {
			String line = (String) iter.next();
			ps.println(line);
		}
	}

	public void loadHistory(InputStream is) throws IOException{
		head.clear();
		tail.clear();
		BufferedReader reader = new BufferedReader(new InputStreamReader(is));
		String line = reader.readLine();
		while(line!=null){
			head.addLast(line);
			line = reader.readLine();
		}
		
	}
}