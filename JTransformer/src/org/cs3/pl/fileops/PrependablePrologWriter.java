/*
 * Created on 27.04.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.fileops;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;


/**
 *
 * This class is yet another implementation of the IPrologWriter interface, that
 * enables the prepending of action to the File (unlike previous implementations
 * that actually wrote to a file or something similar). The Class needs another
 * IPrologWriter implementation, and works by caching the PrologWriterActions 
 * used, only actually doing them once flush() is called. Flush needs to be
 * <u>explicitly</u> called in this implementation, it is not implicitly called
 * when close is used.
 * 
 * @author schulzs1 
 */
public class PrependablePrologWriter implements IPrologWriter {
	
	private Vector actions = new Vector();

	private boolean interpret = true;
	
	private IPrologWriter writer;
	
	private String resource = null;
	private PrologMetaDataManager manager = null;
	
	/**
	 * constructs a new PrependablePrologWriter. This class always assumes that
	 * Interpret mode is enabled, unless you deliberatly disable it by useing
	 * setInterpretMode(false). The class defers all actual writes using tokens
	 * to represent the actions. This makes it cleanly possible to inject rules,
	 * actions and facts after others have been added.
	 * 
	 * @param writer The writer backing this class.
	 */
	
	public PrependablePrologWriter(IPrologWriter writer){
		this.writer = writer;
		writer.setInterpretMode(true);
	}
	
	public PrependablePrologWriter(IPrologWriter writer, String handle, PrologMetaDataManager manager){
		this.writer = writer;
		this.resource = handle;
		this.manager = manager;
		
		writer.setInterpretMode(true);
	}
	
	/**
	 * calls flush(), close() and causes the MetaDataManager to consult the current file. If this Object has
	 * been initialized without a resource handle, it only is a shorthand for flush(); close();
	 * 
	 * @deprecated not a good idea, too much "magic" in this method. Use flush(), close(), consult() instead. 
	 */
	
	public void finish() throws IOException {
		flush();
		close();
		
		if (manager == null || resource == null)
			return;
		
		manager.consult(resource);
	}

	public void addIndention() {
		actions.add(PrologWriterAction.newAddIndent());		
	}

	public void close() {
		writer.close();		
	}

	public void flush() {
		for (Iterator i = actions.iterator(); i.hasNext();) {
			PrologWriterAction act = (PrologWriterAction) i.next();
			act.doAction(writer);
		}
		writer.flush();
	}

	public boolean getInterpretMode() {
		return interpret;
	}
	
	public void insertAction(int nr, PrologWriterAction act){
		actions.add(nr, act);
	}
	
	public void insertActions(int nr, List l){
		for (Iterator iter = l.iterator(); iter.hasNext();) {
			if (!(iter.next() instanceof PrologWriterAction))
				throw new IllegalArgumentException("Non Action passed");
		}
		
		actions.addAll(nr, l);
	}
	
	public void prependAction(PrologWriterAction act){
		insertAction(0, act);
	}
	
	public void prependActions(List l){
		insertActions(0, l);
	}

	/**
	 * @see org.cs3.pl.fileops.IPrologWriter#reduceIndention()
	 */
	public void reduceIndention() {
		actions.add(PrologWriterAction.newReduceIndent());		
	}

	/**
	 * @see org.cs3.pl.fileops.IPrologWriter#setIndentionLevel(int)
	 */
	public void setIndentionLevel(int i) {
		actions.add(PrologWriterAction.newSetIndent(i));		
	}

	/**
	 * @see org.cs3.pl.fileops.IPrologWriter#setInterpretMode(boolean)
	 */
	public void setInterpretMode(boolean interpret) {
		actions.add(PrologWriterAction.newSetInterpret(interpret));
		this.interpret = interpret;
	}

	/**
	 * @see org.cs3.pl.fileops.IPrologWriter#writeFact(java.lang.String, java.lang.String[])
	 */
	public void writeFact(String string, String[] param) {
		actions.add(PrologWriterAction.newWriteFact(string, param));
	}

	/** 
	 * @see org.cs3.pl.fileops.IPrologWriter#writeQuery(java.lang.String)
	 */
	public void writeQuery(String query) {
		actions.add(PrologWriterAction.newWriteQuery(query));
	}

	/**
	 * @see org.cs3.pl.fileops.IPrologWriter#writeRule(java.lang.String, java.lang.String[], java.lang.String[])
	 */
	public void writeRule(String string, String[] param, String[] condi) {
		actions.add(PrologWriterAction.newWriteRule(string, param, condi));		
	}
}
