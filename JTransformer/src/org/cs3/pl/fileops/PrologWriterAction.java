/*
 * Created on 27.04.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.fileops;

/**
 * @author schulzs1
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PrologWriterAction {
	static public final int ADD_INDENT = 0;
	static public final int REDUCE_INDENT = 1;
	static public final int SET_INDENT = 2;
	static public final int WRITE_RULE = 3;
	static public final int WRITE_FACT = 6;
	static public final int WRITE_QUERY = 4;
	static public final int SET_INTERPRET = 5;
	
	
	private int type;
	
	private int newIndent;
	private String name;
	private String head [];
	private String body [];
	private String query; 	
	private boolean interpret;
		
	private PrologWriterAction(){
		
	}
	
	public static PrologWriterAction newAddIndent(){
		PrologWriterAction next = new PrologWriterAction();
		next.type = ADD_INDENT;
		
		return next;
	}
	
	public static PrologWriterAction newReduceIndent(){
		PrologWriterAction next = new PrologWriterAction();
		next.type = REDUCE_INDENT;
		
		return next;
	}
	
	public static PrologWriterAction newSetIndent(int indent){
		PrologWriterAction next = new PrologWriterAction();
		next.type = SET_INDENT;
		next.newIndent = indent;
		
		return next;
	}
	
	public static PrologWriterAction newWriteFact(String name,	String [] args){
		PrologWriterAction next = new PrologWriterAction();
		next.type = WRITE_FACT;
		next.name = name;
		next.head = new String[args.length];
		
		System.arraycopy(args, 0, next.head, 0, args.length);
		
		return next;
	}
	
	public static PrologWriterAction newWriteRule(String name, 
												  String [] args, 
												  String [] cond){
		PrologWriterAction next = new PrologWriterAction();
		next.type = WRITE_RULE;
		next.name = name;
		next.head = new String[args.length];
		
		System.arraycopy(args, 0, next.head, 0, args.length);
		
		next.body = new String[cond.length];
		System.arraycopy(cond, 0, next.body, 0, cond.length);
		
		return next;
	}
	
	public static PrologWriterAction newWriteQuery(String str){
		PrologWriterAction next = new PrologWriterAction();
		next.type = WRITE_QUERY;
		next.query = str;
		
		return next;
	}
	
	public static PrologWriterAction newSetInterpret(boolean inTe){
		PrologWriterAction next = new PrologWriterAction();
		next.type = SET_INTERPRET;
		next.interpret = inTe;
		
		return next;
	}
	
	public void doAction(IPrologWriter plw){
		switch (type){
			case ADD_INDENT:
				plw.addIndention();
				break;
			case REDUCE_INDENT:
				plw.reduceIndention();
				break;
			case SET_INDENT:
				plw.setIndentionLevel(newIndent);
				break;
			case WRITE_RULE:
				plw.writeRule(name, head, body);
				break;
			case WRITE_FACT:
				plw.writeFact(name, head);
				break;
			case WRITE_QUERY:
				plw.writeQuery(query);
				break;
			case SET_INTERPRET:
				plw.setInterpretMode(interpret);
				break;
			default:
				throw new IllegalStateException("Unknown type of Action");
		}
	}
}
