package org.cs3.pl.fileops;


import java.io.*;

import org.cs3.pl.exceptions.ExceptionHandler;

/**
 * Implementation of the IPrologWriter interface, harmonizes with FactFile
 * abstraction
 * 
 * @see IPrologWriter
 * @author Stefan Schulz
 */

public class FactFilePrologWriter implements IPrologWriter {
	
	private Writer writer;
	
	private int indent = 0;
	
	private boolean interpret = true;
	private final String inTe = ":- inTe(";
	
	
	public FactFilePrologWriter(FactFile f){
		writer = f.getWriter();
	}

	public void setIndentionLevel(int i) {
		if (i < 0)
			throw new IllegalArgumentException("negative indention");
		indent = i;
	}

	public void addIndention() {
		indent++;
		
	}

	public void reduceIndention() {
		if (indent == 0)
			throw new IllegalStateException("indent already 0");
		indent--;
		
	}

	public void writeFact(String string, String[] param) {
		writeRule(string, param, null);
	}

	public void writeRule(String string, String[] param, String[] condi) {
		StringBuffer buf = new StringBuffer();
		
		for (int i = 0; i < indent; i++){
			buf.append('\t');
		}
		
		if (interpret)
			buf.append(inTe);
		buf.append(string);
		buf.append("(");
		
		if (param != null && param.length > 0){
			for (int i = 0; i < param.length - 1; i++){
				buf.append(param[i]);
				buf.append(", ");
			}
		
			buf.append(param[param.length - 1]);
		}
		
		buf.append(")");
		
		if (condi != null && condi.length > 0){
			buf.append(" :- ");
			
			for (int i = 0; i < condi.length - 1; i++){
				buf.append(condi[i]);
				buf.append(", ");
			}
			
			buf.append(condi[condi.length - 1]);
		}
		
		if (interpret)
			buf.append(")");
		buf.append(".\n");
	
		try {
			synchronized(writer){
				writer.write(buf.toString());
			}
		} catch (IOException e) {
			ExceptionHandler.handle(e);
		}
		
	}

	public void close() {	
		try {
			writer.close();
		} catch (IOException e) {
			ExceptionHandler.handle(e);
		}		
	}
	

	public void flush() {
		try {
			writer.flush();
		} catch (IOException e) {
			ExceptionHandler.handle(e);
		}		
		
	}

	public void setInterpretMode(boolean interpret) {
		this.interpret = interpret;
		
	}

	public boolean getInterpretMode() {
		return interpret;
	}

	public void writeQuery(String query) {
		try {
			synchronized (writer){
				writer.write(":- " + query +".\n");
			}
		} catch (IOException e) {
			ExceptionHandler.handle(e);
		}	
	}
}
