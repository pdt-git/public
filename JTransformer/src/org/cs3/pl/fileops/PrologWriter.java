
package org.cs3.pl.fileops;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.cs3.pl.exceptions.ExceptionHandler;



public class PrologWriter implements IPrologWriter {
	
	private int ilevel = 0;
	private boolean interpret;
	private static final String INTERPRET_PREDICATE = ":- inTe";
	private BufferedWriter bwriter;
	
	public static PrologWriter getWriter(String path){
		try {
			PrologWriter newWriter = new PrologWriter(path);
			return newWriter;
		} catch (IOException e) {
			ExceptionHandler.handle(e);
			return null;
		}
	}
		
	/**
	 * 
	 * @param path The path of the target prolog file
	 * @param interpret activate/deactivate the interpret mode
	 * @return
	 */
	public static IPrologWriter getWriter(String path, boolean interpret) {
		PrologWriter writer = getWriter(path);
		writer.setInterpretMode(interpret);
		return writer;
	}
	
	
	private PrologWriter(String path)throws IOException {
		File file = new File(path);
		
		if (file.exists())
			file.delete();
		
		file.getParentFile().mkdirs();
		
		bwriter = new BufferedWriter(new FileWriter(file));
	}
	
	public void setIndentionLevel(int i){
		ilevel = i;
	}
	
	public void addIndention(){
		ilevel++;
	}
	
	public void reduceIndention(){
		ilevel--;
	}

	public void writeFact(String string, String[] param) {
		
		writeRule(string, param, null);
	}
	
	synchronized public void writeRule(String string, String[] param, String[] condi){
		StringBuffer buf = new StringBuffer();
		
		for (int i = 0; i < ilevel; i++){
			buf.append('\t');
		}
		
		if (interpret)
			buf.append(INTERPRET_PREDICATE + "(");
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
	
		write(buf.toString());
	}

	private void write(String s){
		//System.out.println("writing: "+s);
		try {
			bwriter.write(s);
		} catch (IOException e) {
			ExceptionHandler.handle(e);
		}
	}
	
	public void flush(){
		try {
			bwriter.flush();
		} catch (IOException e){
			ExceptionHandler.handle(e);
		}
	}
	
	public void close(){
		try {
			flush();
			bwriter.close();
		} catch (IOException e){
			ExceptionHandler.handle(e);
		}
	}
	
	public void setInterpretMode(boolean interpret) {
		this.interpret = interpret;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.fileops.IPrologWriter#getInterpretMode()
	 */
	public boolean getInterpretMode() {
		// TODO Auto-generated method stub
		return interpret;
	}

	synchronized public void writeQuery(String query) {
		write(":- " + query +".\n");
	}
	
}
