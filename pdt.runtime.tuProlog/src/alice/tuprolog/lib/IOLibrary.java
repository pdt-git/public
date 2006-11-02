/*
 * tuProlog - Copyright (C) 2001-2002  aliCE team at deis.unibo.it
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package alice.tuprolog.lib;

import alice.tuprolog.*;

import java.util.*;
import java.io.*;

/**
 * This class provides basic I/O predicates.
 *
 * Library/Theory Dependency: BasicLibrary
 *
 *
 *
 */
public class IOLibrary extends Library {
	
	protected String inputStreamName = "stdin";
	protected InputStream inputStream = System.in;
	protected String outputStreamName = "stdout";
	protected OutputStream outputStream = System.out;
	private Random gen = new Random();
	
	
	public IOLibrary() {
		gen.setSeed(System.currentTimeMillis());
	}
	
	public boolean see_1(Term arg) throws Exception {
		Struct arg0 = (Struct) arg.getTerm();
		if (!arg0.isAtom()) {
			return false;
		}
		if (arg0.getName().equals("stdin")) {
			inputStream = System.in;
		} else {
			inputStream = new FileInputStream(((Struct) arg0).getName());
		}
		inputStreamName = ((Struct) arg0).getName();
		return true;
	}
	
	public boolean seen_0() throws Exception {
		inputStream.close();
		return true;
	}
	
	public boolean seeing_1(Term t) {
		return unify(t, new Struct(inputStreamName));
	}
	
	public boolean tell_1(Term arg) throws Exception {
		Struct arg0 = (Struct) arg.getTerm();
		if (!arg0.isAtom()) {
			return false;
		}
		if (arg0.getName().equals("stdout")) {
			outputStream = System.out;
		} else {
			outputStream = new FileOutputStream(((Struct) arg0).getName());
		}
		outputStreamName = ((Struct) arg0).getName();
		return true;
	}
	
	public boolean told_0() throws Exception {
		outputStream.close();
		return true;
	}
	
	public boolean telling_1(Term arg0) {
		return unify(arg0, new Struct(outputStreamName));
	}
	
	public boolean put_1(Term arg) throws Exception {
		Struct arg0 = (Struct) arg.getTerm();
		if (!arg0.isAtom()) {
			return false;
		} else {
			String ch = arg0.getName();
			if (ch.length() > 1) {
				return false;
			} else {
				if (outputStreamName.equals("stdout")) {
					getEngine().stdOutput(ch);
				} else {
					outputStream.write((byte) ch.charAt(0));
				}
				return true;
			}
		}
	}
	
	public boolean get0_1(Term arg0) throws Exception {
		int ch = inputStream.read();
		if (ch == -1) {
			return unify(arg0, new Int(-1));
		} else {
			return unify(arg0, new Struct(new Character((char) ch).toString()));
		}
	}
	
	public boolean get_1(Term arg0) throws Exception {
		int ch = 0;
		do {
			ch = inputStream.read();
		} while (ch < 0x20 && ch >= 0);
		if (ch == -1) {
			return unify(arg0, new Int(-1));
		} else {
			return unify(arg0, new Struct(new Character(((char) ch)).toString()));
		}
	}
	
	public boolean tab_1(alice.tuprolog.Number arg) throws Exception {
		int n = arg.intValue();
		if (outputStreamName.equals("stdout")) {
			for (int i = 0; i < n; i++) {
				getEngine().stdOutput("\t");
			}
		} else {
			for (int i = 0; i < n; i++) {
				outputStream.write(0x20);
			}
		}
		return true;
	}
	
	public boolean read_1(Term arg0) throws Exception {
		arg0 = arg0.getTerm();
		try {
			int ch = 0;
			
			boolean open_apices = false;
			//boolean just_open_apices = false;
			boolean open_apices2 = false;
			//boolean just_open_apices2 = false;
			
			String st = "";
			do {
				ch = inputStream.read();
				
				if (ch == -1) {
					break;
				}
				boolean can_add = true;
				
				if (ch=='\''){
					if (!open_apices){
						open_apices = true;
					} else {
						open_apices = false;
					}
				} else if (ch=='\"'){
					if (!open_apices2){
						open_apices2 = true;
					} else {
						open_apices2 = false;
					}
				} else {
					if (ch=='.'){
						if (!open_apices && !open_apices2){
							break;
						}
					}
				}
				
				
				if (can_add){
					st += new Character(((char) ch)).toString();
				}
			} while (true);
			
			return unify(arg0, getEngine().toTerm(st));
		} catch (Exception ex){
			return false;
		}
	}
	
	public boolean write_1(Term arg0) throws Exception {
		arg0 = arg0.getTerm();
		try {
			if (outputStreamName.equals("stdout")) {
				getEngine().stdOutput(arg0.toString());
			} else {
				outputStream.write(arg0.toString().getBytes());
			}
			return true;
		} catch (Exception ex){
			return false;
		}
	}
	
	public boolean print_1(Term arg0) throws Exception {
		arg0 = arg0.getTerm();
		try {
			if (outputStreamName.equals("stdout")) {
				getEngine().stdOutput(alice.util.Tools.removeApices(arg0.toString()));
			} else {
				outputStream.write(alice.util.Tools.removeApices(arg0.toString()).getBytes());
			}
			return true;
		} catch (Exception ex){
			return false;
		}
	}
	
	public boolean nl_0() throws Exception {
		if (outputStreamName.equals("stdout")) {
			getEngine().stdOutput("\n");
		} else {
			outputStream.write('\n');
		}
		return true;
	}
	
	/**
	 * reads a source text from a file.
	 * <p>
	 * It's useful used with agent predicate:
	 * text_from_file(File,Source), agent(Source).
	 */
	public boolean text_from_file_2(Term file_name, Term text) {
		Struct fileName = (Struct) file_name.getTerm();
		try {
			Struct goal = new Struct(alice.util.Tools.loadText(
					alice.util.Tools.removeApices(((Struct) fileName).toString())));
			return unify(text, goal);
		} catch (Exception ex) {
			//ex.printStackTrace();
			return false;
		}
	}
	
	// miscellanea
	
	public boolean rand_float_1(Term t) {
		return unify(t, new alice.tuprolog.Double(gen.nextFloat()));
	}
	
	public boolean rand_int_2(Term argNum, Term num) {
		alice.tuprolog.Number arg = (alice.tuprolog.Number) argNum.getTerm();
		return unify(num, new Int(gen.nextInt(arg.intValue())));
	}
	
	public String getTheory() {
		return
		"consult(File) :- text_from_file(File,Text), add_theory(Text).\n" +
		"reconsult(File) :- text_from_file(File,Text), set_theory(Text).\n" +
		"solve_file(File,Goal) :- text_from_file(File,Text),text_term(Text,Goal),call(Goal).\n" +
		"agent_file(X)  :- text_from_file(X,Y),agent(Y).\n";
	}
	
	
	// to allow serialization -> nullify streams before serialization
	
	private void writeObject(java.io.ObjectOutputStream out) throws IOException {
		InputStream inputStreamBak = inputStream;
		OutputStream outputStreamBak = outputStream;
		inputStream = null;
		outputStream = null;
		try {
			out.defaultWriteObject();
		} catch (IOException ex) {
			inputStream = inputStreamBak;
			outputStream = outputStreamBak;
			throw new IOException();
		}
		inputStream = inputStreamBak;
		outputStream = outputStreamBak;
	}
	
	private void readObject(java.io.ObjectInputStream in) throws IOException, ClassNotFoundException {
		in.defaultReadObject();
		if (outputStreamName.equals("user")) {
			outputStream = System.out;
		}
		if (inputStreamName.equals("user")) {
			inputStream = System.in;
		}
	}
}