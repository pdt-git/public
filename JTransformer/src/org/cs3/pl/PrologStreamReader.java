package org.cs3.pl;
import java.io.BufferedReader;
/**
 * @author windeln
 * 
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates. To enable and disable the creation of
 * type comments go to Window>Preferences>Java>Code Generation.
 */
abstract public class PrologStreamReader extends Thread {
	protected BufferedReader reader;
	private boolean errStream;
	private boolean EOL = false; 
	
	public PrologStreamReader(BufferedReader reader, boolean errStream) {
		this.reader = reader;
		this.setPriority(Thread.NORM_PRIORITY);
		this.errStream = errStream;
	}
	public void run() {
		StringBuffer prologChars = new StringBuffer();
		int prologChar;
		int lineIndex = 0;
		while (true) {
			try {
				prologChars.setLength(0);
				char c = (char) reader.read();
				prologChars.append(c);
				while (reader.ready() && c != 10) {
					c = (char) reader.read();
					prologChars.append(c);
//					System.out.println(reader.read() + " " + c);
				}
				lineIndex++;
//				System.out.println("line " + lineIndex);
				if (reader.ready())
					EOL = false;
				else
					EOL = true;
					
				consumeChars(prologChars.toString());
			} catch (Exception e) {
				Debug.report(e);
				break;
			}
		}
	}
	/**
	 * Method readLine.
	 * 
	 * @param prologLine
	 */
	abstract protected void consumeChars(String prologLine);
	/**
	 * @return Returns the eOL.
	 */
	public boolean isEOL() {
		return EOL;
	}
	/**
	 * @param eol The eOL to set.
	 */
	public void setEOL(boolean eol) {
		EOL = eol;
	}
}
