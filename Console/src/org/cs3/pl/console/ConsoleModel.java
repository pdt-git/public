package org.cs3.pl.console;
/**
 * @author schulzs1
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public interface ConsoleModel {
	
	abstract public String getLineBuffer();
	abstract public void setLineBuffer(String line);
	
	abstract public void commitLineBuffer();
	
	abstract public void addConsoleListener(ConsoleModelListener cml);
	abstract public void removeConsoleListener(ConsoleModelListener cml);
	
	abstract public void  putSingleChar(char c);
	
	abstract public boolean isSingleCharMode();
	
	abstract public void shutdown();
		
}
