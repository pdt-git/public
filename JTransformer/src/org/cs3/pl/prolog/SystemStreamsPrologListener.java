package org.cs3.pl.prolog;

import org.cs3.pl.Debug;

public class SystemStreamsPrologListener implements IPrologListener {
    public void enterCatchedCall(PrologEvent e) {
        
    }
    public void exitCatchedCall(PrologEvent e) {
    }
    public void newDataAvailable(PrologEvent e) {
        Debug.debug(e.getData());
    }
	
	
}
