package org.cs3.pl.prolog;

public class CacheOutputPrologListener implements IPrologListener {

    private StringBuffer buf = new StringBuffer();

    public String getBuf() {
        return buf.toString();
    }

    public void enterCatchedCall(PrologEvent e) {
    }

    public void exitCatchedCall(PrologEvent e) {

    }

    public void newDataAvailable(PrologEvent e) {
        buf.append(e.getData());
    }
}