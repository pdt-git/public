/*
 */
package org.cs3.pl.prolog.internal;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.ConsultServiceEvent;
import org.cs3.pl.prolog.ConsultServiceListener;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;

/**
 */
public class DefaultConsultService implements ConsultService ,PrologInterfaceListener{
    private Vector listeners = new Vector();
    private PrologInterface pif;

    private Collector collector;

    private ResourceFileLocator locator;

    private String prefix;

    private class Collector extends ByteArrayOutputStream {
        private String name;

        Thread owner;

        public void setName(String name) {
            this.name = name;

        }

        public synchronized void close() throws IOException {
            PrologSession s = pif.getSession();
            s.consult(name, new ByteArrayInputStream(toByteArray()));
            this.reset();
            setName(null);
            s.dispose();
        }
    }

    public DefaultConsultService(PrologInterface pif, ResourceFileLocator locator) {
        this.pif = pif;
        this.locator = locator;
        
        pif.addPrologInterfaceListener(PrologInterface.SUBJECT_CONSULTED,this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IConsultService#getOutputStream(java.lang.String)
     */
    public PrintStream getOutputStream(String symbol) {
        if (collector == null) {
            collector = new Collector();
        }
        collector.setName(resolve(symbol));
        return new PrintStream(collector);
    }

    private String resolve(String symbol) {
        return Util.normalizeOnWindoze(locator.resolve(symbol).toString());
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IConsultService#isConsulted(java.lang.String)
     */
    public boolean isConsulted(String symbol) {
        PrologSession s = pif.getSession();
        Map map = s.queryOnce(resolve(symbol));
        return map != null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IConsultService#unconsult(java.lang.String)
     */
    public void unconsult(String s) {
        collector.setName(resolve(s));
        collector.reset();
        try {
            collector.close();
        } catch (IOException e) {
            Debug.report(e);
            throw new RuntimeException(e);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IConsultService#getTimeStamp(java.lang.String)
     */
    public long getRecordTimeStamp(String s) {
        return -1;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IConsultService#addConsultServiceListener(org.cs3.pl.prolog.ConsultServiceListener)
     */
    public void addConsultServiceListener(ConsultServiceListener l) {
        synchronized(listeners){
            if(!listeners.contains(l)){
                listeners.add(l);
            }
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IConsultService#removeConsultServiceListener(org.cs3.pl.prolog.ConsultServiceListener)
     */
    public void removeConsultServiceListener(ConsultServiceListener l) {
        synchronized(listeners){
            if(listeners.contains(l)){
                listeners.remove(l);
            }
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IConsultService#reload()
     */
    public void reload() {
      ;

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IConsultService#clearRecords()
     */
    public void clearRecords() {
       ;

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IConsultService#isRecording()
     */
    public boolean isRecording() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IConsultService#setRecording(boolean)
     */
    public void setRecording(boolean val) {
    ;

    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologInterfaceListener#update(org.cs3.pl.prolog.PrologInterfaceEvent)
     */
    public void update(PrologInterfaceEvent e) {
        ConsultServiceEvent e2 = new ConsultServiceEvent(this,e.getEvent());
        Vector cloned = null;
        synchronized(listeners){
            cloned=(Vector) listeners.clone();
        }
        for (Iterator it = cloned.iterator(); it.hasNext();) {
            ConsultServiceListener l= (ConsultServiceListener) it.next();
            l.consultDataChanged(e2);
        }
        
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.ConsultService#isAppendingRecords()
     */
    public boolean isAppendingRecords() {
       ;
        return false;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.ConsultService#setAppendingRecords(boolean)
     */
    public void setAppendingRecords(boolean val) {       
     ;   
    }

}
