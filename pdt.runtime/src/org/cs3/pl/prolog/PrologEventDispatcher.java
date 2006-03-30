package org.cs3.pl.prolog;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

public class PrologEventDispatcher extends DefaultAsyncPrologSessionListener {
	
	private HashMap listenerLists = new HashMap();
	/*
	 * XXX i don't like the idea of keeping a reference to this
	 * session on the heap. 
	 * This has proven a bad practice in the past.
	 * Is there any other way to solve this?
	 */
	private AsyncPrologSession session;
	Object observerTicket = new Object();
	Object eventTicket = new Object();
	private PrologInterface2 pif;
	public PrologEventDispatcher(PrologInterface2 pif) {
		this.pif=pif;
	}
	protected void finalize() throws Throwable {
		if(session!=null){
			stop();
			
		}
	}
	
	public void addPrologInterfaceListener(String subject,
            PrologInterfaceListener l) {
        synchronized (listenerLists) {
            Vector list = (Vector) listenerLists.get(subject);
            if (list == null) {
                list = new Vector();
                listenerLists.put(subject, list);
                enableSubject(subject);
            }
            if (!list.contains(l)) {
                list.add(l);
            }
        }

    }

   
	/*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IPrologInterface#removePrologInterfaceListener(java.lang.String,
     *          org.cs3.pl.prolog.PrologInterfaceListener)
     */
    public void removePrologInterfaceListener(String subject,
            PrologInterfaceListener l) {
        synchronized (listenerLists) {
            Vector list = (Vector) listenerLists.get(subject);
            if (list == null) {
                return;
            }
            if (list.contains(l)) {
                list.remove(l);
            }
            if(list.isEmpty()){
            	disableSubject(subject);
            	listenerLists.remove(subject);
            }
        }

    }
    
    private void enableSubject(String subject) {
    	if(session==null){
    		session=pif.getAsyncSession();
    		session.addBatchListener(this);
    	}else{
    		abort();
    	}
    	PrologSession s = pif.getSession();
    	try{
    		s.queryOnce("observe('"+session.getProcessorThreadAlias()+"',"+subject+",'"+subject+"')");
    	}
    	finally{
    		s.dispose();
    	}
		//session.queryOnce(observerTicket,"thread_self(_Me),observe(_Me,"+subject+",'"+subject+"')");
		dispatch();
	}
	
	private void disableSubject(String subject) {
    	if(session==null){
    		return;
    	}
    	
    	abort(); 		
		session.queryOnce(observerTicket,"thread_self(_Me),unobserve(_Me,"+subject+")");
		if(!listenerLists.isEmpty()){
			dispatch();	
		}				
	}
	private void dispatch() {
		session.queryAll(eventTicket,"dispatch(Subject,Key,Event)");
	}
    private void abort() {
    	PrologSession s =pif.getSession();
    	try{
    		s.queryOnce("thread_send_message('"+session.getProcessorThreadAlias()+"',notify('$abort',_))");
    	}
    	finally{
    		if(s!=null){
    			s.dispose();
    		}
    	}
		
	}
    public void stop(){
    	if(session==null){
    		return;
    	}
    	abort();
    	session.dispose();
    	session=null;
    }
	
	/**
     * @param subject2
     * @param string
     */
    private void fireUpdate(String subject, String key, String event) {
        Vector listeners = (Vector) listenerLists.get(key);
        if (listeners == null) {
            return;
        }
        PrologInterfaceEvent e = new PrologInterfaceEvent(this, subject,key,
                event);

        Vector cloned = null;
        synchronized (listeners) {
            cloned = (Vector) listeners.clone();
        }
        for (Iterator it = cloned.iterator(); it.hasNext();) {
            PrologInterfaceListener l = (PrologInterfaceListener) it.next();
            l.update(e);
        }
    }
    
    public void goalHasSolution(AsyncPrologSessionEvent e) {
    	String subject = (String) e.bindings.get("Subject");
    	if("$abort".equals(subject)){
    		return;
    	}
    	String key = (String) e.bindings.get("Key");
    	String event = (String) e.bindings.get("Event");
    	fireUpdate(subject,key,event);
    }
	public void abortComplete(AsyncPrologSessionEvent e) {
		// TODO Auto-generated method stub
		super.abortComplete(e);
	}
	public void batchComplete(AsyncPrologSessionEvent e) {
		// TODO Auto-generated method stub
		super.batchComplete(e);
	}
	public void goalCut(AsyncPrologSessionEvent e) {
		// TODO Auto-generated method stub
		super.goalCut(e);
	}
	public void goalFailed(AsyncPrologSessionEvent e) {
		// TODO Auto-generated method stub
		super.goalFailed(e);
	}
	public void goalRaisedException(AsyncPrologSessionEvent e) {
		// TODO Auto-generated method stub
		super.goalRaisedException(e);
	}
	public void goalSkipped(AsyncPrologSessionEvent e) {
		// TODO Auto-generated method stub
		super.goalSkipped(e);
	}
	public void goalSucceeded(AsyncPrologSessionEvent e) {
		// TODO Auto-generated method stub
		super.goalSucceeded(e);
	}
	public void joinComplete(AsyncPrologSessionEvent e) {
		// TODO Auto-generated method stub
		super.joinComplete(e);
	}
}
