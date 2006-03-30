package org.cs3.pl.prolog.internal.socket;

import java.util.Iterator;
import java.util.Vector;

import junit.framework.TestCase;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.AsyncPrologSessionEvent;
import org.cs3.pl.prolog.AsyncPrologSessionListener;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologSession;

public class AsyncSocketSessionTest extends TestCase {
	
	private PrologInterface2 pif;
	private Recorder rec;
	private AsyncPrologSession session;
	
	protected void setUp() throws Exception {
		//Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		PrologInterfaceFactory factory = Factory.newInstance();
		pif = (PrologInterface2) factory.create();
		//pif.setOption(SocketPrologInterface.EXECUTABLE, "konsole --noclose -e xpce");
		pif.start();
		rec=new Recorder();
		session = pif.getAsyncSession();
		session.addBatchListener(rec);
	}
	
	protected void tearDown() throws Exception {
		pif.stop();
	}
	
	class Record{
		String method;
		AsyncPrologSessionEvent event;
		public Record(String method, AsyncPrologSessionEvent event) {
			this.method = method;
			this.event = event;
		}
	}
	class Recorder implements AsyncPrologSessionListener{
		public void clear(){
			records.clear();
		}
		public synchronized Record last(){
			return (Record) records.lastElement();
		}
		public String toString() {
			StringBuffer sb = new StringBuffer();
			boolean first=true;
			for (Iterator it = records.iterator(); it.hasNext();) {
				Record r = (Record) it.next();
				if(!first){
					sb.append(", ");
				}
				sb.append(r.method);
				sb.append('(');
				if(r.event.ticket instanceof String){
					sb.append(r.event.ticket==null?"null":r.event.ticket.toString());
				}
				else{
					sb.append(r.event.ticket==null?"null":"dummy");
				}
				sb.append(',');
				sb.append(r.event.message==null?"null":r.event.message);
				sb.append(',');
				sb.append(r.event.bindings==null?"null":"("+Util.prettyPrint(r.event.bindings)+")");
				sb.append(')');
				
				first=false;
			}
			return sb.toString();
		}
		Vector records = new Vector();
		public synchronized void joinComplete(AsyncPrologSessionEvent e) {
			records.add(new Record("joinComplete",e));
			notifyAll();
		}

		public synchronized void abortComplete(AsyncPrologSessionEvent e) {
			records.add(new Record("abortComplete",e));
			notifyAll();
		}

		public synchronized void goalSucceeded(AsyncPrologSessionEvent e) {
			records.add(new Record("goalSucceeded",e));
			notifyAll();
		}

		public synchronized void goalFailed(AsyncPrologSessionEvent e) {
			records.add(new Record("goalFailed",e));
			notifyAll();
		}

		public synchronized void goalRaisedException(AsyncPrologSessionEvent e) {
			records.add(new Record("goalRaisedException",e));
			notifyAll();
		}

		public synchronized void goalHasSolution(AsyncPrologSessionEvent e) {
			records.add(new Record("goalHasSolution",e));
			notifyAll();
		}

		public synchronized void goalSkipped(AsyncPrologSessionEvent e) {
			records.add(new Record("goalSkipped",e));
			notifyAll();
		}

		public synchronized void goalCut(AsyncPrologSessionEvent e) {
			records.add(new Record("goalCut",e));
			notifyAll();
		}

		public synchronized void batchComplete(AsyncPrologSessionEvent e) {
			records.add(new Record("batchComplete",e));
			notifyAll();
		}
		
	}
	
	public void test_queryOnce_sequence01() throws Throwable{
		//PrologSession session=pif.getSession();
		session.queryOnce("1", "member(A,[a,b,c])");
		session.queryOnce("2", "member(a,[a,b,c])");
		session.queryOnce("3", "member(a,[a,b,c)");
		session.queryOnce("4", "member(aA,[a,b,c])");
		session.dispose();
		assertEquals("goalHasSolution(1,null,(A-->a)), " +
				"goalSucceeded(1,null,null), " +
				"goalHasSolution(2,null,()), " +
				"goalSucceeded(2,null,null), " +
				"goalRaisedException(3,error(syntax_error(cannot_start_term), string(member(a,[a,b,c). . , 15)),null), "+
				"goalFailed(4,null,null), " +
				"batchComplete(null,null,null)", 
				rec.toString());
	}
	
	public void test_queryAll_sequence01() throws Throwable{
		//PrologSession session=pif.getSession();
		session.queryAll("1", "member(A,[a,b,c])");
		session.queryAll("2", "member(a,[a,b,c])");
		session.queryAll("3", "member(a,[a,b,c)");
		session.queryAll("4", "member(aA,[a,b,c])");
		session.dispose();
		assertEquals("goalHasSolution(1,null,(A-->a)), " +
				"goalHasSolution(1,null,(A-->b)), " +
				"goalHasSolution(1,null,(A-->c)), " +
				"goalSucceeded(1,null,null), " +
				"goalHasSolution(2,null,()), " +
				"goalSucceeded(2,null,null), " +
				"goalRaisedException(3,error(syntax_error(cannot_start_term), string(member(a,[a,b,c). . , 15)),null), "+
				"goalFailed(4,null,null), " +
				"batchComplete(null,null,null)", 
				rec.toString());
	}
	
	public void test_abort01() throws Throwable{
		session.queryOnce("1", "thread_self(Alias)");
		session.join();
		Record r =(Record) rec.records.get(0);
		final String alias = (String) r.event.bindings.get("Alias");
		session.queryAll("2", "repeat,thread_get_message(test(M))");
		final PrologSession syncSession = pif.getSession();
		
		synchronized (rec) {			
			syncSession.queryOnce("thread_send_message('"+alias+"',test(1))");
			rec.wait();
		}
		
		rec.clear();
		session.queryOnce("3", "should_be_skipped");
		new Thread(){
			public void run(){
				try {
					//we need to make sure that test(2) (see below) is send AFTER 
					//the abort call, otherwise, abort will lock up forever.
					sleep(100);
				} catch (InterruptedException e) {
					Debug.rethrow(e);
				}
				syncSession.queryOnce("thread_send_message('"+alias+"',test(2))");
			
			}
		}.start();
		session.abort();
		
		session.dispose();
		assertEquals("goalHasSolution(2,null,(M-->2)), " +
				"goalCut(2,null,null), " +
				"goalSkipped(3,null,null), " +
				"abortComplete(dummy,null,null), " +
				"batchComplete(null,null,null)",
				rec.toString());
		
	}
	
}
