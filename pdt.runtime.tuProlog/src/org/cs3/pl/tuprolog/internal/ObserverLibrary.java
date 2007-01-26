package org.cs3.pl.tuprolog.internal;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;

import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceListener;

import alice.tuprolog.InvalidTermException;
import alice.tuprolog.Library;
import alice.tuprolog.MalformedGoalException;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Term;

public class ObserverLibrary extends Library {
	private static final long serialVersionUID = 1L;
	
	/**
	 * Observations class is a hashtable used to store 
	 * observations that we are interested, along with their observers thread.
	 *  
	 * @author Hasan Abdel Halim
	 *
	 */
	private class Observations extends Hashtable{

		private static final long serialVersionUID = 3533380814249692805L;

		/**
		 * Replaces the default containsKey method of HashTable so that it 
		 * finds whether there is a key structually equal to the key provided
		 * or not.
		 * 
		 * @param key to look for.
		 * @return true if a structually equal key was found, false otherwise.
		 */
		public boolean containsKey( String key ){
			
			for (Iterator iter = keySet().iterator(); iter.hasNext();) {
				
				String subject = (String) iter.next();
				try {
					SolveInfo info = engine.solve(subject+"=@="+key+".");
					
					if (info.isSuccess())
						return true;
					
				} catch (MalformedGoalException e) {
					e.printStackTrace();
				}				
			}
			return false;
		}
		
		public Object get( String key ){
			
			for (Iterator iter = keySet().iterator(); iter.hasNext();) {
				
				String subject = (String) iter.next();
				try {
					SolveInfo info = engine.solve(subject+"=@="+key+".");
					
					if (info.isSuccess())
						return super.get(subject);
					
					
				} catch (MalformedGoalException e) {
					e.printStackTrace();
				}				
			}
			
			return null;
		}
		
	}

	public String getName() {
		return "ObserverLibrary";
	}
	
	
	/**
	 * Notifier Thread is a thread used to inform all observers related to a specific subject
	 * with a provided msg. 
	 * 
	 * @author Hasan Abdel Halim
	 *
	 */
	private class Notifier extends Thread{
		private String subject = "";
		private String msg= "";
		
		public Notifier(String subject, String msg) {
			this.subject = subject;
			this.msg = msg;
		}
		
		public void run(){
			if ( subject == "" )
				return;

			if ( observations.containsKey(subject) ){
				ArrayList list = (ArrayList) observations.get(subject);
				
				if (list == null )
					return;
				
				for (Iterator iter = list.iterator(); iter.hasNext();) {
					PrologInterfaceListener ls = (PrologInterfaceListener) iter.next();
					//ls.onUpdate(msg);
					ls.update(new PrologInterfaceEvent(this, subject, msg));
				}
			}
		}
	}
	
	private Observations observations = new Observations();
	
	public void dismiss() {
		observations.clear();
	}
	
	public boolean observe_1(Term sub) {
		boolean result = false;
		Term subject = sub.getTerm();
		
		if ( observations.containsKey( subject.toString() ) )
			return false;

		observations.put(subject.toString(), new ArrayList() );	
		
		try {
			//FIXME Observations depends on Sync Library.It should be loaded before usage.
			SolveInfo info = engine.solve("sync:init_idb("+subject.getTerm()+").");
			result = info.isSuccess();
		} catch (MalformedGoalException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return  result;	
	}
	
	public boolean unobserve_1(Term subject) {
		if ( !observations.containsKey( subject.toString() ))
			return false;

		observations.remove(subject.toString());
		return true;
	}

	public boolean pif_notify_2(Term subject, Term msg){
		Notifier thread = new Notifier(subject.getTerm().toString(), msg.toString());		
		thread.start();
		
		return true;
	}
	
	public void addListener(String subject, PrologInterfaceListener listener) throws InvalidTermException {

		if (!observations.containsKey(subject))
			observe_1(Term.parse(subject));
		
		ArrayList list = (ArrayList) observations.get(subject);
		list.add(listener);			
	}
	
	public void removeListener(String subject, PrologInterfaceListener listener){
		if (observations.containsKey(subject)){
			ArrayList list = (ArrayList) observations.get(subject);
			list.remove(listener);
		}
	}

	public void removeAllListeners( String subject ){
		if (observations.containsKey(subject)){
			ArrayList list = (ArrayList) observations.get(subject);
			list.clear();
		}
	}

}
