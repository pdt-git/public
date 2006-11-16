package org.cs3.pl.tuprolog.internal;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;

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

	/**
	 * Listeners class is a linkedlist which stores a key we are interested in,
	 * along with its observers.
	 * 
	 * @author Hasan Abdel Halim
	 *
	 */
	private class Listeners extends ArrayList{

		private static final long serialVersionUID = 1L;
		private Term key;

		public Listeners(Term key) {
			super();
			this.key = key;
		}
		
		public Term getKey() {
			return key;
		}

		public void setKey(Term key) {
			this.key = key;
		}		
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
				
				Listeners list = (Listeners) observations.get(subject);
				
				if (list == null )
					return;
				
				for (Iterator iter = list.iterator(); iter.hasNext();) {
					ObservationListener ls = (ObservationListener) iter.next();
					ls.onUpdate(msg);					
				}
			}
		}
	}
	
	private Observations observations = new Observations();
	
	public boolean observe_2(Term subject, Term key) {
		
		if ( observations.containsKey( subject.toString() ) )
			return false;

		observations.put(subject.toString(), new Listeners(key) );	
		return  true;	
	}
	
	public boolean unobserve_1(Term subject) {

		if ( !observations.containsKey( subject.toString() ))
			return false;
		
		observations.remove(subject.toString());
		return true;
	}

	public boolean notify_2(Term subject, Term msg){
		
		Notifier thread = new Notifier(subject.toString(), msg.toString());
		
		thread.start();
		
		return true;
	}
	
	public void addListener(String subject, ObservationListener listener){
		if ( observations.containsKey(subject) ){
			Listeners list = (Listeners) observations.get(subject);
			list.add(listener);
		}
	}
	
	public void removeListener(String subject, ObservationListener listener){
		if ( observations.containsKey(subject) ){
			Listeners list = (Listeners) observations.get(subject);
			list.remove(listener);
		}
	}

}
