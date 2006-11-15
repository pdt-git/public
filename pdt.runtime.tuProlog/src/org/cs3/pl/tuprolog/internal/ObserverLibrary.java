package org.cs3.pl.tuprolog.internal;

import java.util.HashMap;

import alice.tuprolog.Library;
import alice.tuprolog.MalformedGoalException;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Term;

public class ObserverLibrary extends Library {
	private HashMap listeners = new HashMap();

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	public boolean observe_3(Term thread, Term subject, Term key) {
		
		SolveInfo info;
		
		try {
			info = engine.solve("recorded( observer," +
										" observation("+ thread + "," +" OtherSubject, "+ key+"))," +
										"OtherSubject =@= "+subject+".");
			if (info.isSuccess())
				return false;
			
			info = engine.solve("recordz(observer, observation("+thread+","+subject+","+key+")).");
			
			return  info.isSuccess();
			
		} catch (MalformedGoalException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return false;
		
	}
	
	public boolean unobserve_2(Thread thread, Term subject) {
		SolveInfo info ;
		
		try {
			info = engine.solve("recorded( observer," +
					" observation("+ thread + "," +" OtherSubject, Key), Ref)," +
					"OtherSubject =@= "+subject+", erase(Ref).");
			
			return info.isSuccess();
		} catch (MalformedGoalException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return false;
	}

	public boolean notify_2(Term subject, Term msg){
		
		return true;
	}
}
