package org.cs3.pl.tuprolog.internal;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import alice.tuprolog.Int;
import alice.tuprolog.Library;
import alice.tuprolog.Term;

public class RecordLibrary extends Library {

	private Hashtable rcds_db = new Hashtable();
	/**
	 * TODO: The current implementation is not efficient, it needs to be optimized.
	 */
	private static final long serialVersionUID = 1L;

	
	public boolean recorda_3(Term key, Term value, Term ref){
		Term _key = key.getTerm();
		Term _value = value.getTerm();
		
		/*
		 * Key & Value should be bound to store a record
		 */
		if ( _key.isVar() || _value.isVar() )
			return false;
		
		Term tmp = containsKey(_key);
		List values = null ;
		
		if ( tmp == null )
		{
			values = new ArrayList();
			tmp = _key;
			System.err.println("Key was found.");
		}else {
			values = (ArrayList) rcds_db.get(tmp);
		}
		
		values.add(_value);
		rcds_db.put(tmp, values);
		
		int index = values.indexOf(_value);
		
		System.err.println("HashTabel: "  + rcds_db );
		return unify(ref, new Int(index));
		
	}

	public boolean recorded_3(Term key, Term value, Term ref){
		Term _key = key.getTerm();
		Term _value = value.getTerm();

		// Key should be bound
		if ( _key.isVar() )
			return false;
		
		Term tmp = containsKey(_key);
		
		if ( tmp == null )
			return false;
		
		List values = (ArrayList) rcds_db.get(tmp);
		
		int index = values.indexOf(_value);
		// Value was not found.
		if ( index == -1 )
			return false;
		
		return unify( ref, new Int(index) );
	}
	/*
	 *  Overloaded method to fix HashTable.containsKey since it checks the 
	 *  equality for both hashCode() and equals(). 
	 */
	private Term containsKey(Term key){
		Term _result = null;
		
		for (Iterator it = rcds_db.keySet().iterator(); it.hasNext();) {
			Term element = (Term) it.next();
			
			if ( key.equals(element)){
				_result = element;
				break;
			}
		}
		
		return _result;
		
	}
	/*
	 * (non-Javadoc)
	 * @see alice.tuprolog.Library#getTheory()
	 */
	public String getTheory(){
		/*
		String theory = "recorda(Key, Value):- nonvar(Key), assert( recorded_db(Key,Value)). \n" +
						"recordz(Key, Value):- nonvar(Key), assert( recorded_db(Key,Value)). \n" +
						"recorded(Key, Value):- recorded_db(Key,Value).";
		*/
		return "" ;
	}
}
