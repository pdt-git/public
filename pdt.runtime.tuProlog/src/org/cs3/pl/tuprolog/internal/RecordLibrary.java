package org.cs3.pl.tuprolog.internal;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import alice.tuprolog.Int;
import alice.tuprolog.Library;
import alice.tuprolog.Number;
import alice.tuprolog.Term;
import alice.tuprolog.Var;

public class RecordLibrary extends Library {

	private Hashtable rcds_db = new Hashtable();
	/**
	 * TODO: The current implementation is not efficient, it needs to be optimized.
	 */
	private static final long serialVersionUID = 1L;

	
	public boolean recorda_2(Term key, Term value){
		Term ref = new Var();
		
		return ( recorda_3(key, value, ref) );
	}	
	
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
		}else 
			values = (ArrayList) rcds_db.get(tmp);
				
		RecordEntry entry = new RecordEntry(_value);
		values.add(0, entry);
		rcds_db.put(tmp, values);
		
		return unify(ref, new Int(entry.getRef()));
	}

	public boolean recordz_2(Term key, Term value){
		Term ref = new Var();
		
		return ( recordz_3(key, value, ref) );
	}		
	
	public boolean recordz_3(Term key, Term value, Term ref){
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
		}else 
			values = (ArrayList) rcds_db.get(tmp);
		
		RecordEntry entry = new RecordEntry(_value);
		values.add(entry);
		rcds_db.put(tmp, values);
		
		return unify(ref, new Int(entry.getRef()));		
	}
	
	public boolean recorded_2(Term key, Term value){
		Term ref = new Var();
		
		return ( recorded_3(key, value, ref) );
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
		
		
		int index = -1;
		
		for (Iterator it = values.iterator(); it.hasNext();) {
			RecordEntry en = (RecordEntry) it.next();
			if ( en.getTerm().equals(_value) )
				index = en.getRef();
		}
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

	public boolean erase_1(Term ref){
		
		if ( ! ref.getTerm().isNumber() )
			return false;
		
		Number _ref = (Number) ref.getTerm() ;		
		
		for (Iterator keys = rcds_db.keySet().iterator(); keys.hasNext();) {
			Term key = (Term) keys.next();
			
			for (Iterator values = ((ArrayList)rcds_db.get(key)).iterator(); values.hasNext();) {
				RecordEntry en = (RecordEntry) values.next();
				
				if ( en.getRef() == _ref.intValue() ){
					values.remove();
					return true;
				}
			}
		}
		
		return false;
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
