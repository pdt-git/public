package org.cs3.pl.tuprolog.internal;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import alice.tuprolog.Int;
import alice.tuprolog.Library;
import alice.tuprolog.Number;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Struct;
import alice.tuprolog.Term;
import alice.tuprolog.Var;

public class SWICompatibilityLibrary extends Library {
	
	// A hashtable which stores the records of recorda,recordz, and recorded predicates.
	private Hashtable records_db = new Hashtable();

	// Temperary Hashtable used while evaluating structural equality.
	private Hashtable hsh = new Hashtable();
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 *  Overloaded method to fix HashTable.containsKey since it checks the 
	 *  equality for both hashCode() and equals(). 
	 */
	private Term containsKey(Term key){
		Term _result = null;
		
		for (Iterator it = records_db.keySet().iterator(); it.hasNext();) {
			Term element = (Term) it.next();
			
			if ( key.equals(element)){
				_result = element;
				break;
			}
		}
		
		return _result;
		
	}
	

	/*
	 * 
	 * 	Implementation of Operators Extensions.
	 * 
	 * 
	 */	
	
	/**
	 * method invoked when the engine is going
	 * to demonstrate a goal
	 */
	public void onSolveBegin(Term goal) {
		hsh = new Hashtable();
	}
	
	/**
	 * method invoked when the engine has
	 * finished a demostration
	 */
	public void onSolveEnd() {
		hsh = null;
	}
	
	
	/**
	 * Structural Equality '=@=': TuProlog does not support structural
	 * equality yet.  
	 * 
	 * Two terms are structurally equal if  their tree representation is identical,
	 * and they  have the  same `pattern' of variables.
	 * 
	 * @param x The first term to be compared.
	 * @param y The second term to be compared.
	 * @return true if both terms are structurally equal, otherwise false.
	 */
	public boolean structEq_2(Term x,Term y){
		/*
		 * Extracts real Terms from TuProlog bindings.
		 * => a(b,c) compound is passed as X_e / a(b,c).
		 */
		Term first_term = x.getTerm();
		Term second_term = y.getTerm();
		
		if (first_term.isEqual(second_term))
			return true;

		if (first_term.isVar() && second_term.isVar() )
			return true;
		
		if(first_term.isCompound() && second_term.isCompound() ) {
			Struct frst =((Struct)first_term);
			Struct scnd =((Struct)second_term);
			/*
			 * checks the functor name
			 */
			if ( frst.getName() != scnd.getName() )
				return false;
			
			int arity = frst.getArity();
			/*
			 * checks the arity number
			 */
			if ( arity == scnd.getArity()){
			
				for (int i = 0; i < arity ; i++) {
					Term arg_1 = frst.getArg(i);
					Term arg_2 = scnd.getArg(i);

					/*
					 * Calls recursivelly structural equality between subterms.
					 */
					if (!structEq_2(arg_1 , arg_2 ))
						return false;
					/*
					 * Hashtable which stores the position of occurance for each 
					 * Var subterm.
					 */
					if (arg_1.isVar())
						if ( hsh.containsKey(arg_1) || hsh.containsValue(arg_2)){
							if ( ((Term)hsh.get(arg_1)) != arg_2)
								return false;
						} else{
							hsh.put(arg_1, arg_2);
						}
				}
				
				/*
				 * All subterms are structurally equal.
				 */
				return true;				
			}
			
			/*
			 * differenet arities.
			 */
			return false;
		}
		
		/*
		 *  The rest are false
		 */
		return false;
	}
	

	
	/*
	 * 
	 * 	Implementation of Exceptions Handling.
	 * 	- throw/1
	 * 	- catch/3
	 * 
	 */
	
	/**
	 * TuProlog does not support Exceptions yet.
	 * 
	 * throw(Exception): 
	 * 	Raise  an exception.
	 * 
	 * @param exception An exception to be thrown.
	 * @return
	 * @throws Exception
	 * @throws TuPrologThrowable
	 */
	public boolean throw_1(Term exception) throws Exception, TuPrologThrowable {
		
		if ( exception.getTerm().isAtomic())
			throw new TuPrologThrowable( exception.toString());
		
		return true;
	}
	
	/**
	 * TuProlog does not support Exceptions yet.
	 * 
	 * catch(goal, catcher, recover): 
	 * 	Catch an exception thrown by throw/1 predicate.
	 * 
	 * @param goal A goal to execute.
	 * @param catcher An exception expected to be caught.
	 * @param recover A goal to execute in case of an exception.
	 * @return
	 */
	public boolean catch_3(Term goal, Term catcher, Term recover){
		
		try{
			engine.solve(goal);
		}catch(Throwable ex){
			System.err.println("Prolog Exception occured ..!"+ex.getMessage());
		}
		return true;
	}

	
	/*
	 * 
	 * 	Implementation of Recorded DB.
	 * 	- recorda/2
	 * 	- recorda/3
	 * 	- recordz/2
	 * 	- recordz/3
	 * 	- recorded/2
	 * 	- recorded/3 
	 * 
	 */		
	
	/**
	 * TODO: The current implementation is not efficient, it needs to be optimized.
	 */

	/**
	 * recorda(+Key, +Term) :
	 * 	Equivalent to recorda(Key, Value,  _).
	 * 
	 * @param key A key to store values under.
	 * @param value A value to be stored under key.
	 * @return
	 */
	public boolean recorda_2(Term key, Term value){
		Term ref = new Var();
		
		return ( recorda_3(key, value, ref) );
	}	

	/**
	 * recorda(+Key, +Term, -Reference) :
	 * 	Assert  Term  in the  recorded  database under key  Key. Key  is an  integer,
	 *  atom or  term. Reference  is unified  with a  unique reference to the record
	 *  (see erase/1).
	 *   
	 * @param key	A key to store values under.
	 * @param value	 A value to be stored under key.
	 * @param ref	A unique integer used as reference to the value stored.
	 * @return
	 */
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
			values = (ArrayList) records_db.get(tmp);
				
		RecordEntry entry = new RecordEntry(_value);
		values.add(0, entry);
		records_db.put(tmp, values);
		
		return unify(ref, new Int(entry.getRef()));
	}
	
	/**
	 * recordz(+Key, +Term):
	 * 	Equivalent to recordz(Key, Value,  _).
	 * 
	 * @param key A key to store values under.
	 * @param value A value to be stored under key.
	 * @return
	 */
	public boolean recordz_2(Term key, Term value){
		Term ref = new Var();
		
		return ( recordz_3(key, value, ref) );
	}	
	
	/**
	 * recordz(+Key, +Term, -Reference):
	 * 	Equivalent to recorda/3, but  puts the Term at the tail of the terms
	 *  recorded under Key.
	 *  
	 * @param key	A key to store values under.
	 * @param value	 A value to be stored under key.
	 * @param ref	A unique integer used as reference to the value stored.
	 * @return
	 */
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
			values = (ArrayList) records_db.get(tmp);
		
		RecordEntry entry = new RecordEntry(_value);
		values.add(entry);
		records_db.put(tmp, values);
		
		return unify(ref, new Int(entry.getRef()));		
	}
	
	/**
	 * recorded(+Key, -Value):
	 * 	Equivalent to recorded(Key, Value,  _).
	 * 
	 * @param key A key to store values under.
	 * @param value A value to be stored under key.
	 * @return
	 */
	public boolean recorded_2(Term key, Term value){
		Term ref = new Var();
		
		return ( recorded_3(key, value, ref) );
	}	
	
	/**
	 * recorded(+Key, -Value, -Reference):
	 * 	Unify  Value  with the  first  term recorded  under Key  which  does
	 *  unify.    Reference  is  unified with  the  memory location  of  the
	 *  record.
	 *  
	 * @param key	A key to store values under.
	 * @param value	 A value to be stored under key.
	 * @param ref	A unique integer used as reference to the value stored.
	 * @return
	 */
	public boolean recorded_3(Term key, Term value, Term ref){
		Term _key = key.getTerm();
		Term _value = value.getTerm();

		// Key should be bound
		if ( _key.isVar() )
			return false;
		
		Term tmp = containsKey(_key);
		
		if ( tmp == null )
			return false;
		
		List values = (ArrayList) records_db.get(tmp);
		
		
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
	
	/**
	 * erase(+Reference):
	 * 	Erase  a  record or  clause from  recorded  database. Reference  is  an
	 *  integer  returned by  recorda/3 or  recorded/3. Erase can only be called
	 *  once on  a record or clause. 
	 *  
	 * @param ref	A unique integer used as reference to a value stored in recorded_db.
	 * @return
	 */
	public boolean erase_1(Term ref){
		
		if ( ! ref.getTerm().isNumber() )
			return false;
		
		Number _ref = (Number) ref.getTerm() ;		
		
		for (Iterator keys = records_db.keySet().iterator(); keys.hasNext();) {
			Term key = (Term) keys.next();
			
			for (Iterator values = ((ArrayList)records_db.get(key)).iterator(); values.hasNext();) {
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
	 * 
	 * 	Implemenation of MultiThreading .
	 * 	- with_mutex/2
	 *  
	 */		
	
	/**
	 * A hashtable which stores all synchornization keys.
	 */
	private Hashtable monitors = new Hashtable();

	/**
	 * 
	 * @param key key to synchronize on.
	 * @param goal
	 * @return
	 */
	public boolean with_mutex_2(Struct key, Term goal) {
		
		Object monitor = monitors.get(key.getName());

		if(monitor == null) {
			monitor = new Object();
			monitors.put(key.getName(), monitor);
		}
		synchronized (monitor) {
			SolveInfo info = this.getEngine().solve(goal);
			return (info.isSuccess())?true:false;
		}
	}
	
	
	/*
	 * 
	 * 	Implemenation of the default theory .
	 * 
	 * 
	 */		
	
	
	/**
	 * The default Theory which will be used by SWICompatibilityLibrary once loaded.
	 * @see alice.tuprolog.Library#getTheory()
	 */
	public String getTheory(){
		 return ":- op(700, xfx, '=@='). \n" +
		 		":- op(700, xfx, '\\=@='). \n" +
		 		"Module:Predicate :- call(Predicate).\n"+
		 		"'=@='(X,Y):- structEq(X,Y).\n" +
		 		"'\\=@='(X,Y):- not structEq(X,Y).\n" ;
	}
}
