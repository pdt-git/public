package org.cs3.jlmp.tests; 

import java.util.HashMap;
import java.util.HashSet;
import java.util.Vector;

import junit.framework.TestCase;


/**
 *  A base class for tests that are organized in groups.
 * The main (only?) purpose of this class is to provide means
 * of executing code once (and only once) before and after a
 * group of tests has been run. 
 * Each subclass has to override getKey(). The value returned by
 * this method identfies the group to which each test run belongs,
 * <p>
 * The idea behin this is of course to alow a hughe number of tests to share a 
 * common environment, so for instance you don't  have to copy the
 * test workspace for each of your fact generation tests.
 * <p> 
 * I'm not shure if this does actualy undermines the philosophy of
 * unit tests, comments are welcome. 
 * <p>
 * please note that this is a quick hack. Lean gently upon it! 
 * In particular please read the comments on setUpOnce(), tearDownOnce().
 * <p>
 * ld: i maybe have found a more compact solution:
 * http://junit.sourceforge.net/doc/faq/faq.htm#organize_3
 * <p>
 * problem: how to make the wrapping transparent to subclasses?
 */
public abstract class SuiteOfTestCases extends TestCase{	
	/**
	 * a map of vectors maped to the type of their elems.
	 * For eacj test class, their is a vector containing its instances, i.e., the
	 * test runs.
	 */
	private static HashMap instanceLists = new HashMap();
	
	/**
	 * contains all classes, for which we have already called the setUpOnce method
	 */
	private static HashSet setUpClasses = new HashSet();
	
	public SuiteOfTestCases(String name) {
		super(name);		
		register();
	}
	
	private  void register() {
		
		Vector v = getInstanceList();
		if(!v.contains(this)){
			v.add(this);
		}
	}

	private Vector getInstanceList() {
		Object key = getKey();
		Vector result = null;
		if(instanceLists.containsKey(key)){
			result =(Vector)instanceLists.get(key);
		}
		else{
			result = new Vector();
			instanceLists.put(key,result);
		}
		return result;
	}

	/**
	 * @return
	 */
	protected abstract Object getKey() ;

	private  void unregister() {
		Vector v = getInstanceList();
		if(v.contains(this)){
			v.remove(this);
		}
	}
	
	protected void setUp() throws Exception {		
		super.setUp();
		callSetUpOnce();
	}
	
	protected void tearDown() throws Exception {
		super.tearDown();
		//ld: we assume that this test instance will not be run again.
		unregister();
		callTearDownOnce();
	}
	
	private  void callSetUpOnce(){
		Object key = getKey();
		if(setUpClasses.contains(key)) return;
		if(!instanceLists.containsKey(key)) return;
		Vector v = getInstanceList();
		if (v.isEmpty())return;
		setUpClasses.add(key);
		setUpOnce();
	}
	
	private  void callTearDownOnce(){
		Object key = getKey();
		if(!setUpClasses.contains(key)) return;
		if(!instanceLists.containsKey(key)) return;
		Vector v = getInstanceList();
		if (!v.isEmpty())return;		
		tearDownOnce();
		setUpClasses.remove(key);
	}
	
	/**
	 * This method will be called once for each group.
	 * It will be called within the first call to setup to an instance within the group.
	 * The instance on which this call happens is not determined.
	 * You should only access static members of your subtype within this
	 * class. Actualy this method should be static, but - well - 
	 * you could not override it then, could you.
	 * 
	 * 
	 */
	public  void setUpOnce(){}
	
	/**
	 * This method will be called once for each group.
	 * It will be called within the first call to setup to an instance within the group.
	 * The instance on which this call happens is not determined.
	 * You should only access static members of your subtype within this
	 * class. Actualy this method should be static, but - well - 
	 * you could not override it then, could you.
	 */	
	public  void tearDownOnce(){}
	
	
}