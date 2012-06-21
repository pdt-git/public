package org.cs3.plunit.matcher;

import java.util.List;

import org.hamcrest.Description;
import org.hamcrest.Factory;
import org.hamcrest.Matcher;
import org.hamcrest.TypeSafeMatcher;
import org.hamcrest.collection.IsCollectionWithSize;

import com.google.common.collect.Iterables;


public class PrologResultsInAnyOrder extends TypeSafeMatcher<List<List<String>>> {
	
	
	private Object[][] strings;
	private boolean[] lineNumbers;

	public PrologResultsInAnyOrder() {
		
	}
	
    public PrologResultsInAnyOrder(Object[]... strings) {
		this.strings = strings;
		lineNumbers=new boolean[strings.length];
		for (int i=0;i<strings.length;i++){ 
			lineNumbers[i]=false;
		}
		
	}

	

	@Override
	protected boolean matchesSafely(List<List<String>> item) {
		if(Iterables.size(item) < strings.length)
			return false;
		
		for (Iterable<String> iterable : item) {
			matchLine(iterable);
		}
		
		for (boolean bool : lineNumbers) {
			if(bool == false){
				return false;
			}
			
		}
		return true;
	}

	private void matchLine(Iterable<String> iterable) {
		for(int j=0;j<strings.length;j++){
			for(int i=0;i<strings.length;i++){
				if(containsAll(iterable, strings[j])){
					lineNumbers[j]=true;
				}
			}
		}
	}

	@Override
	public void describeTo(Description description) {
		description.appendValueList("[", ",", "]", strings);
		
	}

	private boolean containsAll(Iterable<String> iterable, Object[] array){
		for (Object string : array) {
			if(!Iterables.contains(iterable, string)){
				return false;
			}
		}
		return true;
	}

	
	
	/**
     *  Matches if all arguments are contained in any order in the iterable
     */
	@Factory
	public static  Matcher<List<List<String>>> has(Object[]... strings) {
		return new PrologResultsInAnyOrder(strings);
	}

	/**
	 * matches if the Iterable<String> object has size n;
	 * @param size
	 */
	@Factory
	public	static <E> Matcher<? super List<E>> hasAnswers(int n) {
		return IsCollectionWithSize.<E>hasSize(n);
	}
	
}

