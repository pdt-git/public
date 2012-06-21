package org.cs3.plunit.matcher;

import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Factory;
import org.hamcrest.Matcher;

/**
 * Tests that an Iterable<String> representing a Prolog Result is null
 * which means by conventions that it was not successful.
 */
public class IsNotSuccessful<E> extends BaseMatcher<E> {

	@Override
	public boolean matches(Object items) {
		return (items == null);
	}
	
	@Override
	public void describeTo(Description description) {
		description.appendText("query should fail");
	}

    /**
     * Matches a null representing a failing Prolog query.
     */
	@Factory
	public static <E> Matcher<E> fails() {
		return new IsNotSuccessful<E>();
	}

}
