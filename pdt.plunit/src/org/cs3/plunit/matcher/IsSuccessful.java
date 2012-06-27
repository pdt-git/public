package org.cs3.plunit.matcher;

import org.hamcrest.Description;
import org.hamcrest.Factory;
import org.hamcrest.Matcher;
import org.hamcrest.TypeSafeMatcher;

/**
 * Tests if collection is empty.
 */
public class IsSuccessful<E> extends TypeSafeMatcher<E> {


	@Override
	protected void describeMismatchSafely(E item, Description mismatchDescription) {
		mismatchDescription.appendText("query failed");
	}
	
	@Override
	public void describeTo(Description description) {
		description.appendText("query should succeed");
	}

    /**
     * Matches an not empty query result, the prolog query should succeed.
     */
	@Factory
	public static <E> Matcher<E> succeeds() {
		return new IsSuccessful<E>();
	}

	@Override
	protected boolean matchesSafely(E items) {
		// 22.10.10, dsp, Prolog fail <-> null
        return (items != null);
		// return !Iterables.isEmpty(item);
	}

}
