/* $LICENSE_MSG$ */

package org.cs3.plunit.matcher;

import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.anyOf;
import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Factory;
import org.hamcrest.Matcher;
public class CombinableMatcher<T> extends BaseMatcher<T> {

	private final Matcher<? super T> fMatcher;

	public CombinableMatcher(Matcher<? super T> matcher) {
		fMatcher= matcher;
	}

	@Override
	public boolean matches(Object item) {
		return fMatcher.matches(item);
	}

	@Override
	public void describeTo(Description description) {
		description.appendDescriptionOf(fMatcher);
	}

	@Factory
	public CombinableMatcher<T> and(Matcher<? super T> matcher) {
		return new CombinableMatcher<T>(allOf(matcher, fMatcher));
	}

	@Factory
	public CombinableMatcher<T> or(Matcher<? super T> matcher) {
		return new CombinableMatcher<T>(anyOf(matcher, fMatcher));
	}
}

