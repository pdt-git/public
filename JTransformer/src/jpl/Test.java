package jpl;


public class Test {

	public static int fac(int n) {
		if (n == 1) {
			return 1;
		} else {
			return n * ((Integer) new Query(new Compound( "jpl_test_fac", new Term[] { new Integer(n - 1), new Variable("F")})).oneSolution().get("F")).value();
		}
	}
}
