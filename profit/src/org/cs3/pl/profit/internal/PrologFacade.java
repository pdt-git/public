package org.cs3.pl.profit.internal;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.common.Util;

public class PrologFacade {

	static PrologInterface pif = PrologInterfaceFactory.newInstance().create();

	public static Object[] queryAll(String query) throws PrologException,
			PrologInterfaceException {
		PrologSession session = pif.getSession();
		List results = session.queryAll(query);
		session.dispose();
		return results.toArray();
	}

	public static Map queryOnce(String query) throws PrologInterfaceException {
		PrologSession session = pif.getSession();
		Map result = session.queryOnce(query);
		session.dispose();
		return result;
	}

	public static boolean consult(File file) throws PrologInterfaceException {
		return queryOnce("consult('" + Util.prologFileName(file) + "')") != null;
	}

	public static boolean assertFactOrRule(String factOrRule)
			throws PrologInterfaceException {
		return queryOnce("assert(" + factOrRule + ")") != null;
	}

	public static boolean retractFactOrRule(String factOrRule)
			throws PrologInterfaceException {
		return queryOnce("retract(" + factOrRule + ")") != null;
	}
}
