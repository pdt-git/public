package org.cs3.pl.metadata;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;

/**
 * This class is intended as a TEMPORARY solution. it contains query related
 * conveniance methods, formerly found in the PrologClient.
 */
public class DefaultMetaInfoProvider implements IMetaInfoProvider {
	public static final boolean windowsPlattform = System
			.getProperty("os.name").indexOf("Windows") > -1;

	private PrologInterface pif = null;

	private String pdtModulePrefix = "";

	public DefaultMetaInfoProvider(PrologInterface pif) {
		this.pif = pif;
	}

	public DefaultMetaInfoProvider(PrologInterface pif, String prefix) {
		this.pif = pif;
		this.pdtModulePrefix = prefix;
	}

	public static String makeFilenameSWIConform(String file) {
		if (windowsPlattform)
			return file.toLowerCase().replace('\\', '/');
		return file;
	}

	public void consult(String filename) throws PrologException {

		PrologSession session = pif.getSession();
		session
				.queryOnce("consult('" + makeFilenameSWIConform(filename)
						+ "')");
		session.dispose();
	}

	public boolean assertFact(String text) throws PrologException {
		PrologSession session = pif.getSession();
		Map r = session.queryOnce("assert(" + text + ")");
		session.dispose();
		return r != null;
	}

	public SourceLocation getLocation(String functor, int arity,
			String contextFile) throws PrologException {
		// return (SourceLocation) catchedCall(ROLE, "getLocation", new
		// Object[]{
		// functor, new Integer(arity), filename});
		// if (!isCompleted())
		// abort();
		PrologSession session = pif.getSession();
		Map solution = session.queryOnce(pdtModulePrefix + "get_file_pos('"
				+ contextFile + "', " + functor + ", " + arity
				+ ",File,Pos,_,_)," +
				// lu: we want to know where this information come from, so we
				// ask.
				"( meta_data(File,_,_,_,_,_,_,_,_)," + "  Parsed=yes"
				+ "; Parsed=no" + ")");
		if (solution == null) {
			session.dispose();
			return null;
		}
		boolean parsed = "yes".equals(solution.get("Parsed"));
		SourceLocation location = new SourceLocation(solution.get("File")
				.toString(), parsed, !parsed);
		location.file = solution.get("File").toString();
		if (parsed) {
			location.offset = Integer.parseInt(solution.get("Pos").toString());
		} else {
			location.line = Integer.parseInt(solution.get("Pos").toString());
		}

		Debug.debug("getLocation solution: " + location.file + ", "
				+ location.line);
		session.dispose();
		return location;

	}

	/**
	 * @param file
	 * @return
	 * @throws PrologException
	 * @throws NumberFormatException
	 */

	public Predicate[] getPredicatesWithPrefix(String module, String prefix)
			throws NumberFormatException, PrologException {
		return getPredicatesWithPrefix(module, prefix, null);
	}

	/**
	 * //TODO: add pos, len(?), dyn, multi.. Retrieves Predicates with prefix
	 * <i>prefix </i>. There to ways to restrict the returned elements: module
	 * and filename
	 * 
	 * @param prefix
	 * @param module
	 *            can be null -> no restriction on the module
	 * @param filename
	 *            can be null -> no restriction on the declaring file
	 * @module Module name or null, if module is not defined.
	 * @return
	 * @throws PrologException
	 * @throws NumberFormatException
	 */
	public Predicate[] getPredicatesWithPrefix(String module, String prefix,
			String filename) throws NumberFormatException, PrologException {
		// return
		// (PrologElementData[])predicates.get(makeFilenameSWIConform(filename));
		PrologSession session = pif.getSession();

		if (module == null)
			module = "_";
		if (filename == null)
			filename = "_";
		String query = pdtModulePrefix + "find_pred('"
				+ filename + "','" + prefix + "', " + module
				+ ",Name,Arity,Public)";
		List results = session.queryAll(query);
		List list = new ArrayList();
		// while (result != null) {
		for (Iterator it = results.iterator(); it.hasNext();) {
			Map result = (Map) it.next();
			boolean pub = Boolean.valueOf(result.get("Public").toString())
					.booleanValue();
			Predicate data = new PredicateData(module, result.get("Name")
					.toString(), Integer.parseInt(result.get("Arity")
					.toString()), pub, false, false);
			list.add(data);

		}
		session.dispose();
		return (Predicate[]) list.toArray(new Predicate[0]);
	}

	public String getSummary(Predicate data) throws PrologException {
		String help = getHelp(data);
		if (help == null)
			return null;
		return help.substring(0, help.indexOf('\n'));
	}

	public Clause[] retrievePrologElements(String file) throws PrologException {
		PrologSession session = pif.getSession();

		List results = session.queryAll(/*
										 * "bagof([Pos_,Len_]," +
										 */"meta_data" + "('" + file
				+ "',Module,Name,Arity,Public,Pos,Len, Dyn,Mul)"
		/* +",[[Pos,Len]|_])" */);
		List list = new ArrayList();
		for (Iterator it = results.iterator(); it.hasNext();) {
			Map result = (Map) it.next();
			// debug(result.get("Name").toString()+" - PUBLIC-
			// "+Boolean.valueOf(result.get("Public").toString()).booleanValue());

			SourceLocation sl = new SourceLocation(file, true, false);
			sl.offset = Integer.parseInt(result.get("Pos").toString());
			sl.endOffset = sl.offset
					+ Integer.parseInt(result.get("Len").toString());
			Clause data = new ClauseData(result.get("Module").toString(),
					result.get("Name").toString(), java.lang.Integer
							.parseInt(result.get("Arity").toString()), Boolean
							.valueOf(result.get("Public").toString())
							.booleanValue(), result.get("Dyn").toString()
							.equals("1"), result.get("Mul").toString().equals(
							"1"), sl);
			list.add(data);

		}
		session.dispose();
		return (Clause[]) list.toArray(new Clause[0]);
	}

	public String getHelp(Predicate data) {

		PrologSession session = pif.getSession();
		Map table = null;
		try {
			table = session.queryOnce("manual_entry(" + data.getLabel() + ","
					+ data.getArity() + ",Info)");
		} catch (PrologException e) {
			Debug.report(e);
		} finally {
			session.dispose();
		}
		if (table != null)
			return table.get("Info").toString().replaceAll("\\\\n", "\n");
		return null;
	}

	public SourceLocation[] findReferences(Predicate data) {
		// TODO Auto-generated method stub
		return null;
	}

	public Clause[] findClauses(Predicate p) {
		PrologSession session = pif.getSession();
		try {
			
			String query= "meta_data(File,"
				+"'"+p.getModule()+"',"
				+"'"+p.getLabel()+"',"
				+p.getArity()+","
				+"_,"//wether it is public - we already know this.
				+"Pos,"
				+"Len,"
				+"_,"//wether it is dynamic - we already know this.
				+"_)"//wether it is multifile - we already know this.
				;
				
				
			List l = session.queryAll(query);
			if (l==null){
				return new Clause[0];
			}
			Clause[] result = new Clause[l.size()];
			int i=0;
			for (Iterator it = l.iterator(); it.hasNext();i++) {
				Map m = (Map) it.next();
				SourceLocation sl = new SourceLocation((String) m.get("File"),true,false);				
				sl.offset=Integer.parseInt( (String) m.get("Pos"));
				sl.endOffset=sl.offset+Integer.parseInt( (String) m.get("Len"));				
				result[i]=new ClauseData(p.getModule(),p.getLabel(),p.getArity(),p.isPublic(),p.isDynamic(),p.isMultifile(),sl);
			}
			return result;
		} finally {
			if (session != null) {
				session.dispose();
			}
		}

	}

	/*
	 * FIXME: the current implementation will simply guess matching predicates
	 * looking at name and arity. That is, if the correct predicate exists and its
	 * definition was parsed, it is guaranteed to be included in the result.
	 * 
	 * Best we can do atm. :-(
	 * 
	 */
	public Predicate[] findPredicates(Goal g) {
		Set result = new HashSet();
		PrologSession session = pif.getSession();
		try{
			String query= "meta_data(File,"
				+"Module,"
				+"'"+g.getLabel()+"',"
				+g.getArity()+","
				+"Pub,"
				+"_,"
				+"_,"
				+"Dyn,"
				+"Mult)"
				;	
			List l = session.queryAll(query);
			for (Iterator it = l.iterator(); it.hasNext();) {
				Map m = (Map) it.next();
				String module = (String) m.get("Module");
				String label = g.getLabel();
				int arity = g.getArity();
				boolean pub = "1".equals(m.get("Pub"));
				boolean dyn = "1".equals(m.get("Dyn"));
				boolean mult = "1".equals(m.get("Mult"));
				result.add(new PredicateData(module, label,arity,pub,dyn,mult));
			}
		}
		finally{
			if(session!=null){
				session.dispose();
			}
		}
		return (Predicate[]) result.toArray(new Predicate[result.size()]);
	}

}
