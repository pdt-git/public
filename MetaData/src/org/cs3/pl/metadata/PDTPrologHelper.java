
package org.cs3.pl.metadata;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.SessionException;

/**
 * This class is intended as a TEMPORARY solution. it contains 
 * query related conveniance methods, formerly found in the PrologClient.
 */
public class PDTPrologHelper implements IMetaInfoProvider{
    public static final boolean windowsPlattform = System
    .getProperty("os.name").indexOf("Windows") > -1;
    private PrologSession session =null;
	private String pdtModulePrefix="";

    
    public PDTPrologHelper(PrologSession session) {
    	this.session=session;
    }

    public PDTPrologHelper(String prefix,PrologSession session) {
    	this.session=session;
    	this.pdtModulePrefix=prefix;
    }
    
    public static String makeFilenameSWIConform(String file) {
        if (windowsPlattform)
            return file.toLowerCase().replace('\\', '/');
        return file;
    }

    public void consult( String filename) throws SessionException{
	    
	  
            session.query("consult('" + makeFilenameSWIConform(filename) + "')");
       
    }
	
    public  boolean assertFact( String text) throws SessionException{
        Hashtable r = session.query("assert("+text+")");	   
        return r!=null;
    }

    /**
     * Retrieves the location or a predicate in the running prolog prozess.
     * 
     * @param functor
     *                    The functor of the predicate.
     * @param arity
     *                    The arity of the predicate.
     * 
     * @return null, if no location can be found.
     * @throws SessionException
     * @see SourceLocation
     */
    public SourceLocation getLocation( String functor, int arity, String filename) throws SessionException {
        //		return (SourceLocation) catchedCall(ROLE, "getLocation", new
        // Object[]{
        //				functor, new Integer(arity), filename});
        //		if (!isCompleted())
        //		abort();
        Hashtable solution = session.query(pdtModulePrefix + "get_file_pos('"
                + filename + "', " + functor + ", " + arity + ",File,Pos,_,_)");
        if (solution == null)
            return null;
        SourceLocation location = new SourceLocation();
        location.file = /* removeQuotes( */solution.get("File").toString()/* ) */;
        location.line = Integer.parseInt(solution.get("Pos").toString());
        Debug
                .debug(
                        "getLocation solution: " + location.file + ", "
                                + location.line);
        return location;
    
    }

    /**
     * @param file
     * @return
     * @throws SessionException
     * @throws NumberFormatException
     */
    
    
    public PrologElementData[] getPredicatesWithPrefix(String module,
            String prefix) throws NumberFormatException, SessionException {
        return getPredicatesWithPrefix(module, prefix, null);
    }

    /**
     * //TODO: add pos, len(?), dyn, multi.. Retrieves Predicates with prefix
     * <i>prefix </i>. There to ways to restrict the returned elements: module
     * and filename
     * 
     * 
     * @param prefix
     * @param module
     *                    can be null -> no restriction on the module
     * @param filename
     *                    can be null -> no restriction on the declaring file
     * @module Module name or null, if module is not defined.
     * @return
     * @throws SessionException
     * @throws NumberFormatException
     */
    public PrologElementData[] getPredicatesWithPrefix(String module,
            String prefix, String filename) throws NumberFormatException, SessionException {
        //return
        // (PrologElementData[])predicates.get(makeFilenameSWIConform(filename));
    
        if (module == null)
            module = "_";
        if (filename == null)
            filename = "_";
        Hashtable result = session.query(pdtModulePrefix+ "find_pred('"
                + filename + "','" + prefix + "', " + module
                + ",Name,Arity,Public)");
        List list = new ArrayList();
        while (result != null) {
            boolean pub = Boolean.valueOf(result.get("Public").toString())
                    .booleanValue();
            PrologElementData data = new PrologElementData(result.get("Name")
                    .toString(), Integer.parseInt(result.get("Arity")
                    .toString()), pub, 0, 0, false, false);
            list.add(data);
            result =session.next();
        }
        return (PrologElementData[]) list.toArray(new PrologElementData[0]);
    }

    public PrologElementData[] retrievePrologElements(String file) throws SessionException {
        Hashtable result = session.query("bagof([Pos_,Len_],"
                + "meta_data"
                + "('"
                + file
                + "',Module,Name,Arity,Public,Pos_,Len_, Dyn,Mul),[[Pos,Len]|_])");
        List list = new ArrayList();
        while (result != null) {
            //debug(result.get("Name").toString()+" - PUBLIC-
            // "+Boolean.valueOf(result.get("Public").toString()).booleanValue());
            PrologElementData data = new PrologElementData(result.get("Name")
                    .toString(), java.lang.Integer.parseInt(result.get("Arity")
                    .toString()), Boolean.valueOf(
                    result.get("Public").toString()).booleanValue(),
                    java.lang.Integer.parseInt(result.get("Pos").toString()),
                    java.lang.Integer.parseInt(result.get("Len").toString()),
                    result.get("Dyn").toString().equals("1"), result.get("Mul")
                            .toString().equals("1"));
            list.add(data);
            result = session.next();
        }
        return (PrologElementData[]) list.toArray(new PrologElementData[0]);
    }

}
