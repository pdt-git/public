/*
 * Created on 01.04.2004
 * 
 * To change the template for this generated file go to Window - Preferences -
 * Java - Code Generation - Code and Comments
 */
package org.cs3.pl.parser;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringBufferInputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import org.cs3.pl.common.Debug;
import org.cs3.pl.doc.PrologModule;
import org.cs3.pl.metadata.PrologElementData;



/**
 * @author xproot
 * 
 * To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Generation - Code and Comments
 */
public class PrologCompiler extends PrologParserTraversal {
    public static final String METADATA = "meta_data";

    public static final String METADATAHELP = METADATA + "_help";

    public static final String METADATAMODULE = METADATA + "_module";

    
    //private static String newLine = System.getProperty("line.separator");
    protected Hashtable pefs = new Hashtable();

    protected Hashtable vars = new Hashtable();

    protected List clauses = new ArrayList();

    protected String symbolicFileName;

    protected ProblemCollector problemCollector;

    protected LineBreakInfoProvider lineBreakInfoProvider;

    protected Hashtable singleton = new Hashtable();

    protected ASTCompilationUnit unit;

    public static final String MSG_COND_PAREN = "the condition part of the ct should be surrounded by parenthesis.";

    public static final String MSG_ACTION_PAREN = "the action part of the ct should be surrounded by parenthesis.";

    public static final String MSG_SINGLETON_VAR_PREFIX = "Singleton variable: ";

    public static final String MSG_SINGLETON_VAR_POSTFIX = " (one occurence only)";

    public static final int INFO = 0;

    public static final int WARNING = 1;

    public static final int ERROR = 2;

    protected static final String DEFAULTMODULE = "user";

    protected PrologParser parser;

    protected static final String LINESEPARATOR = System
            .getProperty("line.separator");

    protected static final int BUFLENGTH = 1024;

    protected ArrayList markers = new ArrayList();

    protected boolean singletonCheckForDTMVars=false;

    protected void resetVars() {
        vars = new Hashtable();
    }

    protected void resetSingleton() {
        singleton = new Hashtable();
    }

    public PrologCompiler() {
        initPEFs();
    }

    public Object visit(ASTCompilationUnit node, Object data) {
        node.childrenAccept(this, data);
        return node;
    }

    public Object visit(ASTVariable node, Object data) {
        if (node.getName().equals("_"))
            return node;
        //		if (node.getName().startsWith("_"))
        //			return node;
        if (vars.get(node.getName()) == null)
            vars.put(node.getName(), node);
        //		if(node.getName().equals("AGet"))
        //			System.out.println("DEBUG");
        if (node.getName().startsWith("_")
                && !singletonCheckForDTMVars)
            return node;
        if (singleton.get(node.getName()) == null) {
            List l = new ArrayList();
            l.add(node);
            singleton.put(node.getName(), l);
        } else
            ((List) singleton.get(node.getName())).add(node);
        return node;
    }

    public Object visit(ASTPredicateHead node, Object data) {
        //checkCTValidity(node, data);
        return node;
    }

    /**
     * @param boundVars
     * @param vars2
     */
    private void checkBoundVars(Hashtable boundVars, Hashtable vars) {
        for (Iterator iter = vars.keySet().iterator(); iter.hasNext();) {
            String s = (String) iter.next();
            if (boundVars.get(s) == null)
                addProblem(((Node) vars.get(s)).getToken(), "Variable '" + s
                        + "' is not bound by the condition part.", WARNING);
        }
    }

    /**
     * @param node
     * @throws CoreException
     */
    protected void addProblem(Token token, String msg, int severity) {
        if (problemCollector != null) {
            problemCollector.reportProblem(token, msg, severity);
        }

    }

    /**
     * @param file2
     * @param attributes
     * @param problem
     * @throws CoreException
     */
    /**
     * @param token
     * @param document
     * @param offset
     * @return
     */
    protected int getLineOffset(int line) {
        //		IEditorPart editor = PDTPlugin.getDefault().getActiveEditor();
        //		Document document = (Document) ((PLEditor) editor)
        //				.getDocumentProvider().getDocument(editor.getEditorInput());

        //		int offset = 0;
        //		try {
        //			offset = document.getLineInformation(line - 1).getOffset();
        //		} catch (BadLocationException e1) {
        //			Debug.report(e1);
        //		}
        //		return offset;
        if (lineBreakInfoProvider == null) {
            return 0;
        }
        return lineBreakInfoProvider.getOffsetAtLine(line-1);
    }

    public void compile(String content) {
        compile("", new StringBufferInputStream(content),
                new StringLineBreakInfoProvider(content));
    }

    /**
     * @param unit
     */
    public void compile(String symbolicFileName, InputStream content,
            LineBreakInfoProvider lineInfo) {
        this.symbolicFileName = symbolicFileName;
        setLineBreakInfoProvider(lineInfo);
        try {
            if (problemCollector != null) {
                problemCollector.reset();
            }

            parser = new PrologParser(content);
            try {
                parser.CompilationUnit();
                unit = getUnit();
                //PrologCompiler checker = new PrologCompilerBackend();
                getUnit().childrenAccept(this, null);
                List errors = parser.getErrors();
                for (Iterator iter = errors.iterator(); iter.hasNext();) {
                    ParseException ex = (ParseException) iter.next();
                    Token errorToken;
                    //				    if(ex.currentToken.next != null)
                    errorToken = ex.currentToken.next;
                    //				    else
                    //				        errorToken = ex.currentToken;
                    addProblem(errorToken, processParserOutput(ex
                            .getLocalizedMessage()), ERROR);

                }
            } catch (ParseException e) {
                Debug.report(e);
                //			try {
                //				addProblem(e.currentToken,
                // processParserOutput(e.getLocalizedMessage()),
                // IMarker.SEVERITY_ERROR);
                //			} catch (CoreException e1) {
                //				throw new RuntimeException(e1);
                //			}
            } catch (TokenMgrError tme) {
                Token token = new Token();
                token.beginLine = tme.errorLine;
                token.beginColumn = tme.errorColumn;
                token.endLine = tme.errorLine;
                token.endColumn = tme.errorColumn;
                addProblem(token,
                        processParserOutput(tme.getLocalizedMessage()), ERROR);
            }
        } finally {
            if (problemCollector != null) {
                problemCollector.done();
            }
        }
    }

    /**
     * @param e
     * @param newLine
     * @return
     */
    protected String processParserOutput(String msg) {
        //msg = msg.replaceAll("...","");
        msg = msg.replaceAll("<", "");
        msg = msg.replaceAll(">", "");
        return msg;
        //return msg.replaceAll(newLine,", ");
    }

    /**
     * 
     * Compiles the File 'file'. This will set addMarkers to false, because
     * there is no related IFile in the workspace avaiables.
     * 
     * @throws CoreException
     * @throws IOException
     *  
     */
    public void compile(File file) throws IOException {

        addProblems = false;
        publicModulePredicates = null;

        compile(file.getCanonicalPath(), new FileInputStream(file),
                new FileLineBreakInfoProvider(file));
    }

    public Object visit(ASTCompound node, Object data) {
        Integer arity = (Integer) pefs.get(node.getName());
        int found = node.jjtGetChild(1).jjtGetNumChildren();
        if (arity != null && arity.intValue() != found)
            addProblem(node.getToken(), "Expected arity for '" + node.getName()
                    + "' is " + arity.intValue() + ", arity found: " + found
                    + ".", WARNING);

        node.childrenAccept(this, data);
        return node;

    }

    public Object visit(ASTClause node, Object data) {
        resetSingleton();
        // TODO: dynamic, multifile
        //int lineOffset = getLineOffset(node.getToken().beginLine);
        clauses.add(node);
        //node.childrenAccept(this, data);
        checkCTValidity(node, data);
        Enumeration elems = singleton.elements();
        List problemVars = new ArrayList();
        for (; elems.hasMoreElements();) {
            List list = (List) elems.nextElement();
            if (list.size() == 1) {
                ASTVariable var = (ASTVariable) list.get(0);
                problemVars.add(var);
            }
        }
        SortedSet set = new TreeSet(new NodePositionComparator());
        set.addAll(problemVars);
        for (Iterator iter = set.iterator(); iter.hasNext();) {
            ASTVariable var = (ASTVariable) iter.next();
            addProblem(var.getToken(), MSG_SINGLETON_VAR_PREFIX + var.getName()
                    + MSG_SINGLETON_VAR_POSTFIX, WARNING);
        }

        /*
         * int endOff = getBeginOffset(node.getToken()); ASTUnitMember member =
         * getPreviousUnitMember(node); int begin = 0; if (member != null) begin =
         * getEndOffset(member.getEndToken());
         * 
         * int closing = closingComment(begin, end); int start = closing;
         * while(start != begin)
         */
        return node;
    }

    /**
     * @param node
     * @return
     */
    private ASTUnitMember getPreviousUnitMember(ASTClause node) {
        Node[] nodes = getUnit().children;
        for (int i = 0; i < nodes.length; i++) {
            if (nodes[i] == node)
                if (i > 0)
                    return (ASTUnitMember) nodes[i - 1];
                else
                    return null;
        }
        return null;
    }

    /**
     * @param node
     * @param data
     */
    private void checkCTValidity(ASTClause node, Object data) {

        if (node.getName().equals("ct")) {
            if (node.children.length != 2) {
                addProblem(node.getToken(), "expected arity of 4, was  "
                        + node.children.length + ".", WARNING);
            } else {
                ASTPredicateArgs args = node.getArgs();
                if (!(args.jjtGetChild(1) instanceof ASTParenthesis)) {
                    addProblem(((Node) args.jjtGetChild(1)).getToken(),
                            MSG_COND_PAREN, WARNING);
                    node.jjtGetChild(1);
                }
                if (!(args.jjtGetChild(2) instanceof ASTParenthesis)) {
                    addProblem(((Node) args.jjtGetChild(2)).getToken(),
                            MSG_ACTION_PAREN, WARNING);
                    node.jjtGetChild(1);
                }
                resetVars();
                args.jjtGetChild(0).jjtAccept(this, data);
                args.jjtGetChild(1).jjtAccept(this, data);
                Hashtable boundVars = vars;
                resetVars();
                args.jjtGetChild(2).jjtAccept(this, data);
                checkBoundVars(boundVars, vars);
            }
        } else {
            node.childrenAccept(this, data);
        }

    }

    /**
     * @return Returns the unit.
     */
    public ASTCompilationUnit getUnit() {
        try {
            return (ASTCompilationUnit) parser.jjtree.rootNode();
        } catch (ArrayIndexOutOfBoundsException ex) {
            //ex.printStackTrace();
        }
        return null;
    }

    public PrologElementData[] getPrologElements() {
        List preds = new ArrayList();
        for (Iterator iter = clauses.iterator(); iter.hasNext();) {
            preds.add(getPrologElementData((ASTClause) iter.next()));
        }
        return (PrologElementData[]) preds.toArray(new PrologElementData[0]);
    }

    public List getClauses() {
        return clauses;
    }

    public boolean isModule() {
        if (getUnit() == null || getUnit().children == null)
            return false;
        if (getUnit().children.length == 0)
            return false;
        return getUnit().children[0] instanceof ASTCall
                && ((ASTCall) getUnit().children[0]).isModuleDeclaration();
    }

    public String getModuleName() {
        if (isModule())
            return ((ASTCall) getUnit().children[0]).getModuleName();
        return DEFAULTMODULE;
    }

    public PrologElementData getPrologElementData(ASTClause clause) {
        int lineOffset = getLineOffset(clause.getToken().beginLine);
        PrologElementData data = new PrologElementData(clause.getName(), clause
                .getArity(), false, lineOffset + clause.getToken().beginColumn
                - 1, getLength(lineOffset, clause.getToken()), false, false);
        HashMap map = getPublicModulePredicates();
        if (map != null)
            data.setPublic(getPublicModulePredicates().containsKey(
                    data.getSignature()));
        else
            data.setPublic(true); //no module
        return data;
    }

    public PrologModule getModule() {
        return new PrologModule(getModuleName(), symbolicFileName,
                getModuleHelp(), getInterfaceElements());
    }

    /**
     * Returns the help information of the module. null, if no help exists.
     * 
     * @return
     */
    public String getModuleHelp() {
        if (!isModule())
            return null;
        return removeEnclosingQuotation(getUnit().children[0].getComment(null));
    }

    private String removeEnclosingQuotation(String s) {
        if (s != null && s.length() > 1 && s.charAt(0) == '"'
                && s.charAt(s.length() - 1) == '"')
            // removes enclosing "
            return s.substring(1, s.length() - 1);
        return s;

    }

    /**
     * @param lineOffset
     * @param token
     * @return
     */
    private int getLength(int lineOffset, Token token) {
        if (token.beginLine == token.endLine)
            return token.endColumn - token.beginColumn + 1;
        else
            return getLineOffset(token.endLine) + token.endColumn + 1
                    - lineOffset - token.beginColumn;
    }

    /**
     * @return
     */
    public ASTNamedCall[] getDynamic() {
        if (getUnit() == null)
            return new ASTNamedCall[0];
        Node[] members = getUnit().children;
        List list = new ArrayList();
        if (members != null)
            for (int i = 0; i < members.length; i++) {
                if (members[i] instanceof ASTNamedCall
                        && ((ASTNamedCall) members[i]).isDynamic())
                    list.add(members[i]);
            }
        return (ASTNamedCall[]) list.toArray(new ASTNamedCall[0]);
    }

    /**
     * @return
     */
    public PrologElementData[] getInterfaceElements() {
        PrologElementData[] elements = getPrologElements();
        HashMap map = new HashMap();
        final boolean isModule = isModule();

        for (int i = 0; i < elements.length; i++) {
            if ((!isModule || elements[i].isPublic())
                    && !map.containsKey(elements[i].getSignature()))
                map.put(elements[i].getSignature(), elements[i]);
        }
        elements = (PrologElementData[]) map.values().toArray(
                new PrologElementData[0]);
        java.util.Arrays.sort(elements, PrologElementData.getComparator());
        return elements;
    }

    protected HashMap publicModulePredicates = null;

    protected boolean addProblems = true;

    /**
     * @return
     */
    protected HashMap getPublicModulePredicates() {
        if (publicModulePredicates == null && isModule()) {
            ASTList list = (ASTList) getUnit().jjtGetChild(0).jjtGetChild(0)
                    .jjtGetChild(1).jjtGetChild(1);
            if(list.children.length % 3 != 0){
                throw new RuntimeException("LD: i replaced an assert with this exception. if you see it, something went wrong :-)");
            }
            HashMap map = new HashMap();

            for (int i = 0; i < list.children.length; i = i + 3) {
                String sig = ((ASTIdentifier) list.children[i]).getName() + "/"
                        + ((ASTAtom) list.children[i + 2]).getName();
                map.put(sig, sig);
            }
            return map;
        }
        return publicModulePredicates;
    }

    /**
     *  
     */
    private void initPEFs() {
        pefs.put("fieldDefT", new Integer(5));
        pefs.put("paramDefT", new Integer(4));
        pefs.put("localDefT", new Integer(6));
        pefs.put("methodDefT", new Integer(7));
        pefs.put("classDefT", new Integer(4));
        pefs.put("getFieldT", new Integer(6));
        pefs.put("identT", new Integer(5));
        pefs.put("literalT", new Integer(5));
        pefs.put("execT", new Integer(4));
        pefs.put("operationT", new Integer(6));
        pefs.put("applyT", new Integer(7));
        pefs.put("blockT", new Integer(4));
        pefs.put("selectT", new Integer(6));
        pefs.put("conditionalT", new Integer(6));
        pefs.put("ifT", new Integer(6));
        pefs.put("assignT", new Integer(5));
        pefs.put("importT", new Integer(3));
        pefs.put("newArrayT", new Integer(6));
        pefs.put("toplevelT", new Integer(4));
        pefs.put("newClassT", new Integer(8));
        pefs.put("returnT", new Integer(4));
        pefs.put("switchT", new Integer(5));
        pefs.put("typeCastT", new Integer(5));
        pefs.put("tryT", new Integer(6));
        pefs.put("whileLoopT", new Integer(5));
        pefs.put("continueT", new Integer(5));
        pefs.put("doLoopT", new Integer(5));
        pefs.put("indexedT", new Integer(5));
        pefs.put("throwT", new Integer(4));
        pefs.put("forLoopT", new Integer(7));
        pefs.put("synchronizedT", new Integer(5));
        pefs.put("labelT", new Integer(5));
        pefs.put("breakT", new Integer(5));
        pefs.put("typeTestT", new Integer(5));
        pefs.put("assignopT", new Integer(6));
        pefs.put("caseT", new Integer(4));
        pefs.put("catchT", new Integer(5));
        pefs.put("assertT", new Integer(5));
        pefs.put("modifierT", new Integer(2));
        pefs.put("externT", new Integer(1));
        pefs.put("interfaceT", new Integer(1));
        pefs.put("lastID", new Integer(1));
        pefs.put("packageT", new Integer(2));
        pefs.put("implementsT", new Integer(2));
        pefs.put("extendsT", new Integer(2));
        pefs.put("precedenceT", new Integer(4));
        pefs.put("nopT", new Integer(3));
    }

    public void setAddMarkers(boolean add) {
        addProblems = add;
    }

    public ProblemCollector getProblemCollector() {
        return problemCollector;
    }

    public void setProblemCollector(ProblemCollector problemCollector) {
        this.problemCollector = problemCollector;
    }

    public LineBreakInfoProvider getLineBreakInfoProvider() {
        return lineBreakInfoProvider;
    }

    public void setLineBreakInfoProvider(
            LineBreakInfoProvider lineBreakInfoProvider) {
        this.lineBreakInfoProvider = lineBreakInfoProvider;
    }
    public boolean isSingletonCheckForDTMVars() {
        return singletonCheckForDTMVars;
    }
    public void setSingletonCheckForDTMVars(boolean singletonCheckForDTMVars) {
        this.singletonCheckForDTMVars = singletonCheckForDTMVars;
    }

    /**
     * parses the file, and generates MetaData for the clauses encountered. These are generated for the
     * facts described in the file, and saved for future reference. They are loaded into the Prolog Engine.
     * @param stream TODO
     * 
     * @param this the compiler used to check the facts for consistency
     * @throws IOException TODO
     */
    
    public void saveMetaDataForClauses(OutputStream stream) throws IOException {
    	try {
    		//PrologMetaDataManager metaDataManager = new PrologMetaDataManager(client,PrologMetaDataManager.MODEL);
    		//IWorkspaceRoot root = PDTPlugin.getDefault().getWorkspace().getRoot();
    		BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(stream));
    		writer.write(":- style_check(-atom).\n");
    		String module = getModuleName();
    		List clauses = getClauses();
    		String moduleHelp = getModuleHelp();
    		if (moduleHelp == null)
    			moduleHelp = "";
    		writer.write(METADATAMODULE+ "('"+symbolicFileName+"','" + 
    				module + "', \"" + moduleHelp+"\").\n"); 
    		//HashMap publicElements = checker.getPublicModulePredicates();
    		for (Iterator iter = clauses.iterator(); iter.hasNext();) {
    			ASTClause clause = (ASTClause) iter.next();
    			PrologElementData data = getPrologElementData(clause);
    			writer.write(METADATA+"('"+symbolicFileName+"'," + module 
    					+","+ data.getLabel()
    					+","+ data.getArity()
    					+","+ data.isPublic()
    					+","+ data.getPosition()
    					+","+ data.getLength()					
    					+","+ data.isDynamic()
    					+","+ data.isMultifile()
    					+").\n");
    			String comment = clause.getComment(data.getLabel());
    			if (comment != null)
    			writer.write(METADATAHELP 
    					+"("
    					+ module
    					+"," +data.getLabel()
    					+","+ data.getArity()
    					+","+ comment
    					+").\n");
    		}
    		saveMetaDataHelpForDynamicPredicates(writer);
    		writer.close();
    		
    	} catch (IOException e1) {
    		Debug.report(e1);
    	}		
    }

    private void saveMetaDataHelpForDynamicPredicates(BufferedWriter writer) throws IOException {
    	ASTNamedCall[] dynamic = getDynamic();
    	for (int i = 0; i < dynamic.length; i++) {
    		if(dynamic[i].jjtGetNumChildren() == 1) {
    			ASTPredicateSignature sig = (ASTPredicateSignature)dynamic[i].jjtGetChild(0);
    			String name = sig.getName();
    			String help = dynamic[i].getComment(name);
    			if (help != null) { 
    				String module = sig.getModule();
    				int arity = sig.getArity();
    				writer.write(METADATAHELP+ "("+
    						module +
    						", " + name + 
    						", " + arity+
    						", " + help + ").\n");
    			}		
    		}
    		
    	}
    	
    }
}