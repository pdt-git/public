/*
 * Created on 01.04.2004
 * 
 * To change the template for this generated file go to Window - Preferences -
 * Java - Code Generation - Code and Comments
 */
package org.cs3.pl.parser;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringBufferInputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.doc.PrologModule;
import org.cs3.pl.prolog.PrologElementData;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.util.Assert;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.texteditor.MarkerUtilities;
/**
 * @author xproot
 * 
 * To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Generation - Code and Comments
 */
public class PrologCompiler extends PrologParserTraversal {
	//private static String newLine = System.getProperty("line.separator");
	private Hashtable pefs = new Hashtable();
	private Hashtable vars = new Hashtable();
	private List clauses = new ArrayList();
	private IFile file;
	private Hashtable singleton = new Hashtable();
	protected ASTCompilationUnit unit;
	public static final String MSG_COND_PAREN = "the condition part of the ct should be surrounded by parenthesis.";
	public static final String MSG_ACTION_PAREN = "the action part of the ct should be surrounded by parenthesis.";
	public static final String MSG_SINGLETON_VAR_PREFIX = "Singleton variable: ";
	public static final String MSG_SINGLETON_VAR_POSTFIX = " (one occurence only)";
	private static final String DEFAULTMODULE = "user";
	private PrologParser parser;
    private IDocument document;
    private static final String LINESEPARATOR = System.getProperty("line.separator");
    private static final int BUFLENGTH = 1024;
	private ArrayList markers = new ArrayList();
	private void resetVars() {
		vars = new Hashtable();
	}
	private void resetSingleton() {
		singleton = new Hashtable();
	}
	public PrologCompiler() {
		initPEFs();
	}
	public Object visit(ASTCompilationUnit node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}
	/**
	 *  
	 */
	protected void resetProblems() throws CoreException {
		file.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
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
		if(node.getName().startsWith("_") && !PDTPlugin.getDefault().singletonCheckForDTMVars())
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
	private void checkBoundVars(Hashtable boundVars, Hashtable vars)
			throws CoreException {
		for (Iterator iter = vars.keySet().iterator(); iter.hasNext();) {
			String s = (String) iter.next();
			if (boundVars.get(s) == null)
				addProblem(((Node) vars.get(s)).getToken(), "Variable '" + s
						+ "' is not bound by the condition part.",
						IMarker.SEVERITY_WARNING);
		}
	}
	/**
	 * @param node
	 * @throws CoreException
	 */
	protected void addProblem(Token token, String msg, int severity)
			throws CoreException {
		if(addProblems) {
			int offset = getLineOffset(token.beginLine);
			HashMap attributes = new HashMap();
			MarkerUtilities.setMessage(attributes, msg);
			MarkerUtilities.setLineNumber(attributes, token.beginLine);
			int begin = offset + token.beginColumn - 1;
			if (begin < 0)
				begin = 0;
			//int add = 0;
			//if (severity == IMarker.SEVERITY_WARNING) //TODO: clean solution needed
			//	add =1;
			MarkerUtilities.setCharStart(attributes, begin);
			MarkerUtilities.setCharEnd(attributes,offset + token.endColumn);
			attributes.put(IMarker.SEVERITY, new Integer(severity));
			
			markers.add(attributes);
			//createMarker(file,attributes,IMarker.PROBLEM);
			//MarkerUtilities.createMarker(file, attributes, IMarker.PROBLEM);
			//		marker.setAttribute(IMarker.CHAR_START, token.beginColumn); //
			// CHAR_START not relative to line !
			//		marker.setAttribute(IMarker.CHAR_END, token.endColumn);
			
		}
	}
	/**
	 * @param file2
	 * @param attributes
	 * @param problem
	 * @throws CoreException
	 */
	private void createMarker() throws CoreException {
		if(markers.size()> 0) {
			
		IWorkspaceRunnable r= new IWorkspaceRunnable() {
			public void run(IProgressMonitor monitor) throws CoreException {
				for(int i =0 ;i < markers.size();i++){
				IMarker marker= file.createMarker(IMarker.PROBLEM);
				marker.setAttributes((HashMap)markers.get(i));
//				MarkerAnnotation annotation = new MarkerAnnotation(marker);
//				annotation.setText("ahahahaha");
				}
			}
		};
		file.getWorkspace().run(r, null,IWorkspace.AVOID_UPDATE, null);
		PDTPlugin.getDefault().getDisplay().syncExec(new Runnable() {
			public void run() {
				try {
					PDTPlugin.getDefault().getActivePage().showView(
							IPageLayout.ID_PROBLEM_VIEW);
					PDTPlugin.getDefault().getActiveEditor().getEditorSite()
							.getPage().activate(
									PDTPlugin.getDefault().getActiveEditor());
				} catch (PartInitException e) {
					Debug.report(e);
				}
			}
		});
		}
	}
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
		int offset = 0;
		try {
			offset = document.getLineInformation(line - 1).getOffset();
		} catch (BadLocationException e1) {
			Debug.report(e1);
		}
		return offset;
	}
	public void compile(String content) throws CoreException {
	    if (document == null)
	        document = new Document(content);
		compile(new StringBufferInputStream(content));
	}
	/**
	 * @param unit
	 */
	private void compile(InputStream content) throws CoreException {
		try {
		resetProblems();

		parser = new PrologParser(content);
		try {
			parser.CompilationUnit();
			unit = getUnit();
			//PrologCompiler checker = new PrologCompilerBackend();
			getUnit().childrenAccept(this, null);
			List errors = parser.getErrors();
			for (Iterator iter = errors.iterator(); iter.hasNext();) {
				ParseException ex = (ParseException) iter.next();
				try {
				    Token errorToken;
//				    if(ex.currentToken.next != null)
				        errorToken = ex.currentToken.next;
//				    else
//				        errorToken = ex.currentToken;
					addProblem(errorToken, processParserOutput(ex
							.getLocalizedMessage()), IMarker.SEVERITY_ERROR);
				} catch (CoreException e1) {
					throw new RuntimeException(e1);
				}
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
			addProblem(token, processParserOutput(tme.getLocalizedMessage()),
					IMarker.SEVERITY_ERROR);
		}
		}finally{
			createMarker();
		}
	}
	/**
	 * @param e
	 * @param newLine
	 * @return
	 */
	private String processParserOutput(String msg) {
		//msg = msg.replaceAll("...","");
		msg = msg.replaceAll("<", "");
		msg = msg.replaceAll(">", "");
		return msg;
		//return msg.replaceAll(newLine,", ");
	}
	
	public void compile(IDocument document,IFile file) throws CoreException {
	    this.file = file;
	    this.document = document;
	    compile(file.getContents());
	}
	
	/**
	 * @throws CoreException
	 * @throws IOException
	 *  
	 */
	public void compile(IFile file) throws CoreException, IOException {
		this.file = file;
		publicModulePredicates = null;
		if (document == null)
		    document = streamToDocument(file);
		compile(file.getContents());
	}
	
	/**
     * @param file
     * @throws CoreException
     */
    private Document streamToDocument(IFile file) throws CoreException,IOException {
        InputStreamReader reader = new InputStreamReader(file.getContents());
        char[] cBuf = new char[BUFLENGTH];
        
    		StringBuffer buf = new StringBuffer();
    		int len =reader.read(cBuf);
            while(len == BUFLENGTH){
                buf.append(cBuf,0,len);
                len = reader.read(cBuf);
            }
            if(len >= 0)
                buf.append(cBuf,0,len);
	    return new Document(buf.toString());
    }
    public Object visit(ASTCompound node, Object data) {
		Integer arity = (Integer)pefs.get(node.getName());
		int found = node.jjtGetChild(1).jjtGetNumChildren();
		if(arity != null &&
		  arity.intValue() != found)
			try {
				addProblem(node.getToken(),
						"Expected arity for '"+node.getName() + "' is " +arity.intValue() + ", arity found: " + found + ".",
						IMarker.SEVERITY_WARNING);
			} catch (CoreException e) {
				Debug.report(e);
			}
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
		try {
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
				addProblem(var.getToken(), MSG_SINGLETON_VAR_PREFIX
						+ var.getName() + MSG_SINGLETON_VAR_POSTFIX,
						IMarker.SEVERITY_WARNING);
			}
		} catch (CoreException e) {
			Debug.report(e);
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
		try {
			if (node.getName().equals("ct")) {
				if (node.children.length != 2) {
					addProblem(node.getToken(), "expected arity of 4, was  "
							+ node.children.length + ".",
							IMarker.SEVERITY_WARNING);
				} else {
					ASTPredicateArgs args = node.getArgs();
					if (!(args.jjtGetChild(1) instanceof ASTParenthesis)) {
						addProblem(((Node) args.jjtGetChild(1)).getToken(),
								MSG_COND_PAREN, IMarker.SEVERITY_WARNING);
						node.jjtGetChild(1);
					}
					if (!(args.jjtGetChild(2) instanceof ASTParenthesis)) {
						addProblem(((Node) args.jjtGetChild(2)).getToken(),
								MSG_ACTION_PAREN, IMarker.SEVERITY_WARNING);
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
			} else
				node.childrenAccept(this, data);
		} catch (CoreException e) {
			Debug.report(e);
		}
	}
	/**
	 * @return Returns the unit.
	 */
	public ASTCompilationUnit getUnit() {
	    try {
	        return (ASTCompilationUnit) parser.jjtree.rootNode();
	    } catch(ArrayIndexOutOfBoundsException ex) {
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
		if(getUnit() == null || getUnit().children == null)
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
		PrologElementData data = new PrologElementData(clause.getName(), clause.getArity(),
				false,
				lineOffset + clause.getToken().beginColumn - 1, getLength(
						lineOffset, clause.getToken()), false, false);
		HashMap map = getPublicModulePredicates();
		if(map != null)
			data.setPublic(getPublicModulePredicates().containsKey(data.getSignature()));
		else
			data.setPublic(true); //no module
		return data;
	}
	
	public PrologModule getModule() {
		return new PrologModule(getModuleName(),file,getModuleHelp(), getInterfaceElements());
	}

	/**
	 * Returns the help information of the module.
	 * null, if no help exists.
	 * 
	 * @return 
	 */
	public String getModuleHelp() {
		if (!isModule())
			return null;
		return removeEnclosingQuotation(getUnit().children[0].getComment(null));
	}
	
	private String removeEnclosingQuotation(String s) {
		if (s != null && s.length() > 1
				&& s.charAt(0) == '"'
				&& s.charAt(s.length()-1) == '"')
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
	    if(getUnit() == null)
	        return new ASTNamedCall[0];
		Node[] members = getUnit().children;
		List list = new ArrayList();
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
			if( (!isModule || elements[i].isPublic()) && 
				!map.containsKey(elements[i].getSignature()))
			map.put(elements[i].getSignature(), elements[i]);
		}
		elements = (PrologElementData[])map.values().toArray(
				new PrologElementData[0]);
		java.util.Arrays.sort(elements, PrologElementData.getComparator());
		return elements;
	}

	
	private HashMap publicModulePredicates = null;
	private boolean addProblems = true;
	/**
	 * @return
	 */
	private HashMap getPublicModulePredicates() {
		if (publicModulePredicates == null && isModule()) {
			ASTList list = (ASTList)getUnit().jjtGetChild(0).jjtGetChild(0).jjtGetChild(1).jjtGetChild(1);
			Assert.isTrue(list.children.length % 3 == 0);
			HashMap map = new HashMap();
			
			for(int i = 0; i < list.children.length; i = i+3) {
				String sig = ((ASTIdentifier)list.children[i]).getName() + "/" + ((ASTAtom)list.children[i+2]).getName();
				map.put(sig,sig);
			}
			return map;
		}
		return publicModulePredicates;
	}
	/**
	 *  
	 */
	private void initPEFs() {
		pefs.put("fieldDefT",new Integer(5));
		pefs.put("paramDefT",new Integer(4));
		pefs.put("localDefT",new Integer(6));
		pefs.put("methodDefT",new Integer(7));
		pefs.put("classDefT",new Integer(4));
		pefs.put("getFieldT",new Integer(6));
		pefs.put("identT",new Integer(5));
		pefs.put("literalT",new Integer(5));
		pefs.put("execT",new Integer(4));
		pefs.put("operationT",new Integer(6));
		pefs.put("applyT",new Integer(7));
		pefs.put("blockT",new Integer(4));
		pefs.put("selectT",new Integer(6));
		pefs.put("conditionalT",new Integer(6));
		pefs.put("ifT",new Integer(6));
		pefs.put("assignT",new Integer(5));
		pefs.put("importT",new Integer(3));
		pefs.put("newArrayT",new Integer(6));
		pefs.put("toplevelT",new Integer(4));
		pefs.put("newClassT",new Integer(8));
		pefs.put("returnT",new Integer(4));
		pefs.put("switchT",new Integer(5));
		pefs.put("typeCastT",new Integer(5));
		pefs.put("tryT",new Integer(6));
		pefs.put("whileLoopT",new Integer(5));
		pefs.put("continueT",new Integer(5));
		pefs.put("doLoopT",new Integer(5));
		pefs.put("indexedT",new Integer(5));
		pefs.put("throwT",new Integer(4));
		pefs.put("forLoopT",new Integer(7));
		pefs.put("synchronizedT",new Integer(5));
		pefs.put("labelT",new Integer(5));
		pefs.put("breakT",new Integer(5));
		pefs.put("typeTestT",new Integer(5));
		pefs.put("assignopT",new Integer(6));
		pefs.put("caseT",new Integer(4));
		pefs.put("catchT",new Integer(5));
		pefs.put("assertT",new Integer(5));
		pefs.put("modifierT",new Integer(2));
		pefs.put("externT",new Integer(1));
		pefs.put("interfaceT",new Integer(1));
		pefs.put("lastID",new Integer(1));
		pefs.put("packageT",new Integer(2));
		pefs.put("implementsT",new Integer(2));
		pefs.put("extendsT",new Integer(2));
		pefs.put("precedenceT",new Integer(4));
		pefs.put("nopT",new Integer(3));
	}
	
	public void addProblems(boolean add) {
		addProblems = add;
	}
}