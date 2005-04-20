/*
 * Created on 02.02.2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pdt.internal.editors;

import java.io.File;
import java.io.FileInputStream;
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

import org.cs3.pdt.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.parser.ASTClause;
import org.cs3.pl.parser.ASTCompilationUnit;
import org.cs3.pl.parser.ASTCompound;
import org.cs3.pl.parser.ASTParenthesis;
import org.cs3.pl.parser.ASTPredicateArgs;
import org.cs3.pl.parser.ASTPredicateHead;
import org.cs3.pl.parser.ASTVariable;
import org.cs3.pl.parser.Node;
import org.cs3.pl.parser.NodePositionComparator;
import org.cs3.pl.parser.ParseException;
import org.cs3.pl.parser.PrologCompiler;
import org.cs3.pl.parser.PrologParser;
import org.cs3.pl.parser.Token;
import org.cs3.pl.parser.TokenMgrError;
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
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.texteditor.MarkerUtilities;

/**
 * @author rho
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class PrologCompilerGUIWrapper extends PrologCompiler {
	// private static String newLine = System.getProperty("line.separator");


	protected ASTCompilationUnit unit;

	public static final String MSG_COND_PAREN = "the condition part of the ct should be surrounded by parenthesis.";

	public static final String MSG_ACTION_PAREN = "the action part of the ct should be surrounded by parenthesis.";

	public static final String MSG_SINGLETON_VAR_PREFIX = "Singleton variable: ";

	public static final String MSG_SINGLETON_VAR_POSTFIX = " (one occurence only)";

	private static final String DEFAULTMODULE = "user";

	private IDocument document;

	protected static final String LINESEPARATOR = System
			.getProperty("line.separator");

	protected static final int BUFLENGTH = 1024;

	protected ArrayList markers = new ArrayList();

	protected IFile file;

	public PrologCompilerGUIWrapper() {
		super();
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
		// if (node.getName().startsWith("_"))
		// return node;
		if (vars.get(node.getName()) == null)
			vars.put(node.getName(), node);
		// if(node.getName().equals("AGet"))
		// System.out.println("DEBUG");
		if (node.getName().startsWith("_") && !false)
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
		// checkCTValidity(node, data);
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
	protected void addProblem(Token token, String msg, int severity) {
		if (addProblems) {
			int offset = getLineOffset(token.beginLine);
			HashMap attributes = new HashMap();
			MarkerUtilities.setMessage(attributes, msg);
			MarkerUtilities.setLineNumber(attributes, token.beginLine);
			int begin = offset + token.beginColumn - 1;
			if (begin < 0)
				begin = 0;
			// int add = 0;
			// if (severity == IMarker.SEVERITY_WARNING) //TODO: clean solution
			// needed
			// add =1;
			MarkerUtilities.setCharStart(attributes, begin);
			MarkerUtilities.setCharEnd(attributes, offset + token.endColumn);
			attributes.put(IMarker.SEVERITY, new Integer(severity));

			markers.add(attributes);
			// createMarker(file,attributes,IMarker.PROBLEM);
			// MarkerUtilities.createMarker(file, attributes, IMarker.PROBLEM);
			// marker.setAttribute(IMarker.CHAR_START, token.beginColumn); //
			// CHAR_START not relative to line !
			// marker.setAttribute(IMarker.CHAR_END, token.endColumn);

		}
	}

	/**
	 * @param file2
	 * @param attributes
	 * @param problem
	 * @throws CoreException
	 */
	private void createMarker() throws CoreException {
		if (markers.size() > 0) {

			IWorkspaceRunnable r = new IWorkspaceRunnable() {
				public void run(IProgressMonitor monitor) throws CoreException {
					for (int i = 0; i < markers.size(); i++) {
						IMarker marker = file.createMarker(IMarker.PROBLEM);
						marker.setAttributes((HashMap) markers.get(i));
						// MarkerAnnotation annotation = new
						// MarkerAnnotation(marker);
						// annotation.setText("ahahahaha");
					}
				}
			};
			file.getWorkspace().run(r, null, IWorkspace.AVOID_UPDATE, null);
			UIUtils.getDisplay().syncExec(new Runnable() {
				public void run() {
					try {
						UIUtils.getActivePage().showView(
								IPageLayout.ID_PROBLEM_VIEW);
						UIUtils.getActiveEditor()
								.getEditorSite().getPage().activate(
										UIUtils
												.getActiveEditor());
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
		// IEditorPart editor = PDTPlugin.getDefault().getActiveEditor();
		// Document document = (Document) ((PLEditor) editor)
		// .getDocumentProvider().getDocument(editor.getEditorInput());
		int offset = 0;
		try {
			offset = document.getLineInformation(line - 1).getOffset();
		} catch (BadLocationException e1) {
			Debug.report(e1);
		}
		return offset;
	}

	public void compile(String content) {
		if (document == null)
			document = new Document(content);
		try {
			compile(new StringBufferInputStream(content));
		} catch (CoreException e) {
			Debug.report(e);
		}
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
				getUnit().childrenAccept(this, null);
				List errors = parser.getErrors();
				for (Iterator iter = errors.iterator(); iter.hasNext();) {
					ParseException ex = (ParseException) iter.next();
					Token errorToken;
					errorToken = ex.currentToken.next;
					addProblem(errorToken, processParserOutput(ex
							.getLocalizedMessage()), IMarker.SEVERITY_ERROR);
				}
			} catch (ParseException e) {
				Debug.report(e);
			} catch (TokenMgrError tme) {
				Token token = new Token();
				token.beginLine = tme.errorLine;
				token.beginColumn = tme.errorColumn;
				token.endLine = tme.errorLine;
				token.endColumn = tme.errorColumn;
				addProblem(token,
						processParserOutput(tme.getLocalizedMessage()),
						IMarker.SEVERITY_ERROR);
			}
		} finally {
			createMarker();
		}
	}

	public void compile(IDocument document, IFile file) throws CoreException {
		this.file = file;
		this.document = document;
		compile(file.getContents());
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
		if (document == null)
			try {
				document = new Document(
						streamToString(new FileInputStream(file)));
				compile(new FileInputStream(file));
			} catch (CoreException e) {
				Debug.report(e);
			}
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
			document = new Document(streamToString(file.getContents()));
		compile(file.getContents());
	}

	/**
	 * @param file
	 * @return
	 * @throws CoreException
	 * @throws IOException
	 */
	private String streamToString(InputStream stream) throws CoreException,
			IOException {
		char[] cBuf = new char[BUFLENGTH];
		InputStreamReader reader = new InputStreamReader(stream);

		StringBuffer buf = new StringBuffer();
		int len = reader.read(cBuf);
		while (len == BUFLENGTH) {
			buf.append(cBuf, 0, len);
			len = reader.read(cBuf);
		}
		if (len >= 0)
			buf.append(cBuf, 0, len);
		return buf.toString();
	}

	public Object visit(ASTCompound node, Object data) {
		Integer arity = (Integer) pefs.get(node.getName());
		int found = node.jjtGetChild(1).jjtGetNumChildren();
		if (arity != null && arity.intValue() != found)
			addProblem(node.getToken(), "Expected arity for '" + node.getName()
					+ "' is " + arity.intValue() + ", arity found: " + found
					+ ".", IMarker.SEVERITY_WARNING);
		node.childrenAccept(this, data);
		return node;

	}

	public Object visit(ASTClause node, Object data) {
		resetSingleton();
		// TODO: dynamic, multifile
		// int lineOffset = getLineOffset(node.getToken().beginLine);
		clauses.add(node);
		// node.childrenAccept(this, data);
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
					+ MSG_SINGLETON_VAR_POSTFIX, IMarker.SEVERITY_WARNING);
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

}