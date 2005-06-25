package org.cs3.pl.parser.internal.term;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.StringBufferInputStream;
import java.rmi.server.ExportException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.ClauseData;
import org.cs3.pl.metadata.PrologElementData;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.parser.LineBreakInfoProvider;
import org.cs3.pl.parser.Problem;
import org.cs3.pl.parser.ProblemCollector;
import org.cs3.pl.parser.PrologCompiler;
import org.cs3.pl.parser.StringLineBreakInfoProvider;
import org.cs3.pl.parser.abba.AbbaGraphGenerator;
import org.cs3.pl.parser.abba.SimpleIDGeneratorStrategie;
import org.cs3.pl.parser.abba.WriteTermsToStreamStrategy;
import org.cs3.pl.parser.internal.classic.ASTClause;

public class TermBasedPrologCompiler implements PrologCompiler {

	private PrologTermParser parser;

	private String symbolicFileName;

	private ProblemCollector problemCollector;

	private List clauses ;

	private Set exports = new HashSet();

	private Set dynamicPredicates = new HashSet();

	private Set multifilePredicates = new HashSet();

	private Map comments;

	private String moduleComment;

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.parser.internal.classic.IPrologCompiler#compile(java.lang.String)
	 */
	public void compile(String content) {
		compile("", new StringBufferInputStream(content),
				new StringLineBreakInfoProvider(content));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.parser.internal.classic.IPrologCompiler#compile(java.lang.String,
	 *      java.io.InputStream,
	 *      org.cs3.pl.parser.internal.classic.LineBreakInfoProvider)
	 */
	public void compile(String symbolicFileName, InputStream content,
			LineBreakInfoProvider lineInfo) {
		this.symbolicFileName = symbolicFileName;

		try {
			if (problemCollector != null) {
				problemCollector.reset();
			}

			parser = new PrologTermParser(content);
			try {
				parser.CompilationUnit();
				List errors = parser.getErrors();
				for (Iterator iter = errors.iterator(); iter.hasNext();) {
					ParseException ex = (ParseException) iter.next();
					Token errorToken;
					errorToken = ex.currentToken.next;
					addProblem(errorToken, processParserOutput(ex
							.getLocalizedMessage()), Problem.ERROR);

				}
				ASTCompilationUnit root = parser.getASTRoot();
				root.setFilename(symbolicFileName);

				root.jjtAccept(
						new SingletonChecker(problemCollector), null);

				root.jjtAccept(new CtChecker(problemCollector),
						null);
				root.jjtAccept(new PEFChecker(problemCollector),
						null);
				
//				root.toCanonicalTerm(true,true).dump("");
				MemberAnalyzer memberAnalyzer = new MemberAnalyzer(
						problemCollector);
				root.jjtAccept(memberAnalyzer, null);
				this.exports = memberAnalyzer.exports;
				
				List l = (List) memberAnalyzer.properties.get("dynamic");
				if (l != null) {
					for (Iterator iter = l.iterator(); iter.hasNext();) {
						SimpleNode elm = (SimpleNode) iter.next();
						dynamicPredicates.add(elm.getImage());
					}
				}
				l = (List) memberAnalyzer.properties.get("multifile");
				if (l != null) {
					for (Iterator iter = l.iterator(); iter.hasNext();) {
						SimpleNode elm = (SimpleNode) iter.next();
						multifilePredicates.add(elm.getImage());
					}
				}
				this.comments=memberAnalyzer.comments;
				if(!"user".equals(getModuleName())){
					moduleComment=((SimpleNode)(root. children[0])).getComment();
				}
			} catch (ParseException e) {
				Token errorToken = e.currentToken.next;
				addProblem(errorToken, processParserOutput(e
						.getLocalizedMessage()), Problem.ERROR);
				Debug.report(e);

			} catch (TokenMgrError tme) {
				Token token = new Token();
				token.beginLine = tme.errorLine;
				token.beginColumn = tme.errorColumn;
				token.endLine = tme.errorLine;
				token.endColumn = tme.errorColumn;
				addProblem(token,
						processParserOutput(tme.getLocalizedMessage()),
						Problem.ERROR);
			}
		}catch(RuntimeException e){
			e.printStackTrace();
			throw e;
		}
		finally {
			if (problemCollector != null) {
				problemCollector.done();
			}
		}
	}

	protected void addProblem(Token token, String msg, int severity) {
		if (problemCollector != null) {
			Problem p = new Problem();
			p.beginOffset = token.beginOffset;
			p.endOffset = token.endOffset;
			p.firstColumn = token.beginColumn;
			p.lastColumn = token.endColumn;
			p.firstRow = token.beginLine;
			p.lastRow = token.endLine;
			p.message = msg;
			p.severity = severity;
			problemCollector.reportProblem(p);
		}

	}

	protected String processParserOutput(String msg) {
		msg = msg.replaceAll("<", "");
		msg = msg.replaceAll(">", "");
		return msg;
	}

	ClauseData createPrologElementData(ASTMember member, ASTCompilationUnit root) {

		if (!member.isDirective()) {
			String module = root.getModuleName();
			if (member.modulePrefix != null) {
				module = member.modulePrefix.toCanonicalTerm(false, true)
						.getImage();
			}
			String functor = member.getHeadLiteral().getFunctor();
			String label = member.getHeadLiteral().getLabel();
			int arity = member.getHeadLiteral().getArity();
			boolean pub = "user".equals(getModuleName())||exports.contains(functor);
			boolean dyn = dynamicPredicates
							.contains(functor);
			boolean mult = multifilePredicates
							.contains(functor);
			SourceLocation sl = TermParserUtils
							.createSourceLocation(member.getHeadLiteral(), root);
			return new ClauseData(module, label, arity,
					pub, dyn, mult, sl);

		}
		return null;
	}

	public List getClauses() {
		if (clauses == null) {
			clauses = new Vector();
			ASTCompilationUnit root = parser.getASTRoot();
			for (int i = 0; i < root.jjtGetNumChildren(); i++) {
				ClauseData c = createPrologElementData((ASTMember) root
						.jjtGetChild(i), root);
				if (c != null) {
					clauses.add(c);
				}
			}
		}
		return clauses;
	}

	public String getModuleName() {
		ASTCompilationUnit root = parser.getASTRoot();

		return root.getModuleName();
	}

	public Set getPublicModulePredicates() {

		return exports;
	}

	public ProblemCollector getProblemCollector() {
		return problemCollector;
	}

	public void setProblemCollector(ProblemCollector problemCollector) {
		this.problemCollector = problemCollector;

	}

	public void saveMetaDataForClauses(OutputStream stream) throws IOException {
    	try {
    		//PrologMetaDataManager metaDataManager = new PrologMetaDataManager(client,PrologMetaDataManager.MODEL);
    		//IWorkspaceRoot root = PDTPlugin.getDefault().getWorkspace().getRoot();
    		BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(stream));
    		writer.write(":- style_check(-atom).\n");
    		String module = getModuleName();
    		List clauses = getClauses();
    		String moduleHelp = moduleComment;
    		if (moduleHelp == null)
    			moduleHelp = "";
    		writer.write(METADATAMODULE+ "('"+symbolicFileName+"','" + 
    				module + "', \"" + moduleHelp+"\").\n"); 
    		//HashMap publicElements = checker.getPublicModulePredicates();
    		for (Iterator iter = clauses.iterator(); iter.hasNext();) {
				Clause data  = (Clause) iter.next();
    			
    			writer.write(METADATA+"('"+symbolicFileName+"'," + module 
    					+","+ data.getLabel()
    					+","+ data.getArity()
    					+","+ data.isPublic()
    					+","+ data.getPosition()
    					+","+ data.getLength()					
    					+","+ data.isDynamic()
    					+","+ data.isMultifile()
    					+").\n");
    			String comment = (String) comments.get(data.getSignature());
    			if (comment != null)
    			writer.write(METADATAHELP 
    					+"("
    					+ module
    					+"," +data.getLabel()
    					+","+ data.getArity()
    					+","+ comment
    					+").\n");
    		}
    		//TODO:saveMetaDataHelpForDynamicPredicates(writer);
    		writer.close();
    		
    	} catch (IOException e1) {
    		Debug.report(e1);
    	}		
    }

	public void saveAbbaData(OutputStream out) throws IOException {
		
		ASTCompilationUnit root = parser.getASTRoot();
		
		WriteTermsToStreamStrategy writeStrategy = new WriteTermsToStreamStrategy(new PrintStream(out));
		SimpleIDGeneratorStrategie idStrategy = new SimpleIDGeneratorStrategie();
		root.jjtAccept(
				new AbbaGraphGenerator(writeStrategy,idStrategy),null);
		out.flush();
	}


}
