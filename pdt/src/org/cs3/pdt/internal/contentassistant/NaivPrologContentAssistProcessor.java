package org.cs3.pdt.internal.contentassistant;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.cterm.CTermUtil;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.swt.graphics.Image;

public abstract class NaivPrologContentAssistProcessor extends PrologContentAssistProcessor implements
		IContentAssistProcessor {

	@Override
	protected void addVariableProposals(IDocument document, int begin, int len,
			String prefix, List<ComparableCompletionProposal> proposals) throws BadLocationException {

		Set<String> unique = new HashSet<String>();
		Image image = ImageRepository.getImage(ImageRepository.PE_PUBLIC);

		if (Util.isVarPrefix(prefix) || prefix.length() == 0) {
			int l = begin == 0 ? begin : begin - 1;
			String proposal = null;
			while (l > 0 && !PLEditor.predicateDelimiter(document, l)) {
				ITypedRegion region = document.getPartition(l);
				if (isComment(region))
					l = region.getOffset();
				else {
					char c = document.getChar(l);
					if (Util.isVarChar(c)) {
						if (proposal == null)
							proposal = "";
						proposal = c + proposal;
					} else if (proposal != null) {
						if (Util.isVarPrefix(proposal.charAt(0))
								&& proposal.regionMatches(true, 0, prefix, 0,
										prefix.length())
								&& !unique.contains(proposal) /*
																 * &&
																 * !proposal.equals("_")
																 */) {
							unique.add(proposal);
							int cursorPos = proposal.length();
							proposals.add(new VariableCompletionProposal(proposal,
									begin, len, cursorPos, image, proposal,
									null, null));
						}
						proposal = null;
					}
				}
				l--;
			}
		}
		if (Util.isVarPrefix(prefix) || prefix.length() == 0) {
			int l = begin == document.getLength() ? begin : begin + 1;
			String proposal = null;
			while (l < document.getLength()
					&& !PLEditor.predicateDelimiter(document, l)) {
				ITypedRegion region = document.getPartition(l);
				if (isComment(region)) {
					l = region.getOffset() + region.getLength();
				} else {
					char c = document.getChar(l);
					if (Util.isVarChar(c)) {
						if (proposal == null)
							proposal = "";
						proposal = proposal + c;
					} else if (proposal != null) {
						if (Util.isVarPrefix(proposal.charAt(0))
								&& proposal.regionMatches(true, 0, prefix, 0,
										prefix.length())
								&& !unique.contains(proposal) /*
																 * &&
																 * !proposal.equals("_")
																 */) {
							unique.add(proposal);
							int cursorPos = proposal.length();
							proposals.add(new VariableCompletionProposal(proposal,
									begin, len, cursorPos, image, proposal,
									null, null));
						}
						proposal = null;
					}
				}
				l++;
			}
		}
	}

	@Override
	protected void addPredicateProposals(IDocument document, int begin, int len,
			String prefix, List<ComparableCompletionProposal> proposals, String module)
			throws PrologInterfaceException, CoreException {

		if (Util.isVarPrefix(prefix)) {
			return;
		}
		if(getProject() == null) {
			if(PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole()!= null){
				PrologSession session =null;
				try {
					String enclFile = UIUtils.getFileFromActiveEditor();
					String moduleArg = module!=null?Util.quoteAtom(module):"Module";
					session = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole().getPrologInterface().getSession();
					String query = "find_pred('"+enclFile+"','"+prefix+"',"+moduleArg+",Name,Arity,Public,Builtin,Doc)";
					List<Map<String, Object>> predicates = session.queryAll(query);
					Debug.info("find predicates with prefix: "+ query);
					for (Map<String, Object> predicate : predicates) {
						String name = (String) predicate.get("Name");
						String strArity = (String) predicate.get("Arity");
						if(predicate.get("Module")!=null){
							module=(String) predicate.get("Module");
							if(module.equals("_")){
								module =null;
							}
						}
						
						int arity = Integer.parseInt(strArity);
						String doc = (String)predicate.get("Doc");
						Map<String, String> tags = new HashMap<String, String>();
						if(Boolean.parseBoolean((String)predicate.get("Public"))){
							tags.put("public","true");
						}
						if(Boolean.parseBoolean((String)predicate.get("Builtin"))){
							tags.put("built_in","true");
						}

						if(!doc.equals("nodoc")){
							if(doc.startsWith("%%")){
								
								doc= doc.replaceAll("%", "").trim();
							}
							
							tags.put("documentation",doc);
						}
						
						ComparableCompletionProposal p = new PredicateCompletionProposal(
														begin, len, name, arity, tags,module);
						proposals.add(p);
					}
					return;
				}catch(Exception e) {
					if(session!=null)session.dispose();
					e.printStackTrace();
				}
			} else {
				Debug.warning("Stopped completion proposal creation. No associated Prolog project found for project '" + getFile().getProject().getName() + "'.");
			}
			return;
		}

		PrologInterface pif = getProject().getMetadataPrologInterface();
		PrologSession s = null;
		try {
			s =  pif.getSession(PrologInterface.CTERMS);			
			/* pdt_completion(File,ContextName,Prefix,ModuleName:PredName/Arity,Tags) */
			IFile file = getFile();
			String path = Util.prologFileName(file.getLocation().toFile());
			String query = "pdt_completion('" + path + "',"
					+ (module != null ? "'" + module + "'" : "_") + ",'"
					+ prefix + "',Module:Name/Arity,Tags)";
			List<Map<String, Object>> answers = s.queryAll(query);

			for (Map<String, Object> anAnswer : answers) {
				String name = ((CTerm) anAnswer.get("Name")).getFunctorValue();
				String strArity = ((CTerm) anAnswer.get("Arity")).getFunctorValue();
				String resolvedModule = ((CTerm) anAnswer.get("Module")).getFunctorValue(); //TRHO TODO: not tested, yet
				int arity = Integer.parseInt(strArity);
				Map<String, CTerm> tags = CTermUtil.listAsMap((CTerm) anAnswer.get("Tags"));
				ComparableCompletionProposal p = new PredicateCompletionProposal(
												begin, len, name, arity, tags,resolvedModule );
				proposals.add(p);
			}
		} finally {
			if (s != null) {
				s.dispose();
			}
		}

	}

}
