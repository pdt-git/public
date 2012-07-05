package org.cs3.pdt.internal.contentassistant;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.swt.graphics.Image;

public abstract class NaivPrologContentAssistProcessor extends PrologContentAssistProcessor implements IContentAssistProcessor {

	@Override
	protected void addVariableProposals(IDocument document, int begin, int len, String prefix, List<ComparableCompletionProposal> proposals) throws BadLocationException {

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
						if (Util.isVarPrefix(proposal.charAt(0)) && proposal.regionMatches(true, 0, prefix, 0, prefix.length()) && !unique.contains(proposal) /*
																																							 * &&
																																							 * !
																																							 * proposal
																																							 * .
																																							 * equals
																																							 * (
																																							 * "_"
																																							 * )
																																							 */) {
							unique.add(proposal);
							int cursorPos = proposal.length();
							proposals.add(new VariableCompletionProposal(proposal, begin, len, cursorPos, image, proposal, null, null));
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
			while (l < document.getLength() && !PLEditor.predicateDelimiter(document, l)) {
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
						if (Util.isVarPrefix(proposal.charAt(0)) && proposal.regionMatches(true, 0, prefix, 0, prefix.length()) && !unique.contains(proposal) /*
																																							 * &&
																																							 * !
																																							 * proposal
																																							 * .
																																							 * equals
																																							 * (
																																							 * "_"
																																							 * )
																																							 */) {
							unique.add(proposal);
							int cursorPos = proposal.length();
							proposals.add(new VariableCompletionProposal(proposal, begin, len, cursorPos, image, proposal, null, null));
						}
						proposal = null;
					}
				}
				l++;
			}
		}
	}

	@Override
	protected void addPredicateProposals(IDocument document, int begin, int len, String prefix, List<ComparableCompletionProposal> proposals, String module) throws PrologInterfaceException,
			CoreException {

		if (Util.isVarPrefix(prefix)) {
			return;
		}
		if (PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole() != null) {
			PrologSession session = null;
			try {
				String enclFile = UIUtils.getFileFromActiveEditor();
				String moduleArg = module != null ? Util.quoteAtom(module) : "Module";
				session = PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().getActivePrologInterface().getSession();
				String query = "pdt_search:find_pred_for_editor_completion('" + enclFile + "','" + prefix + "'," + moduleArg + ",Name,Arity,Public,Builtin,Doc,Kind)";
				List<Map<String, Object>> predicates = session.queryAll(query);
				Debug.info("find predicates with prefix: " + query);
				for (Map<String, Object> predicate : predicates) {
					String name = (String) predicate.get("Name");
					String strArity = (String) predicate.get("Arity");
					String kind = predicate.get("Kind").toString();
					if (predicate.get("Module") != null) {
						module = (String) predicate.get("Module");
						if (module.equals("_")) {
							module = null;
						}
					}

					int arity = Integer.parseInt(strArity);
					String doc = (String) predicate.get("Doc");
					Map<String, String> tags = new HashMap<String, String>();
					if (Boolean.parseBoolean((String) predicate.get("Public"))) {
						tags.put("public", "true");
					}
					if (Boolean.parseBoolean((String) predicate.get("Builtin"))) {
						tags.put("built_in", "true");
					}

					if (!doc.equals("nodoc")) {
						if (doc.startsWith("%%")) {

							doc = doc.replaceAll("%", "").trim();
						}

						tags.put("documentation", doc);
					}

					ComparableCompletionProposal p = new PredicateCompletionProposal(document, begin, len, name, arity, tags, module, kind);
					proposals.add(p);
				}
				return;
			} catch (Exception e) {
				Debug.report(e);
			} finally {
				if (session != null)
					session.dispose();
			}
		} else {
			Debug.warning("Stopped completion proposal creation. No associated Prolog project found for project '" + getFile().getProject().getName() + "'.");
		}
		return;

	}

}
