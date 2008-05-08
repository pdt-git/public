package org.cs3.pdt.internal.contentassistant;

import java.util.List;
import java.util.Map;

import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.swt.graphics.Image;

public abstract class TypeAwarePrologContentAssistProcessor extends PrologContentAssistProcessor implements
IContentAssistProcessor  {

	@Override
	protected void addPredicateProposals(IDocument document, int begin,
			int len, String prefix, List<ComparableCompletionProposal> proposals, String module)
			throws PrologInterfaceException, CoreException {

		
			
		if(getProject() == null) {
			Debug.warning("Stopped completion proposal creation. No associated Prolog project found for project '" + getFile().getProject().getName() + "'.");
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
			List<Map<String, Object>> l = s.queryAll(query);

			for (Map map : l) {
				ComparableCompletionProposal p = new PredicateCompletionProposal(
						begin, len,  
							((CTerm) map.get("Name")).getFunctorValue(), 
							Integer.parseInt(((CTerm) map.get("Arity"))
										.getFunctorValue()), PLUtil
										.listAsMap((CTerm) map.get("Tags")));
				proposals.add(p);
			}
		} finally {
			if (s != null) {
				s.dispose();
			}
		}
	}

	@Override
	protected void addVariableProposals(IDocument document, int begin, int len,
			String prefix, List<ComparableCompletionProposal> proposals) throws BadLocationException, PrologInterfaceException, CoreException {

		Image image = ImageRepository.getImage(ImageRepository.PE_PUBLIC);
		
		if(getProject() == null) {
			Debug.warning("Stopped completion proposal creation. No associated Prolog project found for project '" + getFile().getProject().getName() + "'.");
			return;
		}

		PrologInterface pif = getProject().getMetadataPrologInterface();
		PrologSession s = null;
		try {
			s = (PrologSession) pif.getSession(PrologInterface.CTERMS);			
			/* pdt_completion(File,ContextName,Prefix,Attribute) */
			IFile file = getFile();
			String path = Util.prologFileName(file.getLocation().toFile());
			int offset=PDTCoreUtils.convertPhysicalToLogicalOffset(document, begin);
			String query = "trace,pdt_var_completion('" + path + "',"+offset+",'" + prefix + "',Name)";
			List<Map<String,Object>> l = s.queryAll(query);

			for (Map map : l) {
				String proposal = ((CTerm)map.get("Name")).getFunctorValue();
				int cursorPos=proposal.length();
				ComparableCompletionProposal p = new VariableCompletionProposal(
						proposal, begin, len, cursorPos, image, proposal, null, null);
				proposals.add(p);
			}
		} finally {
			if (s != null) {
				s.dispose();
			}
		}
	}

}
