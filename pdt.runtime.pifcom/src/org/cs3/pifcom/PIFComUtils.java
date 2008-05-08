package org.cs3.pifcom;

import java.util.Vector;

import org.cs3.pifcom.codec.CTermMessage;
import org.cs3.pl.cterm.CAtom;
import org.cs3.pl.cterm.CString;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface;

public class PIFComUtils {
	public static Object processBinding(CTermMessage m, int flags) {
		CTerm term = m.getCTermValue();
		if ((PrologInterface.CTERMS & flags)>0) {
			return term;
		}
		if ((PrologInterface.PROCESS_LISTS&flags)>0) {
			return interpreteLists_deep(term,flags);
		}
		if((PrologInterface.UNQUOTE_ATOMS&flags )>0) {
			if(term instanceof CString||term instanceof CAtom){
				return term.getFunctorValue();
			}
		}
		return m.getStringValue();

	}

	public static Object interpreteLists_deep(CTerm term, int flags) {
		if (PLUtil.isList(term)) {

			Vector<CTerm> terms = PLUtil.listAsVector(term);

			Vector result = new Vector();
			for (CTerm t : terms) {
				result.add(interpreteLists_deep(t,flags));
			}
			return result;
		}else if((PrologInterface.UNQUOTE_ATOMS&flags )>0 &&
				(term instanceof CString||term instanceof CAtom)){
			return term.getFunctorValue();
		} 
		else{
			return PLUtil.renderTerm(term);
		}

	}
}
