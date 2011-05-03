package org.cs3.pdt.actions;

import org.cs3.pdt.transform.internal.PrologRefactoringInfo;
import org.cs3.pl.common.Option;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;

public class ApplyTransformationInfo extends PrologRefactoringInfo{

	private final PrologInterface pif;

	public ApplyTransformationInfo(PrologInterface pif) {
		this.pif = pif;
	}

	public PrologInterface getPrologInterface() {
		return pif;
	}

	@Override
	public void configure(PrologLibraryManager libman, PrologSession s)
			throws PrologInterfaceException {
	}

	@Override
	public String getName() {
		return null;
	}

	@Override
	public String getParameterTerm() {
		return null;
	}

	@Override
	public PrologInterface getPrologInterace() {
		return null;
	}

	@Override
	public String getRefactoringId() {
		return null;
	}

	@Override
	public String getSelectionTerm() {
		return null;
	}

	@Override
	public Option[] getOptions() {
		return null;
	}

}
