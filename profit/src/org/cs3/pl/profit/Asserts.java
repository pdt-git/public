package org.cs3.pl.profit;

import org.cs3.pl.profit.internal.FirstColumnFixture;
import org.cs3.pl.profit.internal.PrologFacade;
import org.cs3.pl.prolog.PrologInterfaceException;

import fit.Parse;

public class Asserts extends FirstColumnFixture {

	@Override
	protected boolean doFirstCellInRow(Parse firstCell) throws PrologInterfaceException {
		return PrologFacade.assertFactOrRule(firstCell.text());
	}
}
