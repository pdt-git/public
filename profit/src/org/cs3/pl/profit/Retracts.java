package org.cs3.pl.profit;

import org.cs3.pl.profit.internal.FirstColumnFixture;
import org.cs3.pl.profit.internal.PrologFacade;

import fit.Parse;

public class Retracts extends FirstColumnFixture {

	@Override
	protected boolean doFirstCellInRow(Parse firstCell) throws Exception {
		return PrologFacade.retractFactOrRule(firstCell.text());
	}
}
