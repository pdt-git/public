package org.cs3.pl.profit.internal;

import fit.Fixture;
import fit.Parse;

public abstract class FirstColumnFixture extends Fixture {

	protected abstract boolean doFirstCellInRow(Parse cell) throws Exception;

	public void doRow(Parse row) {
		Parse cell = row.parts;
		try {
			if (doFirstCellInRow(cell))
				right(cell);
			else
				wrong(cell);
		} catch (Exception e) {
			exception(cell, e);
		}
	}

}
