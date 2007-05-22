package org.cs3.pl.profit;

import java.util.Map;

import org.cs3.pl.profit.internal.PrologFacade;
import org.cs3.pl.profit.internal.ResultMapAdapter;

import fit.Binding;
import fit.Parse;
import fit.RowFixture;

public class Query extends RowFixture {

	@Override
	public Class getTargetClass() {
		return Map.class;
	}

	@Override
	public Object[] query() throws Exception {
		return PrologFacade.queryAll(getArgs()[0]);
	}

	@Override
	protected Binding createBinding(int column, Parse heads) throws Throwable {
		if (ResultMapAdapter.canHandle(heads.text()))
			return ResultMapAdapter.createResultMapBinding(this, heads.text(), null);
		return super.createBinding(column, heads);
	}

}
