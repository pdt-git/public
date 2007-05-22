package org.cs3.pl.profit;

import java.util.Map;

import org.cs3.pl.profit.internal.PrologFacade;
import org.cs3.pl.profit.internal.ResultMapAdapter;
import org.cs3.pl.profit.internal.ResultMapProvider;

import fit.Binding;
import fit.ColumnFixture;
import fit.Parse;

public class Queries extends ColumnFixture implements ResultMapProvider {

	private Map resultMap = null;

	public String query;

	@Override
	protected Binding createBinding(int column, Parse heads) throws Throwable {
		if (ResultMapAdapter.canHandle(heads.text()))
			return ResultMapAdapter.createResultMapBinding(this, heads.text(), this);
		return super.createBinding(column, heads);
	}

	@Override
	public void execute() throws Exception {
		resultMap = PrologFacade.queryOnce(query);
	}

	public Map getResultMap() {
		return resultMap;
	}

}
