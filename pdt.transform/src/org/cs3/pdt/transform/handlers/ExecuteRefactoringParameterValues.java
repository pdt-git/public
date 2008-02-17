package org.cs3.pdt.transform.handlers;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.IParameterValues;

public class ExecuteRefactoringParameterValues implements IParameterValues {

	@Override
	public Map getParameterValues() {
		HashMap<String,String> m = new HashMap<String, String>();
		m.put("Hash mich.", "fruehling");
		m.put("Hashisch.", "sommer");
		return m;
	}

}
