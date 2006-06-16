package org.cs3.jtransformer.internal.actions;

import java.util.Set;

public class AddingFactsWriter extends AbstractStringBufferWriter {

	private char clauseSeparator;

	public AddingFactsWriter(Set filter, char clauseSeparator) {
		super(filter, clauseSeparator);
		this.clauseSeparator = clauseSeparator;
	}

	public void writeRule(String string, String[] param, String[] condi) {
		StringBuffer buf = new StringBuffer();

		if (indent)
			for (int i = 0; i < indention; i++) {
				buf.append('\t');
			}

		buf.append("add(");
		buf.append(string);
		buf.append("(");

		if (param != null && param.length > 0) {
			for (int i = 0; i < param.length - 1; i++) {
				buf.append(param[i]);
				buf.append(", ");
			}

			buf.append(param[param.length - 1]);
		}

		buf.append(")");

		if (condi != null && condi.length > 0) {
			buf.append(" :- ");

			for (int i = 0; i < condi.length - 1; i++) {
				buf.append(condi[i]);
				buf.append(", ");
			}

			buf.append(condi[condi.length - 1]);
		}

		buf.append(")");
		buf.append(clauseSeparator + "\n");

		this.buf.append(buf.toString());
	}
	
}
