package org.cs3.pl.parser.abba;

import java.io.PrintStream;

public class WriteTermsToStreamStrategy implements NodeWriterStrategy {
	private final PrintStream out;

	private String prefix = "";

	public WriteTermsToStreamStrategy(PrintStream stream) {
		this.out = stream;
	}
	public WriteTermsToStreamStrategy(PrintStream stream,String prefix) {
		this.out = stream;
		this.prefix=prefix;
	}
	public void writeNode(String type, String nodeId, String label) {
		out
				.println(":- "+prefix+"abba_assert_data(node(" + nodeId + "," + type + "," + label
						+ ")).");
	}

	public void writeProperty(String nodeId, String property, String[] values) {
		out.print(":- "+prefix+"abba_assert_data(property(" + nodeId + "," + property + "(");
		for (int i = 0; i < values.length; i++) {
			if (i > 0) {
				out.print(",");
			}
			out.print(values[i]);

		}
		out.println("))).");
	}

	public void writeEdge(String edgeId, String edgeType, String label,
			String sourceId, String targetId) {
		out.println(":- "+prefix+"abba_assert_data(edge(" + edgeId + "," + edgeType + "," + label + ","
				+ sourceId + "," + targetId + ")).");

	}

	public void writeSymTabEntry(String globalSymbol, String localId) {
		out.println(":- "+prefix+"abba_put_local_symbol("+localId+", "+globalSymbol+").");		
	}

	public void writeRetractSymTab() {
		out.println(":- "+prefix+"abba_clear_local_symbols.");		
	}
	public void writeBeginCu(String file) {
		out.println(":- "+prefix+"abba_begin_cu('" +file+"').");		
	}

	public void writeAssert(String data){
		out.println(":- "+prefix+"abba_assert_data("+data+").");
	}
}
