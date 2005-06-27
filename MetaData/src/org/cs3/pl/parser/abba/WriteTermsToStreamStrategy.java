package org.cs3.pl.parser.abba;

import java.io.PrintStream;

public class WriteTermsToStreamStrategy implements NodeWriterStrategy {
	private final PrintStream out;

	private String prefix = "user:";

	public WriteTermsToStreamStrategy(PrintStream stream) {
		this.out = stream;
	}

	public void writeNode(String type, String nodeId, String label) {
		out
				.println(prefix + "node(" + nodeId + "," + type + "," + label
						+ ").");
	}

	public void writeProperty(String nodeId, String property, String[] values) {
		out.print(prefix + "property(" + nodeId + "," + property + "(");
		for (int i = 0; i < values.length; i++) {
			if (i > 0) {
				out.print(",");
			}
			out.print(values[i]);

		}
		out.println(")).");
	}

	public void writeEdge(String edgeId, String edgeType, String label,
			String sourceId, String targetId) {
		out.println(prefix+"edge(" + edgeId + "," + edgeType + "," + label + ","
				+ sourceId + "," + targetId + ").");

	}

}
