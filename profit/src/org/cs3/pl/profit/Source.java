package org.cs3.pl.profit;

import java.io.File;
import java.io.FileWriter;

import org.cs3.pl.profit.internal.FirstColumnFixture;
import org.cs3.pl.profit.internal.PrologFacade;

import fit.Parse;

public class Source extends FirstColumnFixture {

	private static final String CODE_TAG = "pre";

	private static final String CODE_END_TAG = "</" + CODE_TAG + ">";

	private static final String CODE_START_TAG = "<" + CODE_TAG + ">";

	private static final String INCLUDE_START_TAG = "<div class=\"included\">";

	@Override
	protected boolean doFirstCellInRow(Parse firstCell) throws Exception {
		String html = firstCell.body;
		if (!html.startsWith(INCLUDE_START_TAG)) {
			return PrologFacade.consult(new File(firstCell.text()));
		}
		boolean result = false;
		File file = File.createTempFile("profit", ".pl");
		try {
			FileWriter writer = new FileWriter(file);
			writer.write(extractPreformatedSections(html));
			writer.close();
			result = PrologFacade.consult(file);
		} finally {
			file.delete();
		}
		return result;
	}

	private String extractPreformatedSections(String html) {
		StringBuffer source = new StringBuffer(html);
		StringBuffer result = new StringBuffer();
		int start = source.indexOf(CODE_START_TAG);
		int end = source.indexOf(CODE_END_TAG, start);
		while (start != -1) {
			if (end == -1)
				end = source.length();
			result.append(source, start + CODE_START_TAG.length(), end);
			start = source.indexOf(CODE_START_TAG, end);
			end = source.indexOf(CODE_END_TAG, start);
		}
		return result.toString();
	}
}
