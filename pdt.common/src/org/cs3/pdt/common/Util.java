package org.cs3.pdt.common;

public class Util {
	
	private static final String SPAN_HIDDEN = "<span style={display:none}>";
	
	public static String[] getPredicateArgNamesFromDocumentation(String doc) {
		String[] names = null;
		if (doc != null) {
			if(doc.startsWith("<dl>\n<dt")){
				if (doc.indexOf("arglist") > 0 && doc.indexOf("</var>") > doc.indexOf("arglist")) {
					String commaSeparatedArgs = doc.substring(doc.indexOf("arglist") + 10, doc.indexOf("</var>") -1);
					commaSeparatedArgs = commaSeparatedArgs.replaceAll("\\?", "");
					commaSeparatedArgs = commaSeparatedArgs.replaceAll("\\-", "");
					commaSeparatedArgs = commaSeparatedArgs.replaceAll("\\+", "");
					names = commaSeparatedArgs.split(",");
					for (int i = 0; i < names.length; i++) {
						int typeSeparator = names[i].indexOf(':');
						if (typeSeparator >= 0) {
							names[i] = names[i].substring(0, typeSeparator);
						}
					}
				}
			} else if (doc.indexOf(SPAN_HIDDEN) > 0 && doc.indexOf("</span>") > 0) {
				String head = doc.substring(doc.indexOf(SPAN_HIDDEN) + SPAN_HIDDEN.length(), doc.indexOf("</span>"));
				int indexOfOpeningBracket = head.indexOf("(");
				if (indexOfOpeningBracket != -1) {
					names = head.substring(indexOfOpeningBracket + 1, head.lastIndexOf(")")).split(",");
				}
			}
		}
		return names;
	}

}
