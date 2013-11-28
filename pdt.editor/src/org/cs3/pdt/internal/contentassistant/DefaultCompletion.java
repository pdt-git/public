package org.cs3.pdt.internal.contentassistant;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.IDocument;

public class DefaultCompletion {
	
	private static ArrayList<DefaultCompletion> defaultCompletions;
	
	private static ArrayList<DefaultCompletion> getDefaultCompletions() {
		if (defaultCompletions == null) {
			defaultCompletions = new ArrayList<DefaultCompletion>();
			defaultCompletions.add(new DefaultCompletion(FILE_TYPE_LOGTALK, "public", ":- public(${Predicate}).\n"));
			defaultCompletions.add(new DefaultCompletion(FILE_TYPE_LOGTALK, "protected", ":- protected(${Predicate}).\n"));
			defaultCompletions.add(new DefaultCompletion(FILE_TYPE_LOGTALK, "private", ":- private(${Predicate}).\n"));
		}
		return defaultCompletions;
	}
	
	public static void addDefaultCompletions(String fileName, IDocument document, int begin, int len, String prefix, List<ComparableTemplateCompletionProposal> proposals) {
		if (fileName.endsWith(".lgt") || fileName.endsWith(".logtalk")) {
			for (DefaultCompletion com : getDefaultCompletions()) {
				if (com.canApply(fileName, prefix)) {
					proposals.add(new SimpleCompletionProposal(document, com.completion, com.key, begin, len));
				}
			}
		}
	}
	
	private static final int FILE_TYPE_PROLOG = 0;
	private static final int FILE_TYPE_LOGTALK = 1;
	
	private int fileType;
	private String key;
	private String completion;
	
	private DefaultCompletion(int fileType, String key, String completion) {
		this.fileType = fileType;
		this.key = key;
		this.completion = completion;
	}
	
	private boolean canApply(String fileName, String prefix) {
		return canApplyToFile(fileName) && canApplyToPrefix(prefix);
	}
	
	private boolean canApplyToFile(String fileName) {
		switch (fileType) {
		case FILE_TYPE_PROLOG:
			return fileName.endsWith(".pl") || fileName.endsWith(".pro") || fileName.endsWith(".prolog");
		case FILE_TYPE_LOGTALK:
			return fileName.endsWith(".lgt") || fileName.endsWith(".logtalk");
		default:
			return false;
		}
	}
	
	private boolean canApplyToPrefix(String prefix) {
		return key.startsWith(prefix);
	}

}
