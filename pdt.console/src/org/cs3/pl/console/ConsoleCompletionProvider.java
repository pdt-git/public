package org.cs3.pl.console;

public interface ConsoleCompletionProvider {
	/**
	 * complete the line.
	 * 
	 * @param line
	 *            the content of the line buffer
	 * @param pos
	 *            the carret position
	 * @return the completed line. If there is no  completeion, the line
	 *         should be returned unchanged.
	 * 			If there is more than one option, the provider should
	 * 			try to do as much as possible.
	 */
	public CompoletionResult doCompletion(String line, int pos);
}