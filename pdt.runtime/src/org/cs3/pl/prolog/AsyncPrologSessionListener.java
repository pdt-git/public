package org.cs3.pl.prolog;

public interface AsyncPrologSessionListener {

	void joinComplete(AsyncPrologSessionEvent e);

	void abortComplete(AsyncPrologSessionEvent e);

	void goalSucceeded(AsyncPrologSessionEvent e);

	void goalFailed(AsyncPrologSessionEvent e);

	void goalRaisedException(AsyncPrologSessionEvent e);

	void goalHasSolutionException(AsyncPrologSessionEvent e);

	void goalSkipped(AsyncPrologSessionEvent e);

	void goalCut(AsyncPrologSessionEvent e);

	void batchComplete(AsyncPrologSessionEvent e);

}
