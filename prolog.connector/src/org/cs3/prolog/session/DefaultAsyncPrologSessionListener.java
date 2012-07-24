/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.session;

import org.cs3.prolog.common.logging.Debug;

public class DefaultAsyncPrologSessionListener implements
		AsyncPrologSessionListener2 {

	@Override
	public void joinComplete(AsyncPrologSessionEvent e) {
		;
	}

	@Override
	public void abortComplete(AsyncPrologSessionEvent e) {
		;
	}

	@Override
	public void goalSucceeded(AsyncPrologSessionEvent e) {
		;
	}

	@Override
	public void goalFailed(AsyncPrologSessionEvent e) {
		Debug.info("Goal failed: "+e.query );
	}

	@Override
	public void goalRaisedException(AsyncPrologSessionEvent e) {
		Debug.error("Goal raised exception: "+e.message+"\n query: "+e.query +"\n ticket: "+e.ticket);
	}

	@Override
	public void goalHasSolution(AsyncPrologSessionEvent e) {
		;
	}

	@Override
	public void goalSkipped(AsyncPrologSessionEvent e) {
		;
	}

	@Override
	public void goalCut(AsyncPrologSessionEvent e) {
		;
	}

	@Override
	public void batchComplete(AsyncPrologSessionEvent e) {
		;
	}

	@Override
	public void batchError(AsyncPrologSessionEvent e) {
		Debug.error("Fatal error during batch processing (probably the connection to the server was lost).");
		
	}

}

