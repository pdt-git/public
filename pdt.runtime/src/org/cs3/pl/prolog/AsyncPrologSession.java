/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pl.prolog;


/**
 * Prolog Session for asynchronous queries.
 * 
 * An AsyncPrologSession is actually a batch processor. As with the normal
 * PrologSession, a single prolog engine thread is attached to it through its
 * complete lifetime. This prolog engine thread is called "the processor" in the
 * remainder of this document.
 * 
 * The batch can be thought of as a FIFO queue. Clients add queries to the queue
 * using the queryAll(Object,String) and queryOnce(Object,String) methods. But
 * unlike the PrologSession, the AsyncPrologSession will not wait for any reply
 * from the processor, but will return control to the calling thread imediatly.
 * Clients can obtain the results of the queries by registering a
 * AsyncPrologSessionListener. The processor will write all results back into
 * the output queue. On the client side, a dedicated dispatcher thread reads and
 * parses the incoming results an will dispatch them to the registered
 * listeners.
 * 
 * @author lukas
 * 
 */
public interface AsyncPrologSession extends  Disposable {
	public void addBatchListener(AsyncPrologSessionListener l);

	public void removeBatchListener(AsyncPrologSessionListener l);

	/**
	 * Enqueue a request for all solutions to a goal.
	 * 
	 * Results, errors, etc will be reported asynchronously to registered
	 * listeners.
	 * 
	 * @param ticket
	 *            An arbitrary object that will be reported back together with
	 *            the results. clients can use this to identify results to a
	 *            certain query or group of queries.
	 * @param query
	 *            the query goal
	 * @see AsyncPrologSessionListener
	 */
	public void queryAll(Object ticket, String query) throws PrologInterfaceException;
	public void queryAll(Object ticket, String query,int flags) throws PrologInterfaceException;

	/**
	 * Enque a request for the first solution to a goal.
	 * 
	 * Results, errors, etc will be reported asynchronously to registered
	 * listeners.
	 * 
	 * @param ticket
	 *            An arbitrary object that will be reported back together with
	 *            the results. clients can use this to identify results to a
	 *            certain query or group of queries.
	 * @param query
	 *            the query goal
	 * @see AsyncPrologSessionListener
	 */
	public void queryOnce(Object ticket, String query) throws PrologInterfaceException;
	public void queryOnce(Object ticket, String query,int flags) throws PrologInterfaceException;

	/**
	 * Wait for pending queries.
	 * 
	 * Blocks the calling thread until all currently pending queries have been
	 * processed. Conceptually, this works like an alarm clock: the calling
	 * thread puts a marker on the queue and then goes to sleep. When the
	 * processor encounters the marker it will send it back to the dispatcher,
	 * which will wake up the sleeping thread. You can use this, if your client
	 * needs (partial) results from the enqueued queries before it can go on.
	 * Other threads may continue adding queries after the mark while this
	 * thread is waiting. Also note that another thread may abort the batch -
	 * the marker will be processed nevertheless.
	 */
	public void join() throws PrologInterfaceException;

	/**
	 * Abort the batch.
	 * 
	 * Adds a special abort marker to the queue and sends an abort message to
	 * the processor's message queue. The processor will cut the current query
	 * at the earliest possible time and will then skip all enqueued queries
	 * until it reaches the abort marker. (other markers will be processed
	 * normally) During the whole process, the batch is operational and can be
	 * used as always. Note that abort can be used to break out of endless
	 * loops, e.g. by cutting a a repeat/0. It does however not help in waking
	 * up blocking system calls, etc. Those have to be dealt with in an
	 * application-specific manner.
	 * 
	 */
	public void abort() throws PrologInterfaceException;

	/**
	 * Abort the batch, using a sepcific monitor as ticket.
	 * 
	 * Like abort(), but allows the caller to choose the ticket that is used
	 * with the abort marker. The session uses this ticket as monitor when
	 * waiting for the marker echo. The idea is to allow something like this:
	 * <code> 
	 * final Object lock = new Object();
	 * session.queryOnce("some_blocking_predicate");
	 * synchronized (lock) {
	 *   Thread thread = new Thread() {
	 *      public void run() {
     *         synchronized (lock) {
	 *            //some code to wake up the processor
	 * 	          //It needs to be called AFTER the abort marker was written
	 *	          //and after the abort message was send.
	 *	       }
     *	    }
	 *   };
	 *   thread.start();
	 *   session.abort(lock);
	 * }
	 * </code>
	 * 
	 * This method is mainly usefull for test cases, where it is important that
	 * things happen in a certain order. You should overuse it in your
	 * application. 
	 */
	public void abort(Object monitor) throws PrologInterfaceException;

	
	
	/**
	 * check wether a request is on queue.
	 * 
	 * 
	 * 
	 * @param ticket the ticket used with the request.
	 * @return true if at least one request was enqueued using this ticket,
	 * and this request has not yet been processed. 
	 * 
	 */
	public boolean isPending(Object ticket);
	
	/**
	 * 
	 * @return true if there are no pending requests.
	 */
	public boolean isIdle();

	/**
	 * Dispose the batch.
	 * 
	 * Adds a special end_of_batch marker to the queue. No more queries may
	 * follow this marker. The processor will continue to process all encqueued
	 * queries and markers until it reaches the end_of_batch marker. It will
	 * then notify the listeners, shut down the dispatcher and close the batch.
	 * 
	 * While the batch is disposing, it is still possible to call join() and
	 * abort(), Once the batch has been closed, this calls will have no effect.
	 * @throws PrologInterfaceException 
	 */
	public void dispose();

	/**
	 * Check if the batch is disposed.
	 * 
	 * @return true if the batch is disposed or in the proceess of beeing
	 *         disposed.
	 * @see dispose()
	 */
	public boolean isDisposed();

	/**
	 * retrieve the thread alias of the processor.
	 */
	public String getProcessorThreadAlias();
}
