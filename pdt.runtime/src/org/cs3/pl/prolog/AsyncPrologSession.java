package org.cs3.pl.prolog;

import org.cs3.pl.common.OptionProvider;


/**
 * Prolog Session for asynchronous queries.
 * 
 * An AsyncPrologSession is actually a batch processor.
 * As with the normal PrologSession, a single prolog engine thread is 
 * attached to it through its complete lifetime. This prolog engine thread is called
 * "the processor" in the remainder of this document.
 * 
 * The batch can be thought of as a FIFO queue. Clients add queries to the queue
 * using the queryAll(Object,String) and queryOnce(Object,String) methods. But unlike the
 * PrologSession, the AsyncPrologSession will not wait for any reply from the processor, but 
 * will return control to the calling thread imediatly.
 * Clients can obtain the results of the queries by registering a AsyncPrologSessionListener.
 * The processor will write all results back into the output queue. On the client side, 
 * a dedicated dispatcher thread reads and parses the incoming results an will dispatch them to
 * the registered listeners.
 *  
 * @author lukas
 *
 */
public interface AsyncPrologSession extends OptionProvider,Disposable{
	public void addBatchListener(AsyncPrologSessionListener l);
	public void removeBatchListener(AsyncPrologSessionListener l);
	
	/**
	 * Enqueue a request for all solutions to a goal.
	 * 
	 * Results, errors, etc will be reported asynchronously to registered listeners.
	 * 
	 * @param ticket An arbitrary object that will be reported back together with the results. 
	 *    clients can use this to identify results to a certain query or group of queries.
	 * @param query the query goal
	 * @see AsyncPrologSessionListener
	 */
	public void queryAll(Object ticket, String query);
	
	/**
	 * Enque a request for the first solution to a goal.
	 * 
	 * Results, errors, etc will be reported asynchronously to registered listeners.
	 *  
	 * @param ticket An arbitrary object that will be reported back together with the results. 
	 *    clients can use this to identify results to a certain query or group of queries.
	 * @param query the query goal
	 * @see AsyncPrologSessionListener
	 */
	public void queryOnce(Object ticket, String query);
	
	/**
	 * Wait for pending queries.
	 * 
	 * Blocks the calling thread until all currently pending queries have been processed.
	 * Conceptually, this works like an alarm clock: the calling thread puts a marker on 
	 * the queue and then goes to sleep. When the processor encounters the marker it will 
	 * send it back to the dispatcher, which will wake up the sleeping thread. You can use 
	 * this, if your client needs (partial) results from the enqueued queries before it can
	 * go on. Other threads may continue adding queries after the mark while this thread is 
	 * waiting. Also note that another thread may abort the batch - the 
	 * marker will be processed nevertheless.  
	 */
	public void join();
	
	/**
	 * Abort the batch.
	 *
	 * Adds a special abort marker to the queue. The processor will cut the 
	 * current query at the earliest possible time and will then skip all 
	 * enqueued queries until it reaches the abort marker. (other markers will be
	 * processed normally)
	 * During the whole process, the batch is operational and can be used as 
	 * always.
	 */
	public void abort();
	
	/**
	 * Dispose the batch.
	 * 
	 * Adds a special end_of_batch marker to the queue. No more queries may
	 * follow this marker. The processor will continue to process all encqueued queries
	 * and markers until it reaches the end_of_batch marker. It will then notify the listeners,
	 * shut down the dispatcher and close the batch. 
	 * 
	 * While the batch is disposing, it is still possible to call join() and abort(),
	 * Once the batch has been closed, this calls will have no effect.
	 */
	public void dispose();
	
	/**
	 * Check if the batch is disposed.
	 * 
	 * @return true if the batch is disposed or in the proceess of beeing disposed.
	 * @see dispose()
	 */
	public boolean isDisposed();
}
