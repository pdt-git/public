package pckg;
import java.util.ArrayList;
import java.util.List;

/*
 * Created on 16.09.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */

/**
 * @author rho
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class HighPerf {
	private int numThreads;


	public void m(){
		int limit = 10;
		for(int i = 0;i< limit;i++) {
			//  body
		}
	}


	public void m_weaved(){
		int limit = 10;
		final int range = (limit - 0) / numThreads;
		List list = new ArrayList();
		final int origLimit = limit;
		for(int threads = 0; threads < numThreads;threads++){
			final int finalThreads = threads;
			Thread thread = new Thread() {
				public void run(){
					int i = 0+range*finalThreads;
					int rangeLimit = i + range;
					if(rangeLimit < origLimit)
						rangeLimit = origLimit;
					for(;i < rangeLimit;i++) {
						// body
					}
				}
			};
			thread.run();
			list.add(thread);
		}
		for(int threads = 0; threads < list.size();threads++)
			try {
				((Thread)list.get(threads)).join();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}

	}} 
