

aspect ForLoop {
	
	int numThreads = 5;
	
around() within(HighPerf) &&
         for(int ?loop = ?lbound;?loop < ?limit;?loop++) ?body {
	final int range = (?limit - ?lbound) / numThreads;
	List list = new ArrayList();
	final int origLimit = ?limit;
	for(int threads = 0; threads < numThreads;threads++){
		final int finalThreads = threads;
		Thread thread = new Thread() {
			public void run(){
				int ?loop = ?lbound+range*finalThreads;
				int rangeLimit = ?loop + range;
				if(rangeLimit < origLimit)
					rangeLimit = origLimit;
				for(;?loop < rangeLimit;?loop++) 
					?body
			}
		}
		thread.run();
		list.add(thread);
	}
	for(int threads = 0; threads < list.size();threads++)
		try {
			((Thread)list.get(threads)).join();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
}
}