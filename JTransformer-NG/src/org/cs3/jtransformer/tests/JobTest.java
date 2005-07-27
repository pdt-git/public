/*
 */
package org.cs3.jtransformer.tests;

import junit.framework.TestCase;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

/**
 */
public class JobTest extends TestCase {
 public void testJob()throws Throwable{
     Job j = new Job("JTransformer Reload Job") {

        protected IStatus run(IProgressMonitor monitor) {
            monitor.beginTask("testtask",42);
            for(int i=0;i<42;i++){
                try {
                    Thread.sleep(500);
                    System.out.println(i);
                    monitor.worked(1);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            monitor.done();
            return Status.OK_STATUS;
        }
         
     };
     j.schedule();
     j.join();
     assertTrue(true);
 }
}
