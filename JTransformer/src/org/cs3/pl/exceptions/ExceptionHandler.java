package org.cs3.pl.exceptions;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.cs3.pl.Debug;
import org.cs3.pl.interaction.InteractionHandler;


public class ExceptionHandler {
	
	public static void handle(final Throwable t){
		String type;
		if(InteractionHandler.askForConfirmation(t.getMessage()+"\n see stacktrace? ","An Exception occurred")){
		    StringWriter sw = new StringWriter();
		    PrintWriter writer = new PrintWriter( sw);
		    t.printStackTrace(writer);
		    InteractionHandler.tell(sw.toString(),"StackTrace");
		}		
		Debug.report(t);
/*	
		try {
			if (noPopUps)
				return;
			IWorkbenchWindow w;
			w = PDTPlugin.getDefault().getActiveWorkbenchWindow();
			if (t instanceof Error)
				type = "Error";
			else
				type = "Exception";
			final String fType = type;
			if(Display.getCurrent()==null){
				Display disp = PDTPlugin.getDefault().getDisplay();
				disp.asyncExec(new Runnable(){
					public void run(){
						MessageDialog.openError(PDTPlugin.getShell(), fType + "!", fType + " occured: " + t.toString());
					}
				});
			} else
				MessageDialog.openError(w.getShell(), type + "!", type + " occured: " + t.toString());
		} catch (NullPointerException e){
			Debug.error("Could not open graphical feedback screen");
		}
*/
	}
		
}
