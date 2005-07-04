package org.cs3.pl.console;

import java.io.IOException;

import org.cs3.pl.common.Debug;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.widgets.Composite;


public class ConsoleView  {

	public static final String CONTEXT_ACTION_EXTENSION_POINT_ID = "consoleContextMenu";
	
    private ConsoleModel model;

    private ConsoleUI ui;

    private ConsoleController controller;

    
    
    public ConsoleView() {
        setController(new DefaultConsoleController());
        setUi(new SplitConsoleUI());
        
    }
    
    /**
     * This is a callback that will allow us to create the viewer and initialize
     * it.
     */
    public void createPartControl(Composite parent) {
    	try{
    		ui.initUI(parent);
    	}catch (Throwable t){
    		Debug.report(t);
    		//XXX: eclipse eats my exceptions. this is debugging in progress.
    	}
    }

    public void setFocus() {
        ui.setFocus();
    }

    public ConsoleController getController() {
        return controller;
    }

    public void setController(ConsoleController controller) {
        if(this.controller!=controller){
        	if(this.controller!=null){
	        	this.controller.setModel(null);
	        	this.controller.setUI(null);
	        }
	    	this.controller = controller;
	        if (ui != null) {
	            ui.setController(controller);
	        }
	        if(controller!=null){
	        	controller.setModel(model);
	        	controller.setUI(ui);
	        }
        }
    }

    public ConsoleModel getModel() {
        return model;
    }

    public void setModel(ConsoleModel model) {
        if(this.model!=model){
        	this.model = model;
        	if (model != null) {
        		controller.setModel(model);
        	}
        }
    }

    public ConsoleUI getUi() {
        return ui;
    }

    public void setUi(ConsoleUI ui) {
    	if(this.ui!=ui){
    		if(this.ui!=null){
    			this.ui.setController(null);
    		}
    		this.ui = ui;
    		if (controller != null) {
    			controller.setUI(ui);
    		}
    		if(ui!=null){
    			ui.setController(controller);
    		}
    	}
    }
}