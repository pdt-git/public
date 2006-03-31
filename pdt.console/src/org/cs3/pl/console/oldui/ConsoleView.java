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

package org.cs3.pl.console.oldui;

import org.cs3.pl.common.Debug;
import org.cs3.pl.console.ConsoleModel;
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