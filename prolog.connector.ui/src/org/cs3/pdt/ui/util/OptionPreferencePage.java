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

package org.cs3.pdt.ui.util;


import org.cs3.pl.common.Option;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public abstract class OptionPreferencePage extends FieldEditorPreferencePage implements
IWorkbenchPreferencePage{

	private Option[] options;

	/** note: if you use this constructor, it is up to you to configure the PreferencePage accordingly.*/
	public OptionPreferencePage(){
		super(GRID);
	}
	
	public OptionPreferencePage(IPreferenceStore store, Option[] options, String descr){
		super(GRID);
        this.setOptions(options);
		setPreferenceStore(store);
        setDescription(descr);
	}
	
	
	
	/**
     * Creates the field editors. Field editors are abstractions of the common
     * GUI blocks needed to manipulate various types of preferences. Each field
     * editor knows how to save and restore itself.
     */
    @Override
	public void createFieldEditors() {        
        Composite parent = getFieldEditorParent();
        addEditorsForOptions(parent, getOptions());
    }

    private void addEditorsForOptions(Composite parent, final Option[] options) {
        FieldEditor editor = null;
        for(int i=0;i<options.length;i++){
            
        	if(!options[i].isVisible()){
            	continue;
            }
        	
        	String name = options[i].getId();
            String label = options[i].getLabel();
            
            final int j =i;
            switch(options[i].getType()){
            	case Option.DIR:
            	    editor = new DirectoryFieldEditor(name,label,parent){
            		@Override
					protected boolean doCheckState() {
            			String errmsg=options[j].validate(getStringValue());
            			if(errmsg==null){
            				return super.doCheckState();
            			}
            			if(errmsg.length()!=0){
            				setErrorMessage(errmsg);
            				return false;
            			}
            			return true;
            		}
            	};            	    
            	break;
            	case Option.FLAG:
            	    editor = new BooleanFieldEditor(name,label,parent);
            	    //TODO: flags are not validated right now. 
            	break;
            	case Option.NUMBER:
            	    editor = new IntegerFieldEditor(name,label,parent){
                		@Override
						protected boolean doCheckState() {
                			String errmsg=options[j].validate(getStringValue());
                			if(errmsg==null){
                				return super.doCheckState();
                			}
                			if(errmsg.length()!=0){
                				setErrorMessage(errmsg);
                				return false;
                			}
                			return true;
                		}
                	};
            	break;
            	case Option.ENUM:
            	    editor = new RadioGroupFieldEditor(name,label,4,options[i].getEnumValues(),parent,true);
            	    //TODO: enums are not validated right now. 
            	break;
            	default:
            	    editor = new StringFieldEditor(name,label,parent){
                		@Override
						protected boolean doCheckState() {
                			String errmsg=options[j].validate(getStringValue());
                			if(errmsg==null){
                				return super.doCheckState();
                			}
                			if(errmsg.length()!=0){
                				setErrorMessage(errmsg);
                				return false;
                			}
                			return true;
                		}
                	};
            		break;
            		
            }
            //disable the editor, if the value is overridden per sys prop.
            editor.setEnabled(System.getProperty(name)==null,parent);
            addField(editor);
        }
    }
    @Override
	public void init(IWorkbench workbench) {
        //setPreferenceStore(PDTPlugin.getDefault().getPreferenceStore());
    }

    /**
     * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
     */
    @Override
	protected void performDefaults() {
        super.performDefaults();
    }

    /**
     * @see org.eclipse.jface.preference.IPreferencePage#performOk()
     */
    @Override
	public boolean performOk() {

        boolean res = super.performOk();
        reconfigure();
        return res;
    }
	protected abstract void reconfigure();
	protected void setOptions(Option[] options) {
		this.options = options;
	}
	protected Option[] getOptions() {
		return options;
	} 


}
