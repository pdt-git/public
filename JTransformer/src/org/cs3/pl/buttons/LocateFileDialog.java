/*
 * Created on 05.10.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.buttons;

import java.io.FileNotFoundException;

import org.cs3.pl.PDTPlugin;
import org.eclipse.swt.widgets.FileDialog;

/**
 * @author windeln
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class LocateFileDialog {
    
	FileDialog fileDialog = null;
	
	/**
	 * Creates a FileDialog
	 */
	
    public LocateFileDialog(String filename,String title){
		fileDialog = new FileDialog(PDTPlugin.getShell(),0);
		
		//String projectName  = currentSelection.getElementName();
		String filterPath;
		try {
			filterPath = PDTPlugin.getDefault().getLocation();
		} catch (FileNotFoundException e2) {
			filterPath = PDTPlugin.getDefault().getStateLocation().toOSString();
		}
		setFilterPath(filterPath /*"src"+File.separator+"test"+File.separator+"factbases"+File.separator*/);
		setFileName(filename);
		setFilterExtensions(new String[] {".pl"});
		setText(title);
		
		
    }
    
    /**
     * @see FileDialog#setFilterPath(java.lang.String)
     */
    public void setFilterPath(String filterPath) {
        fileDialog.setFilterPath(filterPath);
    }

    /**
     * @see FileDialog#setFileName(java.lang.String)
     */
    public void setFileName(String name){
		fileDialog.setFileName(name);
    }

    /**
     * @see FileDialog#setFilterExtensions(java.lang.String[])
     */
    public void setFilterExtensions(String[] extensions){
        fileDialog.setFilterExtensions(extensions);    
    }

    /**
     * @see FileDialog#setText(java.lang.String)
     */
    public void setText(String text){
		fileDialog.setText(text);
    }

    /**
     * opens the File dialog and returns the path to thge selected file.
     * @return
     */
    public String openDialog(){
		return fileDialog.open();

    }
}
