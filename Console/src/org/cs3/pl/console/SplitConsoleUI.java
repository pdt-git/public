/*
 */
package org.cs3.pl.console;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.StyledTextContent;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.cs3.pl.common.*;

/**
 */
public class SplitConsoleUI implements ConsoleUI {

    private StyledText input;

    private StyledText output;

    private ConsoleController controller = null;

    private VerifyListener verifyListener = new VerifyListener() {

        public void verifyText(VerifyEvent e) {
            if (controller != null) {
                e.doit = controller.inputModificationIntercepted(input
                        .getText(), e.start, e.end, e.text);
            }
        }

    };

    private KeyListener keyListener= new KeyListener(){

        public void keyPressed(KeyEvent e) {
            if(controller!=null){
                controller.keyPressed(e.keyCode,e.character);
            }
        }

        public void keyReleased(KeyEvent e) {
        }

        public void keyTyped(KeyEvent e) {
            
            
        }
        
    };
    
    private VerifyKeyListener verifyKeyListener = new VerifyKeyListener() {

        public void verifyKey(VerifyEvent event) {
            if (controller != null) {
                event.doit = controller.keyStrokeIntercepted(event.keyCode,
                        event.character);
            }
        }
    };

	protected boolean thatWasMe=false;

    private ModifyListener modifyListener = new ModifyListener() {

        public void modifyText(ModifyEvent e) {
        	
        		if(controller!=null){
        			controller.inputModified(input.getText());
        		}
        	
        }
    };

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.views.ConsoleUI#appendOutput(java.lang.String)
     */
    public void appendOutput(String text) {
    	if(output==null){
    		Debug.warning("UI not available (yet?). The following output was discarded:\""+text+"\"");
    		return;
    	}
    	if(output.isDisposed()){
    		Debug.warning("UI is disposed. The following output was discarded:\""+text+"\"");
    		return;
    	}
        output.append(text);
        revealEndOfDocument();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.views.ConsoleUI#setLineBuffer(java.lang.String)
     */
    public void setLineBuffer(String text) {
    	if(output==null){
    		Debug.warning("UI not available (yet?). Could not update input to:\""+text+"\"");
    		return;
    	}
        if(! text.equals(input.getText())){
        	input.setText(text);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.views.ConsoleUI#initWidgets(org.eclipse.swt.widgets.Composite)
     */
    public void initUI(Composite parent) {
        GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns=1;
        parent.setLayout(gridLayout);
        output = new StyledText(parent, SWT.BORDER | SWT.MULTI | SWT.H_SCROLL
                | SWT.V_SCROLL | SWT.READ_ONLY);
        GridData gd = new GridData();
        gd.horizontalAlignment=GridData.FILL;
        gd.verticalAlignment=GridData.FILL;
        gd.grabExcessHorizontalSpace=true;
        gd.grabExcessVerticalSpace=true;
        output.setLayoutData(gd);
        input = new StyledText(parent, SWT.BORDER | SWT.SINGLE | SWT.H_SCROLL);
        gd = new GridData();
        gd.horizontalAlignment=GridData.FILL;
        gd.verticalAlignment=GridData.FILL;
        gd.grabExcessHorizontalSpace=true;
        gd.grabExcessVerticalSpace=false;
        input.setLayoutData(gd);
        input.addVerifyKeyListener(verifyKeyListener);
        input.addKeyListener(keyListener);
        input.addVerifyListener(verifyListener);
        input.addModifyListener(modifyListener);
        input.addTraverseListener(new TraverseListener(){

			public void keyTraversed(TraverseEvent e) {
				e.doit=false;
				
			}
        });
        
    }

    //  stolen from the eclipse wiki
    protected void revealEndOfDocument() {
    	if(output==null){
    		Debug.warning("UI unavaliable, so there's nothing to reveal...");
    		return;
    	}
        StyledTextContent doc = output.getContent();
        int docLength = doc.getCharCount();
        if (docLength > 0) {
            output.setCaretOffset(docLength);
            output.showSelection();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.views.ConsoleUI#setFocus()
     */
    public void setFocus() {
    	if(input==null){
    		Debug.warning("UI not available (yet?). Cannot set focus.");
    		return;
    	}
        input.setFocus();

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.views.ConsoleUI#setController(org.cs3.pl.views.ConsoleController)
     */
    public void setController(ConsoleController c) {
    	if (this.controller != c) {
    		this.controller = c;    		
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.views.ConsoleUI#getDisplay()
     */
    public Display getDisplay() {
    	if(input==null){
    		Debug.warning("The ui is not available ATM, thus getDisplay returns null.");
    		return null;
    	}
    	if(input.isDisposed()){
    		Debug.warning("The ui is disposed, thus getDisplay returns null.");
    		return null;
    	}
        return input.getDisplay();
    }

	/* (non-Javadoc)
	 * @see org.cs3.pl.views.ConsoleUI#getCarretPosition()
	 */
	public int getCaretPosition() {
    	if(input==null){
    		Debug.warning("The ui is not available ATM, thus getCaretPosition returns -1.");
    		return -1;
    	}
    	if(input.isDisposed()){
    		Debug.warning("The ui is disposed, thus getCaretPosition returns -1.");
    		return -1;
    	}
    	
		return input.getCaretOffset();
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.views.ConsoleUI#setCaretPosition(int)
	 */
	public void setCaretPosition(int caretPosition) {
		if(input==null){
    		Debug.warning("The ui is not available ATM, thus setCaretPosition returns");
    		return;
    	}
    	if(input.isDisposed()){
    		Debug.warning("The ui is disposed, thus setCaretPosition returns");
    		return;
    	}
    	input.setCaretOffset(caretPosition);
	}

}