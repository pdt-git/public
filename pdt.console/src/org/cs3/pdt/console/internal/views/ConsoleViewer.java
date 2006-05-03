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

package org.cs3.pdt.console.internal.views;

import org.cs3.pl.common.Debug;
import org.cs3.pl.console.CompoletionResult;
import org.cs3.pl.console.ConsoleCompletionProvider;
import org.cs3.pl.console.ConsoleHistory;
import org.cs3.pl.console.ConsoleModel;
import org.cs3.pl.console.ConsoleModelEvent;
import org.cs3.pl.console.ConsoleModelListener;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;

public class ConsoleViewer extends Viewer implements ConsoleModelListener {
	private static class _TextSelection implements ITextSelection {

		private int offset;

		private int length;

		private int startLine;

		private int endLine;

		private String text;

		public _TextSelection(StyledText control) {
			this.text = control.getSelectionText();
			this.offset = control.getSelectionRange().x;
			this.length = control.getSelectionRange().y;
			this.startLine = control.getLineAtOffset(offset);
			this.endLine = control.getLineAtOffset(offset + length);

		}

		public int getOffset() {

			return offset;
		}

		public int getLength() {
			return length;
		}

		public int getStartLine() {
			return startLine;
		}

		public int getEndLine() {
			return endLine;
		}

		public String getText() {
			return text;
		}

		public boolean isEmpty() {
			return text.length() == 0;
		}

	}

	StyledText control;

	private ConsoleModel model;

	private ConsoleCompletionProvider completionProvider;

	private ConsoleHistory history;

	private boolean thatWasMe;

	private KeyListener keyListener = new KeyListener() {
		public void keyPressed(KeyEvent e) {
			ui_keyPressed(e);
		}

		public void keyReleased(KeyEvent e) {
			;
		}
	};

	private ModifyListener modifyListener = new ModifyListener() {
		public void modifyText(ModifyEvent e) {
			ui_inputModified(ui_getLineBuffer());
		}
	};

	private VerifyKeyListener verifyKeyListener = new VerifyKeyListener() {
		public void verifyKey(VerifyEvent event) {
			ui_keyStrokeIntercepted(event);
		}
	};

	private VerifyListener verifyListener = new VerifyListener() {
		public void verifyText(VerifyEvent e) {
			ui_inputModificationIntercepted(e);
		}
	};

	private int startOfInput = 0;

	private boolean enterSendsSemicolon;

	public ConsoleViewer(Composite parent, int styles) {
		createControl(parent, styles);
	}

	private void createControl(Composite parent, int styles) {
		control = new StyledText(parent, styles);
		control.addVerifyKeyListener(verifyKeyListener);
		control.addKeyListener(keyListener);
		control.addVerifyListener(verifyListener);
		control.addModifyListener(modifyListener);

		control.addTraverseListener(new TraverseListener() {
			public void keyTraversed(TraverseEvent e) {
				e.doit = false;
			}
		});
		control.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
				;

			}

			public void widgetSelected(SelectionEvent e) {
				fireSelectionChanged(new SelectionChangedEvent(
						ConsoleViewer.this, getSelection()));
			}

		});

		control.addMouseListener(new MouseListener() {

			public void mouseUp(MouseEvent e) {
				;
			}

			public void mouseDown(MouseEvent e) {
				if (control.getSelectionCount() == 0) {
					try{
						int offset = control
							.getOffsetAtLocation(new Point(e.x, e.y));
						control.setSelection(offset);
					}
					catch(IllegalArgumentException ere){
						// TODO: what do we do when mouse position is behind the end
						// of a line?
						;
					}
				}
			}

			public void mouseDoubleClick(MouseEvent e) {
				;

			}

		});
		control.addDisposeListener(new DisposeListener() {

			public void widgetDisposed(DisposeEvent e) {
				;
			}

		});
		
		control.setEnabled(model!=null&&model.isConnected());
	}

	public Control getControl() {
		return control;
	}

	public Object getInput() {
		return model;
	}

	public ISelection getSelection() {
		return new _TextSelection(control);
	}

	public void refresh() {
		; // something to do? dunno.
	}

	public void setInput(Object input) {
		ConsoleModel m = null;
		if (input instanceof ConsoleModel) {
			m = (ConsoleModel) input;
		}
		setModel(m);
	}

	public void setSelection(ISelection selection, boolean reveal) {
		ITextSelection s = null;
		if (selection instanceof ITextSelection) {
			s = (ITextSelection) selection;
		}
		if (s == null) {
			return;
		}
		control.setSelectionRange(s.getOffset(), s.getLength());
		if (reveal) {
			control.showSelection();
		}

	}

	/**
	 * @param completionProvider
	 *            The completionProvider to set.
	 */
	public void setCompletionProvider(
			ConsoleCompletionProvider completionProvider) {
		this.completionProvider = completionProvider;
	}

	/**
	 * @param history
	 *            The history to set.
	 */
	public void setHistory(ConsoleHistory history) {
		this.history = history;
		if (history != null) {
			history.setConsoleModel(model);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleController#setModel(org.cs3.pl.views.ConsoleModel)
	 */
	public void setModel(ConsoleModel consoleModel) {
		if (model == consoleModel) {
			return;
		}
		if (model != null) {
			model.removeConsoleListener(this);
		}
		this.model = consoleModel;
		if (model != null) {
			model.addConsoleListener(this);
			ui_setSingleCharMode(model.isSingleCharMode());
			ui_setEnabled(model.isConnected());
		}
		else{
			ui_setEnabled(false);
		}

		if (history != null) {
			history.setConsoleModel(model);
		}
	}

	public static final class SavedState{
		private int startOfInput;
		private String contents;
		private ConsoleHistory history;
		private ConsoleModel model;
		public ConsoleCompletionProvider completionProvider;
		public int caretPosition;
		
	}
	
	public SavedState saveState(){
		if(control.isDisposed()){
			return null;
		}
		SavedState s = new SavedState();
		s.startOfInput=startOfInput;
		s.contents=control.getText();
		s.history=history;
		s.model=model;
		s.completionProvider=completionProvider;
		s.caretPosition=control.getCaretOffset();
		return s;
	}
	
	public void loadState(SavedState s){
		thatWasMe=true;
		setCompletionProvider(null);
		setHistory(null);
		setModel(null);
		startOfInput=0;
		control.setText("");
		
		startOfInput=s.startOfInput;
		control.setText(s.contents);
		control.setCaretOffset(s.caretPosition);
		setHistory(s.history);
		setModel(s.model);
		setCompletionProvider(s.completionProvider);
		control.showSelection();
		thatWasMe=false;
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleModelListener#onCommit(org.cs3.pl.views.ConsoleModelEvent)
	 */
	public void onCommit(final ConsoleModelEvent e) {
		Display display = control.getDisplay();
		if (Display.getCurrent() != display) {
			display.asyncExec(new Runnable() {
				public void run() {
					onCommit(e);
				}
			});
		} else {
			ui_setLineBuffer("");
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleModelListener#onEditBufferChanged(org.cs3.pl.views.ConsoleModelEvent)
	 */
	public void onEditBufferChanged(final ConsoleModelEvent e) {
		if (control == null) {
			Debug.warning("no UI, dropping EditBufferChange: "
					+ e.getNewLineState());
			return;
		}
		Display display = control.getDisplay();
		if (display == null) {
			Debug
					.warning("UI seems to be unavailable. dropping EditBufferChange: "
							+ e.getNewLineState());
			return;
		}
		if (Display.getCurrent() != display) {
			display.asyncExec(new Runnable() {
				public void run() {
					onEditBufferChanged(e);
				}
			});
		} else if (!thatWasMe) {
			String text = e.getNewLineState();
			ui_setLineBuffer(text == null ? "" : text);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleModelListener#onModeChange(org.cs3.pl.views.ConsoleModelEvent)
	 */
	public void onModeChange(final ConsoleModelEvent e) {
		Debug.debug("mode changed: " + model.isSingleCharMode());
		Display display = control.getDisplay();
		if (Display.getCurrent() != display) {
			display.asyncExec(new Runnable() {
				public void run() {
					onModeChange(e);
				}
			});
		} else {
			try {
				ui_setSingleCharMode(model.isSingleCharMode());
			} catch (Throwable t) {
				Debug.report(t);
				throw new RuntimeException(t);
			}
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleModelListener#onOutput(org.cs3.pl.views.ConsoleModelEvent)
	 */
	public void onOutput(final ConsoleModelEvent e) {
		if (control == null||control.isDisposed()) {
			return;
		}
		Display display = control.getDisplay();
		if (Display.getCurrent() != display) {
			display.asyncExec(new Runnable() {
				public void run() {
					onOutput(e);
				}
			});
		} else {
			ui_appendOutput(e.getOutput());
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.console.ConsoleModelListener#afterConnect(org.cs3.pl.console.ConsoleModelEvent)
	 */
	public void afterConnect(final ConsoleModelEvent e) {
		Display display = control.getDisplay();
		if (Display.getCurrent() != display) {
			display.asyncExec(new Runnable() {
				public void run() {
					afterConnect(e);
				}
			});
		} else {
			ui_setEnabled(true);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.console.ConsoleModelListener#beforeDisconnect(org.cs3.pl.console.ConsoleModelEvent)
	 */
	public void beforeDisconnect(final ConsoleModelEvent e) {
		Display display = control.getDisplay();
		if (Display.getCurrent() != display) {
			display.asyncExec(new Runnable() {
				public void run() {
					beforeDisconnect(e);
				}
			});
		} else {
			ui_setEnabled(false);
		}
	}

	private void completionAvailable(CompoletionResult r) {

		if (!model.getLineBuffer().equals(r.getOriginalLineContent())) {
			Debug.debug("completion discarded.");
			return;
		}
		if (control.getCaretOffset() - startOfInput != r
				.getOriginalCaretPosition()) {
			Debug.debug("completion discarded.");
			return;
		}

		String[] options = r.getOptions();
		Debug.debug("found " + options.length + " completions");
		model.setLineBuffer(r.getNewLineContent());
		control.setCaretOffset(r.getNewCaretPosition() + startOfInput);

		if (options.length > 1) {
			StringBuffer buf = new StringBuffer();
			buf.append("\n");
			for (int i = 0; i < options.length; i++) {
				buf.append(options[i]);
				buf.append("\t\t\t\t\t\t\t\t");
			}
			buf.append("\n");
			ui_appendOutput(buf.toString());
		}

	}

	private void doCompletion() {
		if (completionProvider == null) {
			return;
		}

		final int caretPosition = control.getCaretOffset() - startOfInput;
		Runnable work = new Runnable() {
			public void run() {

				final CompoletionResult r = completionProvider.doCompletion(
						model.getLineBuffer(), caretPosition);
				final Runnable notify = new Runnable() {
					public void run() {
						completionAvailable(r);
					}
				};
				control.getDisplay().asyncExec(notify);
			}
		};

		new Thread(work, "Console Completion Worker").start();

	}

	private void ui_appendOutput(String output) {
		thatWasMe = true;
		int p = 0;
		try {
			p = control.getCaretOffset() - startOfInput;
			control.replaceTextRange(startOfInput, 0, output);
			startOfInput += output.length();
			control.setCaretOffset(startOfInput + p);
			control.showSelection();
		} catch (Throwable e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
		thatWasMe = false;

	}

	private void ui_setLineBuffer(String string) {
		thatWasMe = true;
		int len = control.getContent().getCharCount() - startOfInput;
		try {
			control.replaceTextRange(startOfInput, len, string);
		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t);
		}
		control.setCaretOffset(control.getCharCount());
		control.showSelection();
		thatWasMe = false;
	}

	
	
	private void ui_setSingleCharMode(boolean b) {
		Display display = control.getDisplay();
		if (b) {
			control.setBackground(display
					.getSystemColor(SWT.COLOR_INFO_BACKGROUND));

		} else {
			control.setBackground(display
					.getSystemColor(SWT.COLOR_LIST_BACKGROUND));
		}
	}
	
	private void ui_setEnabled(boolean b){
		control.setEnabled(b);
		Display display = control.getDisplay();
		if (b) {
			control.setBackground(display
					.getSystemColor(SWT.COLOR_LIST_BACKGROUND));

		} else {
			control.setBackground(display
					.getSystemColor(SWT.COLOR_GRAY));
		}
	}

	protected void ui_inputModificationIntercepted(VerifyEvent e) {
		if (thatWasMe) {
			return;
		}
		if (model == null) {
			e.doit = false;
		} else if (e.start < startOfInput) {
			e.doit = false;
		} else {
			e.doit = true;
		}

	}

	protected void ui_keyStrokeIntercepted(VerifyEvent event) {
		int keyMask = event.stateMask;
		try {
			if (thatWasMe) {
				return;
			}
			int keyCode = event.keyCode;
			int keyChar = event.character;
			
			if ((keyCode&SWT.MODIFIER_MASK)==0&&control.getCaretOffset() < startOfInput) {
				
				control.setCaretOffset(control.getCharCount());
			}
			if (model == null) {
				event.doit = false;
				return;
			}
			if (model.isSingleCharMode()) {
				event.doit = false;

			} else {
				int off = getCaretOffset();
				switch (keyCode) {
				case SWT.HOME:
					event.doit = true;
					break;
				case SWT.KEYPAD_7:
					if (isNumLock(keyMask, keyCode, keyChar)) {
						break;
					}
					event.doit = true;
					break;
				case SWT.ARROW_LEFT:
					event.doit = off > startOfInput;
					break;
				case SWT.KEYPAD_4:
					if (isNumLock(keyMask, keyCode, keyChar)) {
						break;
					}
					event.doit = off > startOfInput;
					break;
				case SWT.CR:
				case SWT.KEYPAD_CR:
					event.doit = false;
					break;

				case SWT.ARROW_UP:
					event.doit = false;
					break;
				case SWT.KEYPAD_8:
					if (isNumLock(keyMask, keyCode, keyChar)) {
						break;
					}
					event.doit = false;
					break;

				case SWT.ARROW_DOWN:
					event.doit = false;
					break;
				case SWT.KEYPAD_2:
					if (isNumLock(keyMask, keyCode, keyChar)) {
						break;
					}
					event.doit = false;
					break;

				case ' ':
					if((keyMask&SWT.CTRL)!=0){
						doCompletion();
						event.doit = false;							
					}
					break;
				case SWT.TAB:
					doCompletion();
					event.doit = false;
					break;
				default:
					event.doit = true;
					break;
				}
			}
		} catch (Exception e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
	}

	/**
	 * 
	 * due to a known problem with swt, it is currently not possible to simply
	 * poll the numlock state.
	 * 
	 * this method tries to work around this limitation by not actualy looking
	 * at the num lock state, but testing, wether the pressed key is one of the
	 * keypad numbers and wether the resulting character is a digit.
	 */
	private boolean isNumLock(int keyMask, int keyCode, int keyChar) {

		if (keyCode >= SWT.KEYPAD_0 && keyCode <= SWT.KEYPAD_9) {
			return keyChar >= 0 && Character.isDigit((char) keyChar);
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.views.ConsoleController#inputModified(java.lang.String)
	 */
	public void ui_inputModified(String newInput) {
		if (!thatWasMe) {
			thatWasMe = true;
			model.setLineBuffer(newInput);
			thatWasMe = false;
		}
	}

	protected String ui_getLineBuffer() {
		int charCount = control.getContent().getCharCount();
		return control.getContent().getTextRange(startOfInput,
				charCount - startOfInput);
	}

	protected void ui_keyPressed(KeyEvent e) {
		char keyChar = e.character;
		int keyCode = e.keyCode;

		int keyMask = e.stateMask;
		if (thatWasMe) {
			return;
		}

		if (model == null) {
			return;
		}
		if (model.isSingleCharMode() && keyChar > 0) {
			Debug.debug("keyChar: '" + keyChar + "'");
			if(enterSendsSemicolon&&(keyCode==SWT.CR||keyCode==SWT.KEYPAD_CR)){
				model.putSingleChar(';');
			}else{
				model.putSingleChar(keyChar);
			}
		} else {
			switch (keyCode) {
			case SWT.HOME:
			case SWT.KEYPAD_7:
				if (isNumLock(keyMask, keyCode, keyChar)) {
					break;
				}
				if ((keyMask & SWT.SHIFT) != 0) {

					Point range = control.getSelectionRange();
					int to = range.x + range.y;
					int from = startOfInput;
					control.setCaretOffset(startOfInput);
					control.setSelectionRange(to, from - to);
				} else {
					control.setCaretOffset(startOfInput);
				}
				break;

			case SWT.CR:
			case SWT.KEYPAD_CR:
				model.commitLineBuffer();
				break;
			case SWT.ARROW_UP:
				Debug.debug("UP");
				history.previous();
				break;
			case SWT.KEYPAD_8:
				if (isNumLock(keyMask, keyCode, keyChar)) {
					break;
				}
				Debug.debug("UP");
				history.previous();
				break;
			case SWT.ARROW_DOWN:
				history.next();
				break;
			case SWT.KEYPAD_2:
				if (isNumLock(keyMask, keyCode, keyChar)) {
					break;
				}
				history.next();
				break;
			case SWT.TAB:
				break;
			default:
				break;
			}
		}

	}

	public ConsoleCompletionProvider getCompletionProvider() {
		return completionProvider;
	}

	public ConsoleHistory getHistory() {
		return history;
	}

	public ConsoleModel getModel() {
		return model;
	}

	public int getLineCount() {
		return control.getLineCount();
	}

	public void clearOutput() {
		thatWasMe = true;
		int c = control.getCaretOffset() - startOfInput;
		control.getContent().replaceTextRange(0, startOfInput, "");
		startOfInput = 0;
		control.setCaretOffset(c);
		thatWasMe = false;

	}

	public int getOffsetAtLine(int line) {
		return control.getOffsetAtLine(line);
	}

	public int getLineAtOffset(int offset) {
		return control.getLineAtOffset(offset);
	}

	public String getText() {
		return control.getText();
	}

	public String getTextRange(int offset, int length) {
		return control.getTextRange(offset, length);
	}

	public int getCaretOffset() {
		return control.getCaretOffset();
	}

	public void setCaretOffset(int offset){
		control.setCaretOffset(offset);
	}
	
	public void cut() {
		control.cut();

	}

	public void copy() {
		control.copy();

	}

	public void paste() {
		control.paste();

	}

	public void selectAll() {
		control.selectAll();

	}

	public void setEnterSendsSemicolon(boolean useEnter) {
		this.enterSendsSemicolon=useEnter;
		
	}

	public boolean getEnterSendsSemicolon() {
		return enterSendsSemicolon;
	}

	public int getStartOfInput() {
		return startOfInput;
	}
}
