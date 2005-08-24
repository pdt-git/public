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
					int offset = control
							.getOffsetAtLocation(new Point(e.x, e.y));
					control.setSelection(offset);
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
		}

		if (history != null) {
			history.setConsoleModel(model);
		}
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
		if (control == null) {
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
	public void afterConnect(ConsoleModelEvent e) {
		;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.console.ConsoleModelListener#beforeDisconnect(org.cs3.pl.console.ConsoleModelEvent)
	 */
	public void beforeDisconnect(ConsoleModelEvent e) {
		;
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
		try {
			if (thatWasMe) {
				return;
			}
			if (control.getCaretOffset() < startOfInput) {
				control.setCaretOffset(control.getCharCount());
			}
			int keyCode = event.keyCode;
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
				case SWT.KEYPAD_7:
					event.doit=true;
					break;
				case SWT.ARROW_LEFT:
				case SWT.KEYPAD_4:
					event.doit=off>startOfInput;
					break;
				case SWT.CR:
				case SWT.KEYPAD_CR:
					event.doit = false;
					break;

				case SWT.ARROW_UP:
				case SWT.KEYPAD_8:
					event.doit = false;
					break;

				case SWT.ARROW_DOWN:
				case SWT.KEYPAD_2:
					event.doit = false;
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
		int keyMask=e.stateMask;
		if (thatWasMe) {
			return;
		}

		if (model == null) {
			return;
		}
		if (model.isSingleCharMode() && keyChar > 0) {
			Debug.debug("keyChar: '" + keyChar + "'");
			model.putSingleChar(keyChar);
		} else {
			switch (keyCode) {
			case SWT.HOME:
			case SWT.KEYPAD_7:
				if((keyMask&SWT.SHIFT)!=0){
					 
					Point range = control.getSelectionRange();
					int to = range.x + range.y;
					int from = startOfInput;
					control.setCaretOffset(startOfInput);
					control.setSelectionRange(to,from-to);
				}else {
					control.setCaretOffset(startOfInput);
				}
				break;
			
			case SWT.CR:
			case SWT.KEYPAD_CR:
				model.commitLineBuffer();
				break;
			case SWT.ARROW_UP:
			case SWT.KEYPAD_8:
				Debug.debug("UP");
				history.previous();
				break;
			case SWT.ARROW_DOWN:
			case SWT.KEYPAD_2:
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
}
