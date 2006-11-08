/*
 * tuProlog - Copyright (C) 2001-2004  aliCE team at deis.unibo.it
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package alice.tuprologx.ide;

import alice.tuprolog.*;
import alice.tuprolog.event.OutputEvent;
import alice.tuprolog.event.SpyEvent;
import alice.tuprolog.event.WarningEvent;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * The tuProlog IDE to be run on a Java2 platform. Makes use of Thinlet and
 * Swing for advanced components, e.g. a more advanced edit area.
 * 
 * @author	<a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
 * @version	1.0 - 14-nov-02
 */

public class JavaIDE extends JFrame implements IDE {

    private ThinletTheoryEditor editor;
    private JavaEditArea editArea;
    private ThinletToolBar toolBar;
	private Prolog engine;
	
    public JavaIDE() {
        super("tuProlog IDE");
        initComponents();
    }

    /**
     * Initialize the graphic components and set the dependencies among them.
     */
    private void initComponents() {
        JPanel corePanel = new JPanel();
        corePanel.setLayout(new BorderLayout());
        
        engine = new Prolog();

        //EngineThread et = new EngineThread(engine, null);
        System.out.println("tuProlog system - release " + Prolog.getVersion());
        ThinletDebugArea debugArea = new ThinletDebugArea();
    
        ThinletStatusBar statusBar = new ThinletStatusBar();

        editor = new ThinletTheoryEditor();
        editor.setEngine(engine);
        editor.addPropertyChangeListener(statusBar);
        corePanel.add(editor, BorderLayout.NORTH);

        editArea = new JavaEditArea();
        editor.setEditArea(editArea);
        editArea.addPropertyChangeListener(editor);
        corePanel.add(editArea, BorderLayout.CENTER);
        // make visible Swing components the same background color as Thinlet(s).
        // (there is no way to get the right background color from a Thinlet yet)
        editArea.setBackground(new Color(230, 230, 230));

        ThinletConsole console = new ThinletConsole(this);
        console.setEngine(engine);
        console.addPropertyChangeListener(statusBar);
        console.setStatusMessage("Ready.");
        engine.addQueryListener(console);

        JavaInputField inputField = new JavaInputField();
        // make visible Swing components the same background color as Thinlet(s).
        // (there is no way to get the right background color from a Thinlet yet)
        inputField.setBackground(new Color(230, 230, 230));
        console.setInputField(inputField);
        JPanel queryPanel = new JPanel();
        queryPanel.setLayout(new GridBagLayout());
        GridBagConstraints inputConstraints = new GridBagConstraints();
        inputConstraints.gridx = 0;
        inputConstraints.gridy = 0;
        inputConstraints.weightx = 1;
        inputConstraints.fill = GridBagConstraints.HORIZONTAL;
        queryPanel.add(inputField, inputConstraints);
        JPanel consolePanel = new JPanel();
        consolePanel.setLayout(new BorderLayout());
        consolePanel.add(queryPanel, BorderLayout.NORTH);
        consolePanel.add(console, BorderLayout.CENTER);
        corePanel.add(consolePanel, BorderLayout.SOUTH);

        JPanel IDEPanel = new JPanel();
        IDEPanel.setLayout(new BorderLayout());
        toolBar = new ThinletToolBar(this);
        toolBar.setEngine(engine);
        IDEPanel.add(toolBar, BorderLayout.NORTH);
        IDEPanel.add(corePanel, BorderLayout.CENTER);
        toolBar.addPropertyChangeListener(statusBar);
        toolBar.setDebugArea(debugArea);
        toolBar.setFileManager(new JavaIOManager(toolBar));
        FrameLauncher frameLauncher = new SwingFrameLauncher();
        frameLauncher.setFrameIcon("img/tuProlog.gif");
        toolBar.setFrameLauncher(frameLauncher);
        IDEPanel.add(statusBar, BorderLayout.SOUTH);

        LibraryManager libraryManager = new LibraryManager();
        libraryManager.setEngine(engine);
        LibraryDialog libraryDialog = new LibraryDialog(libraryManager);
        engine.addLibraryListener(libraryDialog);
        toolBar.setLibraryDialog(libraryDialog);

        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent event) {
                System.exit(0);
            }
        });

        getContentPane().add(IDEPanel);
 
        // Set a title bar icon
        ImageIcon icon = new ImageIcon(getClass().getResource("img/tuProlog.gif"));
        setIconImage(icon.getImage());
 
		engine.addWarningListener(debugArea);
		engine.addOutputListener(console);
		engine.addSpyListener(debugArea);

 
    }

	public void onOutput(OutputEvent e) {
		System.out.print(e.getMsg());
	}
	public void onSpy(SpyEvent e) {
		System.out.println(e.getMsg());
	}
	public void onWarning(WarningEvent e) {
		System.out.println(e.getMsg());
	}

    public void enableTheoryCommands(boolean flag) {
        editor.enableTheoryCommands(flag);
        toolBar.enableTheoryCommands(flag);
    }

    public boolean isFeededTheory() {
        return !editArea.isDirty();
    }
    
    public void setFeededTheory(boolean flag) {
    	editArea.setDirty(!flag);
    }

    public String getEditorContent() {
        return editArea.getTheory();
    }
    
    public void setEditorContent(String text) {
    	editArea.setTheory(text);
    }

} // end JavaIDE class