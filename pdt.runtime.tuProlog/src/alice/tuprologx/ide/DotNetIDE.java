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

import java.awt.*;
import java.awt.event.*;

/**
 * The tuProlog IDE to be run on a .NET (i.e. Java 1.1) platform. Makes use of
 * both Thinlet and AWT, since Java 1.1 does not feature Swing components.
 * 
 * @author	<a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
 * @version	1.0 - 14-nov-02
 */

public class DotNetIDE extends Frame implements IDE {

    private ThinletTheoryEditor editor;
    private DotNetEditArea editArea;
    private ThinletToolBar toolBar;

    public DotNetIDE() {
        super("tuProlog IDE");
        initComponents();
    }

    /**
     * Initialize the graphic components and set the dependencies among them.
     */
    private void initComponents() {
        Panel corePanel = new Panel();
        corePanel.setLayout(new BorderLayout());
		Prolog engine = new Prolog();
        System.out.println("tuProlog system - release " + Prolog.getVersion());
        ThinletDebugArea debugArea = new ThinletDebugArea();
        engine.addSpyListener(debugArea);

        ThinletStatusBar statusBar = new ThinletStatusBar();

		editor = new ThinletTheoryEditor();
		//editor.setEngine(engine);
        editor.addPropertyChangeListener(statusBar);
		corePanel.add(editor, BorderLayout.NORTH);

        editArea = new DotNetEditArea();
        editor.setEditArea(editArea);
        editArea.addPropertyChangeListener(editor);
        // make visible AWT components the same background color as Thinlet(s).
        // (there is no way to get the right background color from a Thinlet yet)
        editArea.setBackground(new Color(230, 230, 230));
        corePanel.add(editArea, BorderLayout.CENTER);

		ThinletConsole console = new ThinletConsole(this);
		//console.setEngine(engine);
        console.addPropertyChangeListener(statusBar);
        console.setStatusMessage("Ready.");

        DotNetInputField inputField = new DotNetInputField();
        console.setInputField(inputField);
        // make visible AWT components the same background color as Thinlet(s).
        // (there is no way to get the right background color from a Thinlet yet)
        inputField.setBackground(new Color(230, 230, 230));
        Panel queryPanel = new Panel();
        queryPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 1;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        queryPanel.add(inputField, constraints);
        Panel consolePanel = new Panel();
        consolePanel.setLayout(new BorderLayout());
        consolePanel.add(queryPanel, BorderLayout.NORTH);
        consolePanel.add(console, BorderLayout.CENTER);

		corePanel.add(consolePanel, BorderLayout.SOUTH);

        Panel IDEPanel = new Panel();
        IDEPanel.setLayout(new BorderLayout());
        toolBar = new ThinletToolBar(this);
        //toolBar.setEngine(engine);
        IDEPanel.add(toolBar, BorderLayout.NORTH);
        IDEPanel.add(corePanel, BorderLayout.CENTER);
        toolBar.addPropertyChangeListener(statusBar);
        toolBar.setDebugArea(debugArea);
        toolBar.setFileManager(new DotNetIOManager(this));
        FrameLauncher frameLauncher = new AWTFrameLauncher();
        frameLauncher.setFrameIcon("tuProlog.gif");
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

        add(IDEPanel);

        // Set a title bar icon
        setIconImage(Toolkit.getDefaultToolkit().getImage(getClass().getResource("tuProlog.gif")));
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

} // end DotNetIDE class