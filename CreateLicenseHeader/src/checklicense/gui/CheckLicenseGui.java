/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Fabian Noth
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package checklicense.gui;

import java.awt.BorderLayout;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.UIManager;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import checklicense.CheckLicense;
import checklicense.utils.Utils;
import javax.swing.JComboBox;
import javax.swing.DefaultComboBoxModel;

import static checklicense.CheckLicense.NO_AUTHOR;
import static checklicense.CheckLicense.NO_HEADER;;

public class CheckLicenseGui extends JFrame {

	private static final String BORDER_TITLE = "Files";
	private static final long serialVersionUID = 1L;
	private static final File LAST_PATH_FILE = new File("data/last_path.txt");
	private JPanel contentPane;
	private JTextField textField;
	private CheckLicense check;
	private JList<String> list;
	private JTextArea textArea;
	private JTextField tfAuthor;
	private TitledBorder border;
	private JComboBox<String> comboBox;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			@Override public void run() {
				try {
					CheckLicenseGui frame = new CheckLicenseGui();
					frame.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the frame.
	 */
	public CheckLicenseGui() {
		check = new CheckLicense();
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		setTitle("Check License Header");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setBounds(100, 100, 450, 300);
		contentPane = new JPanel();
		contentPane.setBorder(new EmptyBorder(5, 5, 5, 5));
		contentPane.setLayout(new BorderLayout(0, 5));
		setContentPane(contentPane);
		
		JPanel panel = new JPanel();
		contentPane.add(panel, BorderLayout.NORTH);
		panel.setLayout(new BorderLayout(2, 2));
		
		JLabel lblNewLabel = new JLabel("Path to repository:");
		panel.add(lblNewLabel, BorderLayout.WEST);
		
		textField = new JTextField();
		textField.addKeyListener(new KeyAdapter() {
			@Override public void keyReleased(KeyEvent evt) {
				if(evt.getKeyCode() == KeyEvent.VK_ENTER) {
					listFiles();
				}
			}
		});
		textField.setText(Utils.readFileToString(LAST_PATH_FILE).trim());
		panel.add(textField);
		textField.setColumns(10);
		JButton btnNewButton = new JButton("Search");
		btnNewButton.addActionListener(new ActionListener() {
			@Override public void actionPerformed(ActionEvent arg0) {
				listFiles();
			}
		});
		panel.add(btnNewButton, BorderLayout.EAST);
		
		final JPanel panel_1 = new JPanel();
		border = new TitledBorder(null, BORDER_TITLE, TitledBorder.LEADING, TitledBorder.TOP, null, null);
		panel_1.setBorder(border);
		contentPane.add(panel_1, BorderLayout.CENTER);
		panel_1.setLayout(new BorderLayout(0, 0));
		
		JSplitPane splitPane = new JSplitPane();
		panel_1.add(splitPane, BorderLayout.CENTER);
		
		final JScrollPane scrollPane_1 = new JScrollPane();
		splitPane.setRightComponent(scrollPane_1);
		
		textArea = new JTextArea();
		textArea.addKeyListener(new KeyAdapter() {
			@Override public void keyReleased(KeyEvent evt) {
				if (evt.getKeyCode() == KeyEvent.VK_S && evt.isControlDown()) {
					if (list.getSelectedValue() != null) {
						Utils.writeToFile(new File(list.getSelectedValue()), textArea.getText());
					}
				}
			}
		});
		scrollPane_1.setViewportView(textArea);
		
		JPanel pnlList = new JPanel();
		pnlList.setLayout(new BorderLayout(5, 5));
		JScrollPane scrollPane = new JScrollPane();
		pnlList.add(scrollPane);
		splitPane.setLeftComponent(pnlList);
		
		list = new JList<String>();
		list.addListSelectionListener(new ListSelectionListener() {
			@Override public void valueChanged(ListSelectionEvent arg0) {
				String selectedValue = list.getSelectedValue();
				if (selectedValue != null) {
					String content = Utils.readFileToString(new File(selectedValue));
					textArea.setText(content);
					textArea.setCaretPosition(0);
				}
			}
		});
		scrollPane.setViewportView(list);
		
		comboBox = new JComboBox<>();
		comboBox.addActionListener(new ActionListener() {
			@Override public void actionPerformed(ActionEvent arg0) {
				if (NO_AUTHOR.equals(comboBox.getSelectedItem())) {
					showNoAuthor();
				} else if (NO_HEADER.equals(comboBox.getSelectedItem())) {
					showNoHeader();
				}
			}
		});
		comboBox.setModel(new DefaultComboBoxModel<String>(new String[] {NO_HEADER, NO_AUTHOR}));
		pnlList.add(comboBox, BorderLayout.NORTH);
		
		JPanel panel_2 = new JPanel();
		contentPane.add(panel_2, BorderLayout.SOUTH);
		
		JLabel lblAuthor = new JLabel("Author:");
		panel_2.add(lblAuthor);
		
		tfAuthor = new JTextField();
		panel_2.add(tfAuthor);
		tfAuthor.setColumns(20);
		
		JButton btnAddLicenseHeader = new JButton("Add License Header");
		panel_2.add(btnAddLicenseHeader);
		btnAddLicenseHeader.addActionListener(new ActionListener() {
			@Override public void actionPerformed(ActionEvent arg0) {
				for(String s : list.getSelectedValuesList()) {
					check.addHeaderToFile(s, tfAuthor.getText());
				}
				if (list.getSelectedValuesList().isEmpty()) {
					JOptionPane.showMessageDialog(CheckLicenseGui.this, "You have to select at least one file to add the license header.", "no file selected", JOptionPane.INFORMATION_MESSAGE);
				}

				String content = Utils.readFileToString(new File(list.getSelectedValue()));
				textArea.setText(content);
				textArea.setCaretPosition(0);
			}
		});
	}

	protected void showNoAuthor() {
		list.setModel(models.get(NO_AUTHOR));
	}

	protected void showNoHeader() {
		list.setModel(models.get(NO_HEADER));
	}

	private final HashMap<String, DefaultListModel<String>> models = new HashMap<>();
	
	public void listFiles() {
		models.clear();
		
		HashMap<String, ArrayList<String>> files = check.checkFiles(textField.getText());
		for (String key : files.keySet()) {
			DefaultListModel<String> model = new DefaultListModel<String>();
			for (String s : files.get(key)) {
				model.addElement(s);
			}
			models.put(key, model);
		}
		
		if (NO_AUTHOR.equals(comboBox.getSelectedItem())) {
			showNoAuthor();
		} else if (NO_HEADER.equals(comboBox.getSelectedItem())) {
			showNoHeader();
		}
	}

}
