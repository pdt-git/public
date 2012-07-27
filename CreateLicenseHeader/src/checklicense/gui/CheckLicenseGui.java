package checklicense.gui;

import java.awt.BorderLayout;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;

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
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

public class CheckLicenseGui extends JFrame {

	private static final long serialVersionUID = 1L;
	private static final File LAST_PATH_FILE = new File("data/last_path.txt");
	private JPanel contentPane;
	private JTextField textField;
	private CheckLicense check;
	private JList<String> list;
	private JTextArea textArea;

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
		
		JButton btnAddLicenseHeader = new JButton("Add License Header");
		btnAddLicenseHeader.addActionListener(new ActionListener() {
			@Override public void actionPerformed(ActionEvent arg0) {
				for(String s : list.getSelectedValuesList()) {
					check.addHeaderToFile(s);
				}
				if (list.getSelectedValuesList().isEmpty()) {
					JOptionPane.showMessageDialog(CheckLicenseGui.this, "You have to select at least one file to add the license header.", "no file selected", JOptionPane.INFORMATION_MESSAGE);
				}

				String content = Utils.readFileToString(new File(list.getSelectedValue()));
				textArea.setText(content);
				textArea.setCaretPosition(0);
			}
		});
		contentPane.add(btnAddLicenseHeader, BorderLayout.SOUTH);
		
		JPanel panel_1 = new JPanel();
		panel_1.setBorder(new TitledBorder(null, "Files without valid license header", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		contentPane.add(panel_1, BorderLayout.CENTER);
		panel_1.setLayout(new BorderLayout(0, 0));
		
		JSplitPane splitPane = new JSplitPane();
		panel_1.add(splitPane, BorderLayout.CENTER);
		
		final JScrollPane scrollPane_1 = new JScrollPane();
		splitPane.setRightComponent(scrollPane_1);
		
		textArea = new JTextArea();
		scrollPane_1.setViewportView(textArea);
		
		JScrollPane scrollPane = new JScrollPane();
		splitPane.setLeftComponent(scrollPane);
		
		list = new JList<String>();
		list.addListSelectionListener(new ListSelectionListener() {
			@Override public void valueChanged(ListSelectionEvent arg0) {
				String content = Utils.readFileToString(new File(list.getSelectedValue()));
				textArea.setText(content);
				textArea.setCaretPosition(0);
			}
		});
		scrollPane.setViewportView(list);
	}

	public void listFiles() {
		ArrayList<String> files = check.checkFiles(textField.getText());
		DefaultListModel<String> model = new DefaultListModel<String>();
		for (String s : files) {
			model.addElement(s);
		}
		list.setModel(model);
	}

}
