package org.cs3.pdt.preferences;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.editors.PDTColors;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * This class represents a preference page that is contributed to the
 * Preferences dialog. By subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows us to create a page
 * that is small and knows how to save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They are stored in the
 * preference store that belongs to the main plug-in class. That way,
 * preferences can be accessed directly via the preference store.
 */

public class PreferencePageColor extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
	private ColorFieldEditor background;
	private ColorFieldEditor backgroundExtern;
	private ColorFieldEditor default_;
	private ColorFieldEditor string;
	private ColorFieldEditor comment;
	private ColorFieldEditor variable;
	private ColorFieldEditor undefined;
	private ColorFieldEditor keyword;
	private ColorFieldEditor dynamic;
	private ColorFieldEditor transparent;
	private ColorFieldEditor meta;

	
	public PreferencePageColor() {
		super(GRID);
		setPreferenceStore(PDTPlugin.getDefault().getPreferenceStore());
		setDescription("Editor color preferences");
	}

	private void initColorFieldEditors(boolean show_colors){
		background.setEnabled(show_colors, getFieldEditorParent());
		backgroundExtern.setEnabled(show_colors, getFieldEditorParent());
		default_.setEnabled(show_colors, getFieldEditorParent());
		string.setEnabled(show_colors, getFieldEditorParent());
		comment.setEnabled(show_colors, getFieldEditorParent());
		variable.setEnabled(show_colors, getFieldEditorParent());
		undefined.setEnabled(show_colors, getFieldEditorParent());
		keyword.setEnabled(show_colors, getFieldEditorParent());
		dynamic.setEnabled(show_colors, getFieldEditorParent());
		transparent.setEnabled(show_colors, getFieldEditorParent());
		meta.setEnabled(show_colors, getFieldEditorParent());
	}
	
	
	class BooleanFieldEditorWithAccessToCheckBox extends BooleanFieldEditor{
			
		public BooleanFieldEditorWithAccessToCheckBox() {
			super();
		}

		public BooleanFieldEditorWithAccessToCheckBox(String name, String label, Composite parent) {
			super(name, label, parent);
		}

		public BooleanFieldEditorWithAccessToCheckBox(String name, String labelText, int style, Composite parent) {
			super(name, labelText, style, parent);
		}

		public Button getCheckBox(Composite parent){
			return getChangeControl(parent);
		}
	}
	
	
	
	
	/**
	 * Creates the field editors. Field editors are abstractions of the common
	 * GUI blocks needed to manipulate various types of preferences. Each field
	 * editor knows how to save and restore itself.
	 */
	@Override
	public void createFieldEditors() {
		background = new ColorFieldEditor(PDTColors.PREF_BACKGROUND, PDTColors.BACKGROUND_STRING, getFieldEditorParent());
		backgroundExtern = new ColorFieldEditor(PDTColors.PREF_BACKGROUND_EXTERNAL_FILES, PDTColors.BACKGROUND_EXTERN_STRING, getFieldEditorParent());
		default_ = new ColorFieldEditor(PDTColors.PREF_DEFAULT, PDTColors.DEFAULT_STRING, getFieldEditorParent());
		string = new ColorFieldEditor(PDTColors.PREF_STRING, PDTColors.STRING_STRING, getFieldEditorParent());
		comment = new ColorFieldEditor(PDTColors.PREF_COMMENT, PDTColors.COMMENT_STRING, getFieldEditorParent());
		variable = new ColorFieldEditor(PDTColors.PREF_VARIABLE, PDTColors.VARIABLE_STRING, getFieldEditorParent());
		undefined = new ColorFieldEditor(PDTColors.PREF_UNDEFINED, PDTColors.UNDEFINED_STRING, getFieldEditorParent());
		keyword = new ColorFieldEditor(PDTColors.PREF_BUILTIN, PDTColors.BUILT_IN_STRING, getFieldEditorParent());
		dynamic = new ColorFieldEditor(PDTColors.PREF_DYNAMIC, PDTColors.DYNAMIC_STRING, getFieldEditorParent());
		transparent = new ColorFieldEditor(PDTColors.PREF_TRANSPARENT, PDTColors.MODULE_TRANSPARENT_STRING, getFieldEditorParent());
		meta = new ColorFieldEditor(PDTColors.PREF_META, PDTColors.META_PREDICATE_STRING, getFieldEditorParent());
		
		addField(background);
		addField(backgroundExtern);
		addField(default_);
		addField(string);
		addField(comment);
		addField(variable);		
		addField(undefined);
		addField(keyword);
		addField(dynamic);
		addField(transparent);
		addField(meta);
		
		initColorFieldEditors(true);
		
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	@Override
	public void init(IWorkbench workbench) {
	}

}