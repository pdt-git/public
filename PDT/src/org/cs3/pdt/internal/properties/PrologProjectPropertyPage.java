package org.cs3.pdt.internal.properties;

import org.cs3.pdt.IPrologProject;
import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.PropertyPage;

public class PrologProjectPropertyPage extends PropertyPage {

    private static final String OWNER_TITLE = "Source &Path:";

    private static final int TEXT_FIELD_WIDTH = 50;

    private Text sourcePathText;

    /**
     * Constructor for SamplePropertyPage.
     */
    public PrologProjectPropertyPage() {
        super();
    }

    /**
     * @see PreferencePage#createContents(Composite)
     */
    protected Control createContents(Composite parent) {
        Composite composite = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        composite.setLayout(layout);
        GridData data = new GridData(GridData.FILL);
        data.grabExcessHorizontalSpace = true;
        data.verticalAlignment = GridData.FILL;
        data.horizontalAlignment = GridData.FILL;
        composite.setLayoutData(data);

        //info text
        Text info = new Text(composite, SWT.READ_ONLY | SWT.MULTI | SWT.WRAP);
        String sep = System.getProperty("path.separator");
        info
                .setText("Please enter a '"
                        + sep
                        + "'-separated list of paths "
                        + "to your prolog source directories." +
                        		" \n The paths should be specified relative to the project root.");
        GridData gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.CENTER;
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = false;
        gd.horizontalSpan = 2;
        info.setLayoutData(gd);

        //Field label
        Label ownerLabel = new Label(composite, SWT.NONE);
        ownerLabel.setText(OWNER_TITLE);
        gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.CENTER;
        gd.grabExcessHorizontalSpace = false;
        gd.grabExcessVerticalSpace = false;
        gd.widthHint = convertWidthInCharsToPixels(ownerLabel.getText()
                .length() + 4);
        ownerLabel.setLayoutData(gd);

        //  text field
        sourcePathText = new Text(composite, SWT.SINGLE | SWT.BORDER);
        gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.FILL;
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = false;
        gd.widthHint = convertWidthInCharsToPixels(TEXT_FIELD_WIDTH);
        sourcePathText.setLayoutData(gd);

        // Populate owner text field
        try {
            IProject project = ((IProject) getElement());
            IPrologProject plProject = (IPrologProject) project.getNature(PDT.NATURE_ID);
            String sourcePath =plProject.getSourcePath();
            sourcePathText.setText((sourcePath != null) ? sourcePath : "");
        } catch (CoreException e) {
            sourcePathText.setText("");
        }
        return composite;
    }

    private String getSourcePathDefault() {
        IPreferencesService service = Platform.getPreferencesService();
        String qualifier = PDTPlugin.getDefault().getBundle().getSymbolicName();
        String d = service.getString(qualifier, PDT.PREF_SOURCE_PATH_DEFAULT,"", null);
        return d;
    }
    
    protected void performDefaults() {
        // Populate the owner text field with the default value
        sourcePathText.setText(getSourcePathDefault());
    }

    public boolean performOk() {
        // store the value in the owner text field
        try {
            IProject project = ((IProject) getElement());
            IPrologProject plProject = (IPrologProject) project.getNature(PDT.NATURE_ID);
            plProject.setSourcePath(sourcePathText.getText());
        } catch (CoreException e) {
            return false;
        }
        return true;
    }

}