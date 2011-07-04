package org.cs3.pdt.quickfix;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolutionGenerator;

public class PDTMarker implements IMarkerResolutionGenerator {

	public static final String SMELL_NAME = "PDT_Quickfix";
	public static final String QUICKFIX_DESCRIPTION = "PDT_QuickfixDescription";
	public static final String QUICKFIX_ACTION = "PDT_QuickfixAction";
	public static final String PDT_MARKER_ID = "PDT_Marker";

	@Override
	public IMarkerResolution[] getResolutions(IMarker mk) {
		try {
			Object smellName = mk.getAttribute(SMELL_NAME);
			Object quickfixDescription = mk.getAttribute(QUICKFIX_DESCRIPTION);

			if (smellName != null && !smellName.equals("") && quickfixDescription != null && !quickfixDescription.equals("")) {
				return new IMarkerResolution[] {
						new PDTQuickFix(quickfixDescription.toString(), false),
						new PDTQuickFix(quickfixDescription.toString(), true)
				};        	   
			} else  {
				return new IMarkerResolution[0];
			}

		}
		catch (CoreException e) {
			return new IMarkerResolution[0];
		}
	}

}
