package pdt.y.preferences;

import pdt.y.model.labels.BracketLabel;
import pdt.y.model.labels.MiddleLabel;
import pdt.y.model.labels.PostfixLabel;
import pdt.y.model.labels.PrefixLabel;

/**
 * Constant definitions for plug-in preferences
 */
public class PreferenceConstants {

	/* UPDATE MODE */
	
	public static final String P_UPDATE_MODE = "updateMode";
	
	public static final String P_UPDATE_MODE_MANUAL = "manual";
	
	public static final String P_UPDATE_MODE_AUTOMATIC = "automatic";
	
	/* NAME CROPPING */
	
	public static final String P_NAME_CROPPING = "nameCropping";
	
	public static final String P_NAME_CROPPING_PREFIX = PrefixLabel.class.getSimpleName();
	
	public static final String P_NAME_CROPPING_POSTFIX = PostfixLabel.class.getSimpleName();
	
	public static final String P_NAME_CROPPING_BRACKET = BracketLabel.class.getSimpleName();
	
	public static final String P_NAME_CROPPING_MIDDLE = MiddleLabel.class.getSimpleName();
}
