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
	
	/* NODE SIZE */
	
	public static final String P_NODE_SIZE = "nodeSize";
	
	public static final String P_NODE_SIZE_FIXED = "fixed";
	
	public static final String P_NODE_SIZE_FIXED_WIDTH = "nodeSizeWidth";
	
	public static final String P_NODE_SIZE_FIXED_HEIGHT = "nodeSizeHeight";
	
	public static final String P_NODE_SIZE_MEDIAN = "median";
	
	public static final String P_NODE_SIZE_MAXIMUM = "maximum";
	
	public static final String P_NODE_SIZE_INDIVIDUAL = "individual";
	
	/* LAYOUT ALGORITHM */
	
	public static final String LAYOUT = "layout";
	
	public static final String LAYOUT_HIERARCHY = "hierarchy";
	
	public static final String LAYOUT_ORGANIC = "organic";
}
