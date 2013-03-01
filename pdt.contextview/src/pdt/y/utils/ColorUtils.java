package pdt.y.utils;

import java.awt.Color;

import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.graphics.RGB;

public class ColorUtils {

	public static String getColorString(Color color) {
		RGB rgb = new RGB(color.getRed(), color.getGreen(), color.getBlue());
		return StringConverter.asString(rgb);
	}

}
