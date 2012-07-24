/* $LICENSE_MSG$ */

package pdt.y.model.labels;

import java.awt.FontMetrics;
import java.awt.Graphics2D;

public class PrefixLabel extends CroppingLabelBase {

	@Override
	protected void fillText(String text, String[] lines, int lineWidth,
			Graphics2D gfx, FontMetrics fontmtx) {

		fillLinesFromStart(text, lines, lineWidth, gfx, fontmtx);
		
		String lastLine = lines[lines.length - 1];
		if (lastLine.length() > 0)
			lines[lines.length - 1] = lastLine.substring(0, lastLine.length() - 1) + "...";
	}

}

