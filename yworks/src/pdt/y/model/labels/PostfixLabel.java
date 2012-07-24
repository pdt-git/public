/* $LICENSE_MSG$ */

package pdt.y.model.labels;

import java.awt.FontMetrics;
import java.awt.Graphics2D;

public class PostfixLabel extends CroppingLabelBase {

	@Override
	protected void fillText(String text, String[] lines, int lineWidth, Graphics2D gfx, FontMetrics fontmtx) {
		
		fillLinesFromEnd(text, lines, lineWidth, gfx, fontmtx);
		
		if (lines[0].length() > 0)
			lines[0] = "..." + lines[0].substring(1);
	}
}

