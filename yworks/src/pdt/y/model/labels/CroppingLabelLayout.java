/* $LICENSE_MSG$ */

package pdt.y.model.labels;

import java.awt.font.FontRenderContext;

import y.geom.YDimension;
import y.view.YLabel;
import y.view.YLabel.Layout;

public class CroppingLabelLayout implements Layout {

	@Override
	public void calculateContentSize(YLabel label, FontRenderContext rndCtx) {
		
		YDimension dimension = (YDimension)label.getUserData();
		
		if (dimension != null)
			label.setContentSize(dimension.getWidth(), dimension.getHeight());
	}

	@Override
	public boolean contains(YLabel label, double x, double y) {
		return false;
	}
}

