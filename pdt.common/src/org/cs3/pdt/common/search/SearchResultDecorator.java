package org.cs3.pdt.common.search;

import java.util.List;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.structureElements.PrologMatch;
import org.cs3.pdt.common.structureElements.SearchMatchElement;
import org.cs3.prolog.common.Util;
import org.eclipse.jface.viewers.BaseLabelProvider;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;

public class SearchResultDecorator extends BaseLabelProvider implements ILightweightLabelDecorator {

	@Override
	public void decorate(Object element, IDecoration decoration) {
		if (element instanceof SearchMatchElement) {
			SearchMatchElement matchElement = (SearchMatchElement) element;
			PrologMatch match = matchElement.getMatch();
			if (match == null) {
				return;
			}
			List<String> properties = match.getProperties();
			String property = PDTCommonUtil.getProperty("is_alias", properties);
			if (property != null) {
				decoration.addSuffix(" [" + Util.unquoteAtom(property) + "]");
			}
		}
	}

}
