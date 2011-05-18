/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pdt.internal.editors;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.cs3.pdt.PDTPlugin;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

public class ColorManager {
	
	private IPreferenceStore store;
	
	protected Map<RGB, Color> fColorTable = new HashMap<RGB, Color>(10);
	
	public ColorManager() {
		store = PDTPlugin.getDefault().getPreferenceStore();
	}

	public void dispose() {
		Iterator<Color> e = fColorTable.values().iterator();
		while (e.hasNext())
			 e.next().dispose();
	}
	
	public Color getColor(RGB rgb) {
		Color color = fColorTable.get(rgb);
		if (color == null) {
			color = new Color(Display.getCurrent(), rgb);
			fColorTable.put(rgb, color);
		}
		return color;
	}

	public RGB getBackgroundColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_BACKGROUND);
	}

	public RGB getDefaultColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_DEFAULT);  
	}

	public RGB getStringColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_STRING);
	}

	public RGB getCommentColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_COMMENT);	
	}

	public RGB getVariableColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_VARIABLE);
	}

	public RGB getUndefinedColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_UNDEFINED);
	}

	public RGB getKeywordColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_BUILTIN);
	}

	public RGB getDynamicColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_DYNAMIC);
	}

	public RGB getTransparentColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_TRANSPARENT);
	}
	
	public RGB getMetaColor() {
		return PreferenceConverter.getColor(store, PDTColors.PREF_META);
	}
}
