/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package pdt.y.preferences;

import java.util.LinkedList;
import java.util.List;

import pdt.y.preferences.PreferenceConstants;
import pdt.y.preferences.controls.CheckboxFieldEditor;
import pdt.y.utils.GenericAction;
import pdt.y.utils.GenericEventListener;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

import pdt.y.main.PluginActivator;

public class PredicateVisibilityPreferences extends PreferencePageBase {
	
	private List<GenericAction> postInitializeActions = new LinkedList<GenericAction>();
	
	@Override
	protected void initialize() {
		super.initialize();
		
		for (GenericAction f : postInitializeActions) {
			f.action();
		}
	}
	
	@Override
	protected void createFieldEditors() {
		createPdtGroup();
		createSwiGroup();
	}

	private void createPdtGroup() {

		Group pdtPredicates = createGroup("PDT Predicates", groupLayout);
		
		final CheckboxFieldEditor pdtPredicatesCheckbox = new CheckboxFieldEditor(PreferenceConstants.PREDICATE_VISIBILITY_PDT_PREDICATES, "Show &PDT Predicates", wrap(pdtPredicates, cellData));
		
		final Composite pdtMetapredicatesCheckboxParent = wrap(pdtPredicates, cellData);
		final CheckboxFieldEditor pdtMetapredicatesCheckbox = new CheckboxFieldEditor(PreferenceConstants.PREDICATE_VISIBILITY_PDT_METAPREDICATES, "Show P&DT Metapredicates", pdtMetapredicatesCheckboxParent); 
		
		pdtPredicatesCheckbox.addListener(new GenericEventListener<Boolean>() {	
			@Override
			public void valueChanged(Boolean oldValue, Boolean newValue) {
				pdtMetapredicatesCheckbox.setEnabled(newValue, pdtMetapredicatesCheckboxParent);
			}
		});
		
		postInitializeActions.add(new GenericAction() {
			@Override
			public void action() {
				pdtMetapredicatesCheckbox.setEnabled(pdtPredicatesCheckbox.getBooleanValue(), pdtMetapredicatesCheckboxParent);
			}
		});
		
		addField(pdtPredicatesCheckbox);
		addField(pdtMetapredicatesCheckbox);
	}

	private void createSwiGroup() {

		Group swiPredicates = createGroup("SWI Predicates", groupLayout);
		
		final CheckboxFieldEditor swiPredicatesCheckbox = new CheckboxFieldEditor(PreferenceConstants.PREDICATE_VISIBILITY_SWI_PREDICATES, "Show &SWI Predicates", wrap(swiPredicates, cellData));
		
		final Composite swiMetapredicatesCheckboxParent = wrap(swiPredicates, cellData);
		final CheckboxFieldEditor swiMetapredicatesCheckbox = new CheckboxFieldEditor(PreferenceConstants.PREDICATE_VISIBILITY_SWI_METAPREDICATES, "Show S&WI Metapredicates", swiMetapredicatesCheckboxParent);
		
		swiPredicatesCheckbox.addListener(new GenericEventListener<Boolean>() {	
			@Override
			public void valueChanged(Boolean oldValue, Boolean newValue) {
				swiMetapredicatesCheckbox.setEnabled(newValue, swiMetapredicatesCheckboxParent);
			}
		});

		postInitializeActions.add(new GenericAction() {
			@Override
			public void action() {
				swiMetapredicatesCheckbox.setEnabled(swiPredicatesCheckbox.getBooleanValue(), swiMetapredicatesCheckboxParent);
			}
		});
		
		addField(swiPredicatesCheckbox);
		addField(swiMetapredicatesCheckbox);
	}

	public static IPreferenceStore getCurrentPreferences() {
		return PluginActivator.getDefault().getPreferenceStore();
	}
	
	public static boolean showPDTPredicates() {
		return getCurrentPreferences().getBoolean(PreferenceConstants.PREDICATE_VISIBILITY_PDT_PREDICATES);
	}
	
	public static boolean showPDTMetapredicates() {
		return getCurrentPreferences().getBoolean(PreferenceConstants.PREDICATE_VISIBILITY_PDT_METAPREDICATES);
	}
	
	public static boolean showSWIPredicates() {
		return getCurrentPreferences().getBoolean(PreferenceConstants.PREDICATE_VISIBILITY_SWI_PREDICATES);
	}
	
	public static boolean showSWIMetapredicates() {
		return getCurrentPreferences().getBoolean(PreferenceConstants.PREDICATE_VISIBILITY_SWI_METAPREDICATES);
	}
}
