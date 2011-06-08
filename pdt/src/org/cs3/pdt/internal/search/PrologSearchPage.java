package org.cs3.pdt.internal.search;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.queries.CategorizedDefinitionsSearchQuery;
import org.cs3.pdt.internal.queries.PDTSearchQuery;
import org.cs3.pdt.internal.queries.ReferencesSearchQueryDirect;
import org.cs3.pl.metadata.Goal;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jdt.core.formatter.IndentManipulation;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.SearchPattern;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.search.ui.ISearchPage;
import org.eclipse.search.ui.ISearchPageContainer;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;


public class PrologSearchPage extends DialogPage implements ISearchPage {

	
	private static class SearchPatternData {
		private final int searchFor;
		private final int limitTo;
		private final String pattern;
				
		public SearchPatternData(int searchFor, int limitTo, String pattern) {
			this.searchFor= searchFor;
			this.limitTo= limitTo;
			this.pattern= pattern;
		}

		public int getLimitTo() {
			return limitTo;
		}

		public String getPattern() {
			return pattern;
		}
		public int getSearchFor() {
			return searchFor;
		}
		
		public void store(IDialogSettings settings) {
			settings.put("searchFor", searchFor); //$NON-NLS-1$
			settings.put("pattern", pattern); //$NON-NLS-1$
			settings.put("limitTo", limitTo); //$NON-NLS-1$
		}
		
		public static SearchPatternData create(IDialogSettings settings) {
			String pattern= settings.get("pattern"); //$NON-NLS-1$
			if (pattern.length() == 0) {
				return null;
			}

			try {
				int searchFor= settings.getInt("searchFor"); //$NON-NLS-1$
				int limitTo= settings.getInt("limitTo"); //$NON-NLS-1$
				
				return new SearchPatternData(searchFor, limitTo, pattern);
			} catch (NumberFormatException e) {
				return null;
			}
		}
	}
	
	// search for
	private final static int MODULE= 0;
	private final static int PREDICATE= 1;
	//private final static int CONSTRUCTOR= IJavaSearchConstants.CONSTRUCTOR;
	
	// limit to
	private final static int DECLARATIONS= IJavaSearchConstants.DECLARATIONS;
	private final static int REFERENCES= IJavaSearchConstants.REFERENCES;
	
	public static final String PARTICIPANT_EXTENSION_POINT= "org.eclipse.jdt.ui.queryParticipants"; //$NON-NLS-1$

	public static final String EXTENSION_POINT_ID= "org.cs3.pdt.prologSearchPage"; //$NON-NLS-1$
	
	private static final int HISTORY_SIZE= 12;
	
	// Dialog store id constants
	private final static String PAGE_NAME= "PrologSearchPage"; //$NON-NLS-1$
//	private final static String STORE_CASE_SENSITIVE= "CASE_SENSITIVE"; //$NON-NLS-1$
	private final static String STORE_HISTORY= "HISTORY"; //$NON-NLS-1$
	private final static String STORE_HISTORY_SIZE= "HISTORY_SIZE"; //$NON-NLS-1$
	
	private final List<SearchPatternData> fPreviousSearchPatterns;
	
//	private SearchPatternData fInitialData;
	private boolean fFirstTime= true;
	private IDialogSettings fDialogSettings;
//	private boolean fIsCaseSensitive;
	
	private Combo fPattern;
	private ISearchPageContainer fContainer;
	private Button fCaseSensitive;
	
	private Button[] fSearchFor;
	private Button[] fLimitTo;
	private Group fLimitToGroup;
	
	public PrologSearchPage() {
		fPreviousSearchPatterns= new ArrayList<SearchPatternData>();
	}
	
	
	//---- Action Handling ------------------------------------------------
	
	public boolean performAction() {
		return performNewSearch();
	}
	
	private boolean performNewSearch() {
		SearchPatternData data= getPatternData();
		
		int searchFor= data.getSearchFor();
		int limitTo= data.getLimitTo();
	
		PDTSearchQuery searchQuery;
		
		Goal goal;
		if (searchFor == PREDICATE)
			goal = new Goal("", "", data.pattern, -1, data.pattern);
		else
			goal = new Goal("", data.pattern, "", -1, data.pattern+":Predicate");

		if (limitTo == REFERENCES)
			searchQuery = new ReferencesSearchQueryDirect(null, goal);
		else 
			searchQuery = new CategorizedDefinitionsSearchQuery(null, goal);

		NewSearchUI.activateSearchResultView();
		NewSearchUI.runQueryInForeground(null,searchQuery);
		return true;
	}
	
	private int getLimitTo() {
		for (int i= 0; i < fLimitTo.length; i++) {
			Button button= fLimitTo[i];
			if (button.getSelection()) {
				return getIntData(button);
			}
		}
		return -1;
	}

	private String[] getPreviousSearchPatterns() {
		// Search results are not persistent
		int patternCount= fPreviousSearchPatterns.size();
		String [] patterns= new String[patternCount];
		for (int i= 0; i < patternCount; i++)
			patterns[i]= ((SearchPatternData) fPreviousSearchPatterns.get(i)).getPattern();
		return patterns;
	}
	
	private int getSearchFor() {
		for (int i= 0; i < fSearchFor.length; i++) {
			Button button= fSearchFor[i];
			if (button.getSelection()) {
				return getIntData(button);
			}
		}
		Assert.isTrue(false, "shouldNeverHappen"); //$NON-NLS-1$
		return -1;
	}
	
	private void setSearchFor(int searchFor) {
		for (int i= 0; i < fSearchFor.length; i++) {
			Button button= fSearchFor[i];
			button.setSelection(searchFor == getIntData(button));
		}
	}
	
	private int getIntData(Button button) {
		return ((Integer) button.getData()).intValue();
	}
	
	private String getPattern() {
		return fPattern.getText();
	}

	
	private SearchPatternData findInPrevious(String pattern) {
		for (Iterator<SearchPatternData> iter= fPreviousSearchPatterns.iterator(); iter.hasNext();) {
			SearchPatternData element= iter.next();
			if (pattern.equals(element.getPattern())) {
				return element;
			}
		}
		return null;
	}
	
	/**
	 * Return search pattern data and update previous searches.
	 * An existing entry will be updated.
	 * @return the pattern data
	 */
	private SearchPatternData getPatternData() {
		String pattern= getPattern();
		SearchPatternData match= findInPrevious(pattern);
		if (match != null) {
			fPreviousSearchPatterns.remove(match);
		}
		match= new SearchPatternData(
				getSearchFor(),
				getLimitTo(),
				pattern
		);
			
		fPreviousSearchPatterns.add(0, match); // insert on top
		return match;
	}

	/*
	 * Implements method from IDialogPage
	 */
	public void setVisible(boolean visible) {
		if (visible && fPattern != null) {
			if (fFirstTime) {
				fFirstTime= false;
				// Set item and text here to prevent page from resizing
				fPattern.setItems(getPreviousSearchPatterns());
				initSelections();
			}
			fPattern.setFocus();
		}
		updateOKStatus();
		super.setVisible(visible);
	}
	
	public boolean isValid() {
		return true;
	}

	//---- Widget creation ------------------------------------------------

	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
	 */
	public void createControl(Composite parent) {
		initializeDialogUnits(parent);
		readConfiguration();
		
		Composite result= new Composite(parent, SWT.NONE);
		
		GridLayout layout= new GridLayout(2, false);
		layout.horizontalSpacing= 10;
		result.setLayout(layout);
		
		Control expressionComposite= createExpression(result);
		expressionComposite.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false, 2, 1));
		
		Label separator= new Label(result, SWT.NONE);
		separator.setVisible(false);
		GridData data= new GridData(GridData.FILL, GridData.FILL, false, false, 2, 1);
		data.heightHint= convertHeightInCharsToPixels(1) / 3;
		separator.setLayoutData(data);
		
		Control searchFor= createSearchFor(result);
		searchFor.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));

		Control limitTo= createLimitTo(result);
		limitTo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));
				
		//createParticipants(result);
		
		SelectionAdapter javaElementInitializer= new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				int limitTo = getLimitTo();
				fillLimitToGroup(limitTo);
			}
		};

		for (int i= 0; i < fSearchFor.length; i++) {
			fSearchFor[i].addSelectionListener(javaElementInitializer);
		}

		setControl(result);

		Dialog.applyDialogFont(result);
//		PlatformUI.getWorkbench().getHelpSystem().setHelp(result, IJavaHelpContextIds.JAVA_SEARCH_PAGE);	
	}
	


	private Control createExpression(Composite parent) {
		Composite result= new Composite(parent, SWT.NONE);
		GridLayout layout= new GridLayout(2, false);
		layout.marginWidth= 0;
		layout.marginHeight= 0;
		result.setLayout(layout);

		// Pattern text + info
		Label label= new Label(result, SWT.LEFT);
//		label.setText(SearchMessages.SearchPage_expression_label);
		label.setText("Search String");
		label.setLayoutData(new GridData(GridData.FILL, GridData.FILL, false, false, 2, 1));

		// Pattern combo
		fPattern= new Combo(result, SWT.SINGLE | SWT.BORDER);
		fPattern.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				handlePatternSelected();
				updateOKStatus();
			}
		});
		fPattern.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				updateOKStatus();

			}
		});
//		TextFieldNavigationHandler.install(fPattern);
		GridData data= new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1);
		data.widthHint= convertWidthInCharsToPixels(50);
		fPattern.setLayoutData(data);
		
		return result;
	}
	
	final void updateOKStatus() {
		boolean isValid= isValidSearchPattern();
		getContainer().setPerformActionEnabled(isValid);
	}
	
	private boolean isValidSearchPattern() {
		if (getPattern().length() == 0) {
			return false;
		}
		return SearchPattern.createPattern(getPattern(), getSearchFor(), getLimitTo(), SearchPattern.R_EXACT_MATCH) != null;		
	}
	
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.DialogPage#dispose()
	 */
	public void dispose() {
		writeConfiguration();
		super.dispose();
	}

	private void handlePatternSelected() {
		int selectionIndex= fPattern.getSelectionIndex();
		if (selectionIndex < 0 || selectionIndex >= fPreviousSearchPatterns.size())
			return;
		
		SearchPatternData initialData= (SearchPatternData) fPreviousSearchPatterns.get(selectionIndex);

		setSearchFor(initialData.getSearchFor());
		int limitTo = initialData.getLimitTo();
		fillLimitToGroup(limitTo);
		fPattern.setText(initialData.getPattern());
		fCaseSensitive.setEnabled(true);
		
//		fInitialData= initialData;
	}
	

	private Control createSearchFor(Composite parent) {
		Group result= new Group(parent, SWT.NONE);
		result.setText("Search For"); 
		result.setLayout(new GridLayout(2, true));

		fSearchFor= new Button[] {
			createButton(result, SWT.RADIO, "Module", MODULE, false),
			createButton(result, SWT.RADIO, "Predicate", PREDICATE, false),
		};
			
		// Fill with dummy radio buttons
		Label filler= new Label(result, SWT.NONE);
		filler.setVisible(false);
		filler.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false, 1, 1));

		return result;		
	}
	
	private Control createLimitTo(Composite parent) {
		fLimitToGroup= new Group(parent, SWT.NONE);
		fLimitToGroup.setText("Limit To"); 
		fLimitToGroup.setLayout(new GridLayout(2, false));

		fillLimitToGroup(DECLARATIONS);
		return fLimitToGroup;
	}
		
	private void fillLimitToGroup(int limitTo) {
		Control[] children= fLimitToGroup.getChildren();
		for (int i= 0; i < children.length; i++) {
			children[i].dispose();
		}		
		ArrayList<Button> buttons= new ArrayList<Button>();
		buttons.add(createButton(fLimitToGroup, SWT.RADIO, "Declarations & Definitions", DECLARATIONS, limitTo == DECLARATIONS));
		buttons.add(createButton(fLimitToGroup, SWT.RADIO, "References", REFERENCES, limitTo == REFERENCES));
		
		fLimitTo= (Button[]) buttons.toArray(new Button[buttons.size()]);
		
		SelectionAdapter listener= new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				performLimitToSelectionChanged((Button) e.widget);
			}
		};
		for (int i= 0; i < fLimitTo.length; i++) {
			fLimitTo[i].addSelectionListener(listener);
		}
		Dialog.applyDialogFont(fLimitToGroup); // re-apply font as we disposed the previous widgets
		
		fLimitToGroup.layout();
	}

	protected final void performLimitToSelectionChanged(Button button) {
		if (button.getSelection()) {
			for (int i= 0; i < fLimitTo.length; i++) {
				Button curr= fLimitTo[i];
				if (curr != button) {
					curr.setSelection(false);
				}
			}
		}
	}
		
	private Button createButton(Composite parent, int style, String text, int data, boolean isSelected) {
		Button button= new Button(parent, style);
		button.setText(text);
		button.setData(new Integer(data));
		button.setLayoutData(new GridData(SWT.BEGINNING, SWT.CENTER, false, false));
		button.setSelection(isSelected);
		return button;
	}
	
	private void initSelections() {
		ISelection sel= getContainer().getSelection();
		
		SearchPatternData initData= null;

		if (sel instanceof ITextSelection) {
			if (initData == null) {
				initData= trySimpleTextSelection((ITextSelection) sel);
			}
		}
		if (initData == null) {
			initData= getDefaultInitValues();
		}
		
//		fInitialData= initData;
		setSearchFor(initData.getSearchFor());
		int limitTo = initData.getLimitTo();
		fillLimitToGroup(limitTo);
		fPattern.setText(initData.getPattern());
	}
	
	private SearchPatternData trySimpleTextSelection(ITextSelection selection) {
		String selectedText= selection.getText();
		if (selectedText != null && selectedText.length() > 0) {
			int i= 0;
			while (i < selectedText.length() && !IndentManipulation.isLineDelimiterChar(selectedText.charAt(i))) {
				i++;
			}
			if (i > 0) {
				return new SearchPatternData(PREDICATE, REFERENCES, selectedText.substring(0, i));
			}
		}
		return null;
	}
	
	private SearchPatternData getDefaultInitValues() {
		if (!fPreviousSearchPatterns.isEmpty()) {
			return (SearchPatternData) fPreviousSearchPatterns.get(0);
		}

		return new SearchPatternData(PREDICATE, REFERENCES, ""); //$NON-NLS-1$
	}
	
	/*
	 * Implements method from ISearchPage
	 */
	public void setContainer(ISearchPageContainer container) {
		fContainer= container;
	}
	
	/**
	 * Returns the search page's container.
	 * @return the search page container
	 */
	private ISearchPageContainer getContainer() {
		return fContainer;
	}
	
	//--------------- Configuration handling --------------
	
	/**
	 * Returns the page settings for this Java search page.
	 * 
	 * @return the page settings to be used
	 */
	private IDialogSettings getDialogSettings() {
		if (fDialogSettings == null) {
			fDialogSettings= PDTPlugin.getDefault().getDialogSettingsSection(PAGE_NAME);
		}
		return fDialogSettings;
	}
	
	/**
	 * Initializes itself from the stored page settings.
	 */
	private void readConfiguration() {
		IDialogSettings s= getDialogSettings();
		
		try {
			int historySize= s.getInt(STORE_HISTORY_SIZE);
			for (int i= 0; i < historySize; i++) {
				IDialogSettings histSettings= s.getSection(STORE_HISTORY + i);
				if (histSettings != null) {
					SearchPatternData data= SearchPatternData.create(histSettings);
					if (data != null) {
						fPreviousSearchPatterns.add(data);
					}
				}
			}
		} catch (NumberFormatException e) {
			// ignore
		}
	}
	
	/**
	 * Stores the current configuration in the dialog store.
	 */
	private void writeConfiguration() {
		IDialogSettings s= getDialogSettings();
		
		int historySize= Math.min(fPreviousSearchPatterns.size(), HISTORY_SIZE);
		s.put(STORE_HISTORY_SIZE, historySize);
		for (int i= 0; i < historySize; i++) {
			IDialogSettings histSettings= s.addNewSection(STORE_HISTORY + i);
			SearchPatternData data= ((SearchPatternData) fPreviousSearchPatterns.get(i));
			data.store(histSettings);
		}
	}
}
