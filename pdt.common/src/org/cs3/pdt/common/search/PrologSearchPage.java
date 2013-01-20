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

package org.cs3.pdt.common.search;

import java.util.ArrayList;
import java.util.List;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.metadata.Goal;
import org.cs3.pdt.common.queries.DeadPredicatesSearchQuery;
import org.cs3.pdt.common.queries.GlobalDefinitionsSearchQuery;
import org.cs3.pdt.common.queries.ModuleDefinitionsSearchQuery;
import org.cs3.pdt.common.queries.ModuleReferenceSearchQuery;
import org.cs3.pdt.common.queries.PDTSearchQuery;
import org.cs3.pdt.common.queries.ReferencesSearchQueryDirect;
import org.cs3.pdt.common.queries.UndefinedCallsSearchQuery;
import org.cs3.prolog.common.Util;
import org.eclipse.jdt.core.formatter.IndentManipulation;
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

    private static final String PREDICATE_DEF_HINT = "Search for predicates declarations and definitions (e.g. \"member\" or \"member/2\")";
    private static final String PREDICATE_REF_HINT = "Search for references to predicates (e.g. \"member\" or \"member/2\")";
    private static final String ENTITY_DEF_HINT = "Search for entities (e.g. module \"lists\")";
    private static final String ENTITY_REF_HINT = "Search for references to entities (e.g. module \"lists\")";
    private static final String UNDEFINED_HINT = "Search for undefined calls";
    private static final String DEAD_HINT = "Search for dead predicates";
    
    private static final String[][] hints = new String[][]{
    	{ENTITY_DEF_HINT, ENTITY_REF_HINT},
    	{PREDICATE_DEF_HINT, PREDICATE_REF_HINT},
    	{UNDEFINED_HINT, UNDEFINED_HINT},
    	{DEAD_HINT, DEAD_HINT}
    };

	private static class SearchPatternData {
        private static final String SEARCH_FOR = "searchFor";
        private static final String PATTERN2 = "pattern";
        private static final String LIMIT_TO = "limitTo";
        private static final String EXACT_MATCH = "exactMatch";

        private final int searchFor;
        private final int limitTo;
        private final String pattern;
        private final boolean exactMatch;

        public SearchPatternData(int searchFor, int limitTo, String pattern, boolean exactMatch) {
            this.searchFor = searchFor;
            this.limitTo = limitTo;
            this.pattern = pattern;
            this.exactMatch = exactMatch;
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

        public boolean isExactMatch() {
            return exactMatch;
        }

        public void store(IDialogSettings settings) {
            settings.put(SEARCH_FOR, searchFor);
            settings.put(PATTERN2, pattern);
            settings.put(LIMIT_TO, limitTo);
            settings.put(EXACT_MATCH, exactMatch);
        }

        public static SearchPatternData create(IDialogSettings settings) {
            String pattern = settings.get(PATTERN2);
            if (pattern.length() == 0) {
                return null;
            }

            try {
                int searchFor = settings.getInt(SEARCH_FOR);
                int limitTo = settings.getInt(LIMIT_TO);
                boolean exactMatch = settings.getBoolean(EXACT_MATCH);

                return new SearchPatternData(searchFor, limitTo, pattern, exactMatch);
            } catch (NumberFormatException e) {
                return null;
            }
        }
    }

    // search for
    private final static int MODULE = 0;
    private final static int PREDICATE = 1;
    private final static int UNDEFINED_CALL = 2;
    private final static int DEAD_PREDICATE = 3;

    // limit to
    private final static int DECLARATIONS = 0;
    private final static int REFERENCES = 1;

    public static final String EXTENSION_POINT_ID = "org.cs3.pdt.prologSearchPage";

    private static final int HISTORY_SIZE = 12;

    // Dialog store id constants
    private final static String PAGE_NAME = "PrologSearchPage";
    private final static String STORE_HISTORY = "HISTORY";
    private final static String STORE_HISTORY_SIZE = "HISTORY_SIZE";

    private final List<SearchPatternData> previousSearchPatterns = new ArrayList<SearchPatternData>();

    private boolean firstSetVisibleCall = true;
    private IDialogSettings dialogSettings;

    private Combo patternList;
    private ISearchPageContainer searchPageContainer;

    private Button[] searchForRadioButtons;
    private Button[] limitToRadioButtons;
    private Button exactMatchCheckBox;
	private Label explainingLabel;

    //---- Action Handling ------------------------------------------------

    @Override
    public boolean performAction() {
        return performNewSearch();
    }

    private boolean performNewSearch() {
        SearchPatternData data = getPatternDataAndUpdatePatternHistory();

        int searchFor = data.getSearchFor();
        int limitTo = data.getLimitTo();

        PDTSearchQuery searchQuery = null;

        Goal goal;
        if (searchFor == PREDICATE) {
            String functor = "";
            int arity = 0;
            int lastSlash = data.pattern.lastIndexOf("/");
            if (lastSlash == -1) {
                functor = data.pattern;
                arity = -1;
            } else {
                try {
                    arity = Integer.parseInt(data.pattern.substring(lastSlash + 1));
                } catch (NumberFormatException e) {}
                functor = data.pattern.substring(0, lastSlash);
            }
            goal = new Goal("", null, Util.quoteAtom(functor), arity, null, data.isExactMatch());
            
            if (limitTo == REFERENCES) {
            	searchQuery = new ReferencesSearchQueryDirect(goal);
            } else {
            	searchQuery = new GlobalDefinitionsSearchQuery(goal);
            }
        } else if (searchFor == MODULE) {
        	boolean exactMatch = data.isExactMatch();
			goal = new Goal("", data.pattern, "", 0, data.pattern, exactMatch);
            
            if (limitTo == REFERENCES) {
            	searchQuery = new ModuleReferenceSearchQuery(goal);
            } else {
            	searchQuery = new ModuleDefinitionsSearchQuery(goal);
            }
        } else if (searchFor == UNDEFINED_CALL) {
        	searchQuery = new UndefinedCallsSearchQuery();
        } else if (searchFor == DEAD_PREDICATE) {
        	searchQuery = new DeadPredicatesSearchQuery();
        }

        NewSearchUI.activateSearchResultView();
        NewSearchUI.runQueryInForeground(null,searchQuery);
        return true;
    }

    private String getPattern() {
        return patternList.getText();
    }

    private int getLimitTo() {
        for (int i = 0; i < limitToRadioButtons.length; i++) {
            Button button = limitToRadioButtons[i];
            if (button.getSelection()) {
                return getIntData(button);
            }
        }
        return -1;
    }

    private int getSearchFor() {
        for (int i = 0; i < searchForRadioButtons.length; i++) {
            Button button = searchForRadioButtons[i];
            if (button.getSelection()) {
                return getIntData(button);
            }
        }
        return -1;
    }

    private boolean isExactMatch() {
        return exactMatchCheckBox.getSelection();
    }

    private Object[] getFunctorAndArity(String pattern) {
        if (pattern == null || pattern.isEmpty()) {
            return null;
        }
        String functor = "";
        int arity = 0;
        int lastSlash = pattern.lastIndexOf("/");
        if (lastSlash == -1) {
            functor = pattern;
            arity = -1;
        } else {
            try {
                arity = Integer.parseInt(pattern.substring(lastSlash + 1));
            } catch (NumberFormatException e) {
                return null;
            }
            if (arity < 0) {
                return null;
            }
            functor = pattern.substring(0, lastSlash);
        }
        return new Object[]{functor, arity};
    }

    private void setPattern(String pattern) {
        patternList.setText(pattern);
    }

    private void setSearchFor(int searchFor) {
        for (int i = 0; i < searchForRadioButtons.length; i++) {
            Button button = searchForRadioButtons[i];
            button.setSelection(searchFor == getIntData(button));
        }
        updateLabelAndEnablement(searchFor, getLimitTo());
    }

	private void updateLabelAndEnablement(int searchFor, int limitTo) {
		if (searchFor >= 0 && searchFor < 4 && limitTo >= 0 && limitTo < 2) {
        	explainingLabel.setText(hints[searchFor][limitTo]);
    		setSearchFieldsEnablement(searchFor < 2);
        }
	}
	
	private void setSearchFieldsEnablement(boolean enablement) {
		patternList.setEnabled(enablement);
		exactMatchCheckBox.setEnabled(enablement);
		for (Button limitToButton : limitToRadioButtons) {
			limitToButton.setEnabled(enablement);
		}
	}

    private void setLimitTo(int limitTo) {
        for (int i = 0; i < limitToRadioButtons.length; i++) {
            Button button = limitToRadioButtons[i];
            button.setSelection(limitTo == getIntData(button));
        }
        updateLabelAndEnablement(getSearchFor(), limitTo);
    }

    private void setExactMatch(boolean exactMatch) {
        exactMatchCheckBox.setSelection(exactMatch);
    }

    private String[] getPreviousSearchPatterns() {
        // Search results are not persistent
        int patternCount = previousSearchPatterns.size();
        String [] patterns = new String[patternCount];
        for (int i = 0; i < patternCount; i++)
            patterns[i] = ((SearchPatternData) previousSearchPatterns.get(i)).getPattern();
        return patterns;
    }

    private int getIntData(Button button) {
        return ((Integer) button.getData()).intValue();
    }

    private SearchPatternData findInPrevious(String pattern) {
        for (SearchPatternData element : previousSearchPatterns) {
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
    private SearchPatternData getPatternDataAndUpdatePatternHistory() {
        String pattern = getPattern();
        SearchPatternData match = findInPrevious(pattern);
        if (match != null) {
            previousSearchPatterns.remove(match);
        }
        match = new SearchPatternData(
                getSearchFor(),
                getLimitTo(),
                pattern,
                isExactMatch()
        );

        previousSearchPatterns.add(0, match); // insert on top
        return match;
    }

    /*
     * Implements method from IDialogPage
     */
    @Override
    public void setVisible(boolean visible) {
        if (visible && patternList != null) {
            if (firstSetVisibleCall) {
                firstSetVisibleCall = false;
                // Set item and text here to prevent page from resizing
                patternList.setItems(getPreviousSearchPatterns());
                initSelections();
            }
            patternList.setFocus();
        }
        updateOKStatus();
        super.setVisible(visible);
    }

    //---- Widget creation ------------------------------------------------


    /* (non-Javadoc)
     * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public void createControl(Composite parent) {
        initializeDialogUnits(parent);
        readConfiguration();

        Composite result = new Composite(parent, SWT.NONE);

        GridLayout layout = new GridLayout(2, false);
        layout.horizontalSpacing = 10;
        result.setLayout(layout);

        Control expressionComposite = createExpression(result);
        expressionComposite.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));

        explainingLabel = new Label(result, SWT.WRAP);
        GridData labelLayoutData = new GridData(SWT.FILL, SWT.BEGINNING, true, false, 2, 1);
        labelLayoutData.heightHint = convertHeightInCharsToPixels(2);
		explainingLabel.setLayoutData(labelLayoutData);

        Label separator = new Label(result, SWT.NONE);
        separator.setVisible(false);
        GridData data = new GridData(GridData.FILL, GridData.FILL, false, false, 2, 1);
        data.heightHint = convertHeightInCharsToPixels(1) / 3;
        separator.setLayoutData(data);

        Control searchFor = createSearchFor(result);
        searchFor.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));

        Control limitTo = createLimitTo(result);
        limitTo.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, 1, 1));

        setControl(result);

        Dialog.applyDialogFont(result);
    }

    private Control createExpression(Composite parent) {
        Composite result = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(2, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        result.setLayout(layout);

        // Pattern text + info
        Label label = new Label(result, SWT.LEFT);
        label.setText("Search String");
        label.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false, 2, 1));

        // Pattern combo
        patternList = new Combo(result, SWT.SINGLE | SWT.BORDER);
        patternList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handlePatternSelected();
                updateOKStatus();
            }
        });
        patternList.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                updateOKStatus();

            }
        });
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, false, 1, 1);
        data.widthHint = convertWidthInCharsToPixels(50);
        patternList.setLayoutData(data);

        exactMatchCheckBox = createButton(result, SWT.CHECK, "Exact matches only", 0, true);
        exactMatchCheckBox.setLayoutData(new GridData(SWT.END, SWT.CENTER, false, false));

        return result;
    }

    private Control createSearchFor(Composite parent) {
        Group result = new Group(parent, SWT.NONE);
        result.setText("Search For");
        result.setLayout(new GridLayout(2, false));

        searchForRadioButtons = new Button[] {
                createButton(result, SWT.RADIO, "Entity", MODULE, false),
                createButton(result, SWT.RADIO, "Predicate", PREDICATE, true),
                createButton(result, SWT.RADIO, "Undefined Call", UNDEFINED_CALL, false),
                createButton(result, SWT.RADIO, "Dead Predicate", DEAD_PREDICATE, false)
        };
        
        for (Button button : searchForRadioButtons) {
        	button.addSelectionListener(new SelectionAdapter() {
				@Override
        		public void widgetSelected(SelectionEvent e) {
					if (e.getSource() instanceof Button) {
						updateLabelAndEnablement(getIntData((Button) e.getSource()), getLimitTo());
						updateOKStatus();
					}
				}
			});
        }

        return result;
    }

    private Control createLimitTo(Composite parent) {
        Group result = new Group(parent, SWT.NONE);
        result.setText("Limit To");
        result.setLayout(new GridLayout(2, false));

        limitToRadioButtons = new Button[]{
                createButton(result, SWT.RADIO, "Declarations && Definitions", DECLARATIONS, true),
                createButton(result, SWT.RADIO, "References", REFERENCES, false)
        };
        for (Button button : limitToRadioButtons) {
        	button.addSelectionListener(new SelectionAdapter() {
				@Override
        		public void widgetSelected(SelectionEvent e) {
					if (e.getSource() instanceof Button) {
						updateLabelAndEnablement(getSearchFor(), getIntData((Button) e.getSource()));
						updateOKStatus();
					}
				}
			});
        }

        return result;
    }

    private Button createButton(Composite parent, int style, String text, int data, boolean isSelected) {
        Button button = new Button(parent, style);
        button.setText(text);
        button.setData(new Integer(data));
        button.setLayoutData(new GridData(SWT.BEGINNING, SWT.CENTER, false, false));
        button.setSelection(isSelected);
        return button;
    }

    final void updateOKStatus() {
        boolean isValid = isValidSearchPattern();
        getContainer().setPerformActionEnabled(isValid);
    }

    private boolean isValidSearchPattern() {
        String pattern = getPattern();
        if (pattern.length() == 0) {
            return false;
        } else if (getSearchFor() == MODULE) {
        	return true;
        }
        return getFunctorAndArity(pattern) != null;
    }


    /* (non-Javadoc)
     * @see org.eclipse.jface.dialogs.DialogPage#dispose()
     */
    @Override
    public void dispose() {
        writeConfiguration();
        super.dispose();
    }

    private void handlePatternSelected() {
        int selectionIndex = patternList.getSelectionIndex();
        if (selectionIndex < 0 || selectionIndex >= previousSearchPatterns.size())
            return;

        SearchPatternData data = previousSearchPatterns.get(selectionIndex);
        applySearchPatternData(data);
    }

    private void applySearchPatternData(SearchPatternData data) {
        setPattern(data.getPattern());
        setSearchFor(data.getSearchFor());
        setLimitTo(data.getLimitTo());
        setExactMatch(data.isExactMatch());
    }

    private void initSelections() {
        ISelection sel = getContainer().getSelection();

        SearchPatternData initData = null;

        if (sel instanceof ITextSelection) {
            if (initData == null) {
                initData = trySimpleTextSelection((ITextSelection) sel);
            }
        }
        if (initData == null) {
            initData = getDefaultInitValues();
        }

        applySearchPatternData(initData);
    }

    private SearchPatternData trySimpleTextSelection(ITextSelection selection) {
        String selectedText = selection.getText();
        if (selectedText != null && selectedText.length() > 0) {
            int i = 0;
            while (i < selectedText.length() && !IndentManipulation.isLineDelimiterChar(selectedText.charAt(i))) {
                i++;
            }
            if (i > 0) {
                return new SearchPatternData(PREDICATE, DECLARATIONS, selectedText.substring(0, i),true);
            }
        }
        return null;
    }

    private SearchPatternData getDefaultInitValues() {
        if (!previousSearchPatterns.isEmpty()) {
            return (SearchPatternData) previousSearchPatterns.get(0);
        }
        return new SearchPatternData(PREDICATE, DECLARATIONS, "", true);
    }

    /*
     * Implements method from ISearchPage
     */
    @Override
    public void setContainer(ISearchPageContainer container) {
        searchPageContainer = container;
    }

    /**
     * Returns the search page's container.
     * @return the search page container
     */
    private ISearchPageContainer getContainer() {
        return searchPageContainer;
    }

    //--------------- Configuration handling --------------

    /**
     * Returns the page settings for this Java search page.
     *
     * @return the page settings to be used
     */
    private IDialogSettings getDialogSettings() {
        if (dialogSettings == null) {
            dialogSettings = PDTCommonPlugin.getDefault().getDialogSettingsSection(PAGE_NAME);
        }
        return dialogSettings;
    }

    /**
     * Initializes itself from the stored page settings.
     */
    private void readConfiguration() {
        IDialogSettings s = getDialogSettings();

        try {
            int historySize = s.getInt(STORE_HISTORY_SIZE);
            for (int i = 0; i < historySize; i++) {
                IDialogSettings histSettings = s.getSection(STORE_HISTORY + i);
                if (histSettings != null) {
                    SearchPatternData data = SearchPatternData.create(histSettings);
                    if (data != null) {
                        previousSearchPatterns.add(data);
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
        IDialogSettings s = getDialogSettings();

        int historySize = Math.min(previousSearchPatterns.size(), HISTORY_SIZE);
        s.put(STORE_HISTORY_SIZE, historySize);
        for (int i = 0; i < historySize; i++) {
            IDialogSettings histSettings = s.addNewSection(STORE_HISTORY + i);
            SearchPatternData data = ((SearchPatternData) previousSearchPatterns.get(i));
            data.store(histSettings);
        }
    }
}


