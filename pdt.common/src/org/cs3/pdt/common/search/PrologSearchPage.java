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

import static org.cs3.prolog.common.QueryUtils.bT;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.queries.DeadPredicatesSearchQuery;
import org.cs3.pdt.common.queries.GlobalDefinitionsSearchQuery;
import org.cs3.pdt.common.queries.MetaPredicatesSearchQuery;
import org.cs3.pdt.common.queries.ModuleDefinitionsSearchQuery;
import org.cs3.pdt.common.queries.ModuleReferenceSearchQuery;
import org.cs3.pdt.common.queries.PDTSearchQuery;
import org.cs3.pdt.common.queries.ReferencesSearchQueryDirect;
import org.cs3.pdt.common.queries.UndefinedCallsSearchQuery;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.ui.util.ExternalPrologFilesProjectUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
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
    private static final String UNDEFINED_HINT = "Search for all undefined calls";
    private static final String DEAD_HINT = "Search for all predicates unreachable from entry points";
    private static final String META_HINT = "Search for all undeclared meta predicates";
    
    private static final String[][] hints = new String[][]{
    	{ENTITY_DEF_HINT, ENTITY_REF_HINT},
    	{PREDICATE_DEF_HINT, PREDICATE_REF_HINT},
    	{UNDEFINED_HINT, UNDEFINED_HINT},
    	{DEAD_HINT, DEAD_HINT},
    	{META_HINT, META_HINT}
    };
    
    private static final String PROJECT_SCOPE = "projectScope";
    private static final String LAST_PROJECT = "lastProject";
    private static final String LAST_CREATE_MARKERS = "lastCreateMarkers";
    
	private static class SearchPatternData {
        private static final String SEARCH_FOR = "searchFor";
        private static final String PATTERN = "pattern";
        private static final String LIMIT_TO = "limitTo";
        private static final String EXACT_MATCH = "exactMatch";

        public final int searchFor;
        public final int limitTo;
        public final String pattern;
        public final boolean exactMatch;
        public final boolean createMarkers;
        private final boolean projectScope;
        public final String project;

        public SearchPatternData(int searchFor, int limitTo, String pattern, boolean exactMatch) {
        	this(searchFor, limitTo, pattern, exactMatch, true, true, "");
        }
        
        public SearchPatternData(int searchFor, int limitTo, String pattern, boolean exactMatch, boolean createMarkers, boolean projectScope, String project) {
            this.searchFor = searchFor;
            this.limitTo = limitTo;
            this.pattern = pattern;
            this.exactMatch = exactMatch;
            this.createMarkers = createMarkers;
            this.projectScope = projectScope;
            this.project = project;
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
        
        public boolean isCreateMarkers() {
        	return createMarkers;
        }
        
        public boolean isProjectScope() {
        	return projectScope;
        }
        
        public String getProject() {
        	return project;
        }

        public void store(IDialogSettings settings) {
            settings.put(SEARCH_FOR, searchFor);
            settings.put(PATTERN, pattern);
            settings.put(LIMIT_TO, limitTo);
            settings.put(EXACT_MATCH, exactMatch);
        }

        public static SearchPatternData create(IDialogSettings settings) {
            String pattern = settings.get(PATTERN);
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
    private final static int META_PREDICATE = 4;

    // limit to
    private final static int DECLARATIONS = 0;
    private final static int REFERENCES = 1;
    
    public static final String EXTENSION_POINT_ID = "org.cs3.pdt.prologSearchPage";

    private static final int HISTORY_SIZE = 12;

    // Dialog store id constants
    private final static String PAGE_NAME = "PrologSearchPage";
    private final static String STORE_HISTORY = "HISTORY";
    private final static String STORE_HISTORY_SIZE = "HISTORY_SIZE";
	private static final int NUMBER_OF_SEARCH_FOR = 5;
	private static final int NUMBER_OF_LIMIT_TO = 2;

    private final List<SearchPatternData> previousSearchPatterns = new ArrayList<SearchPatternData>();

    private boolean firstSetVisibleCall = true;
    private IDialogSettings dialogSettings;

    private Combo patternList;
    private ISearchPageContainer searchPageContainer;

    private Button[] searchForRadioButtons;
    private Button[] limitToRadioButtons;
    private Button exactMatchCheckBox;
    private Button createMarkersCheckBox;
	private Label explainingLabel;
	private Button projectScopeCheckBox;
	private Combo projectSelector;
	private String lastProject;
	private boolean lastProjectScope;
	private boolean lastCreateMarkers;

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

//        Goal goal;
        if (searchFor == PREDICATE) {
        	SearchPattern searchPattern = SearchPattern.createSearchPattern(data.pattern);
        	String searchGoal = searchPattern.toSearchGoal();
//            goal = new Goal("", searchPattern.module, Util.quoteAtom(searchPattern.name), searchPattern.arity, null, data.isExactMatch());
            
            if (limitTo == REFERENCES) {
            	searchQuery = new ReferencesSearchQueryDirect(searchGoal, data.pattern, data.isExactMatch());
            } else {
            	searchQuery = new GlobalDefinitionsSearchQuery(searchGoal, data.pattern, data.isExactMatch());
            }
        } else if (searchFor == MODULE) {
//        	boolean exactMatch = data.isExactMatch();
//			goal = new Goal("", data.pattern, "", 0, data.pattern, exactMatch);
//            
            if (limitTo == REFERENCES) {
            	searchQuery = new ModuleReferenceSearchQuery(Util.quoteAtom(data.pattern), data.pattern, data.isExactMatch());
            } else {
            	searchQuery = new ModuleDefinitionsSearchQuery(Util.quoteAtom(data.pattern), data.pattern, data.isExactMatch());
            }
        } else if (searchFor == UNDEFINED_CALL) {
//        	searchQuery = new UndefinedCallsSearchQuery(data.isCreateMarkers());
        	if (data.isProjectScope()) {
        		IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(data.getProject());
        		if (project.isAccessible()) {
        			searchQuery = new UndefinedCallsSearchQuery(data.isCreateMarkers(), project);
        		} else {
        			Debug.error(project.getName() + " is not accessible.");
        		}
        	} else {
        		searchQuery = new UndefinedCallsSearchQuery(data.isCreateMarkers());
        	}
        } else if (searchFor == DEAD_PREDICATE) {
        	if (data.isProjectScope()) {
        		IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(data.getProject());
        		if (project.isAccessible()) {
        			searchQuery = new DeadPredicatesSearchQuery(data.isCreateMarkers(), project);
        		} else {
        			Debug.error(project.getName() + " is not accessible.");
        		}
        	} else {
        		searchQuery = new DeadPredicatesSearchQuery(data.isCreateMarkers());
        	}
        } else if (searchFor == META_PREDICATE) {
        	if (data.isProjectScope()) {
        		IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(data.getProject());
        		if (project.isAccessible()) {
        			searchQuery = new MetaPredicatesSearchQuery(data.isCreateMarkers(), project);
        		} else {
        			Debug.error(project.getName() + " is not accessible.");
        		}
        	} else {
        		searchQuery = new MetaPredicatesSearchQuery(data.isCreateMarkers());
        	}
//        	searchQuery = new MetaPredicatesSearchQuery(data.isCreateMarkers());
        }
        
        if (searchQuery != null) {
        	NewSearchUI.activateSearchResultView();
        	NewSearchUI.runQueryInForeground(null,searchQuery);
        }
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

    private boolean isCreateMarkers() {
    	return createMarkersCheckBox.getSelection();
    }
    
    private String getProject() {
		return projectSelector.getText();
    }
    
    private boolean isProjectScope() {
    	return projectScopeCheckBox.getSelection();
    }
   
//    private SearchPattern getSearchPattern(String pattern) {
//        if (pattern == null || pattern.isEmpty()) {
//            return null;
//        }
//        String module = null;
//        String mSeparator;
//        String functor = "";
//        String aSeparator = null;
//        int arity = 0;
//        
//        String beforeArity;
//        int lastSlash = pattern.lastIndexOf("//");
//        if (lastSlash == -1) {
//        	lastSlash = pattern.lastIndexOf("/");
//        	if (lastSlash != -1) {
//        		aSeparator = "/";
//        	}
//        } else {
//        	aSeparator = "//";
//        }
//        if (lastSlash == -1) {
//        	beforeArity = pattern;
//            arity = -1;
//        } else {
//            try {
//                arity = Integer.parseInt(pattern.substring(lastSlash + aSeparator.length()));
//            } catch (NumberFormatException e) {
//                return null;
//            }
//            if (arity < 0) {
//                return null;
//            }
//            beforeArity = pattern.substring(0, lastSlash);
//        }
//        if (beforeArity.isEmpty()) {
//        	return null;
//        }
//        int firstSplitter = beforeArity.indexOf("::");
//        int splitterLength = 2;
//        if (firstSplitter == -1) {
//        	firstSplitter = beforeArity.indexOf(":");
//        	splitterLength = 1;
//        }
//        if (firstSplitter == -1) {
//        	functor = beforeArity;
//        } else {
//        	module = beforeArity.substring(0, firstSplitter);
//        	functor = beforeArity.substring(firstSplitter + splitterLength);
//        	if (module.isEmpty() || functor.isEmpty()) {
//        		return null;
//        	}
//        }
//        return new Object[]{module, functor, arity};
//    }
    
    private static class SearchPattern {
    	private static final Pattern p = Pattern.compile("(([^:]+)(:|::))?([^:/]+)((/|//)(\\d+))?");
    	
    	private static SearchPattern createSearchPattern(String pattern) {
    		Matcher matcher = p.matcher(pattern);
    		if (matcher.matches()) {
    			return new SearchPattern(matcher.group(2), matcher.group(3), matcher.group(4), matcher.group(6), matcher.group(7));
    		}
    		return null;
    	}
    	
    	final String module;
    	final String mSeparator;
    	final String name;
    	final String aSeparator;
    	final int arity;
    	
    	private SearchPattern(String module, String mSeparator, String name, String aSeparator, String arity) {
			super();
			this.module = module;
			this.mSeparator = mSeparator;
			this.name = name;
			this.aSeparator = aSeparator;
			if (arity == null) {
				this.arity = -1;
			} else {
				int parseInt = -1;
				try {
					parseInt = Integer.parseInt(arity);
				} catch (NumberFormatException e) {
				}
				this.arity = parseInt;
			}
		}
    	
    	public String toSearchGoal() {
    		return bT(SearchConstants.PREDICATE_GOAL_FUNCTOR, 
    				module != null ? Util.quoteAtomIfNeeded(module) : "_",
    				mSeparator != null ? mSeparator : "_",
    				Util.quoteAtomIfNeeded(name),
    				aSeparator != null ? aSeparator : "_",
    				arity >= 0 ? arity : "_");
    	}
    	
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
    
    private void setProject(String project) {
    	if (project != null) {
    		projectSelector.setText(project);
    	}
    	if (projectSelector.getText().isEmpty() && projectSelector.getItemCount() > 0) {
    		projectSelector.select(0);
    	}
    }
    
    private void setProjectScope(boolean projectScope) {
    	projectScopeCheckBox.setSelection(projectScope);
    }

	private void updateLabelAndEnablement(int searchFor, int limitTo) {
		if (searchFor >= 0 && searchFor < NUMBER_OF_SEARCH_FOR && limitTo >= 0 && limitTo < NUMBER_OF_LIMIT_TO) {
        	explainingLabel.setText(hints[searchFor][limitTo]);
    		setSearchFieldsEnablement(searchFor);
        }
	}
	
	private void setSearchFieldsEnablement(int searchFor) {
		boolean doPatternSearch = searchFor < 2;
		patternList.setEnabled(doPatternSearch);
		exactMatchCheckBox.setEnabled(doPatternSearch);
		for (Button limitToButton : limitToRadioButtons) {
			limitToButton.setEnabled(doPatternSearch);
		}
		createMarkersCheckBox.setEnabled(!doPatternSearch);
		projectScopeCheckBox.setEnabled(!doPatternSearch);
		projectSelector.setEnabled(!doPatternSearch && projectScopeCheckBox.getSelection());
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

    private void setCreateMarkers(boolean createMarkers) {
    	createMarkersCheckBox.setSelection(createMarkers);
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
        int searchFor = getSearchFor();
        SearchPatternData data = new SearchPatternData(
        		searchFor,
        		getLimitTo(),
        		pattern,
        		isExactMatch(),
        		isCreateMarkers(),
        		isProjectScope(),
        		getProject()
        		);
    	if (searchFor < 2) {
    		// search with pattern
    		SearchPatternData match = findInPrevious(pattern);
    		if (match != null) {
    			previousSearchPatterns.remove(match);
    		}
    		previousSearchPatterns.add(0, data); // insert on top
    	}
        return data;
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
        GridData data = new GridData(SWT.FILL, SWT.FILL, false, false, 2, 1);
        data.heightHint = convertHeightInCharsToPixels(1) / 3;
        separator.setLayoutData(data);

        Control searchFor = createSearchFor(result);
        searchFor.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false, 1, 1));

        Control limitTo = createLimitTo(result);
        limitTo.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true, false, 1, 1));

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
        Composite container = new Composite(parent, SWT.NONE);
        container.setLayout(new GridLayout(1, true));
    	
        Group patternSearch = new Group(container, SWT.NONE);
        patternSearch.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        patternSearch.setText("Search For");
        patternSearch.setLayout(new GridLayout(2, false));

    	Group globalSearch = new Group(container, SWT.NONE);
    	globalSearch.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
    	globalSearch.setText("Search Globally For");
    	globalSearch.setLayout(new GridLayout(2, false));
        
        searchForRadioButtons = new Button[] {
                createButton(patternSearch, SWT.RADIO, "Entity", MODULE, false),
                createButton(patternSearch, SWT.RADIO, "Predicate", PREDICATE, true),
                createButton(globalSearch, SWT.RADIO, "Undefined Call", UNDEFINED_CALL, false),
                createButton(globalSearch, SWT.RADIO, "Dead Predicate", DEAD_PREDICATE, false),
                createButton(globalSearch, SWT.RADIO, "Undeclared Meta Predicate", META_PREDICATE, false)
        };
        createMarkersCheckBox = createButton(globalSearch, SWT.CHECK, "Create Warnings", -1, true);
        
        for (Button button : searchForRadioButtons) {
        	button.addSelectionListener(new SelectionAdapter() {
				@Override
        		public void widgetSelected(SelectionEvent e) {
					if (e.getSource() instanceof Button) {
						Button button2 = (Button) e.getSource();
						for (Button button3 : searchForRadioButtons) {
							button3.setSelection(false);
						}
						button2.setSelection(true);
						updateLabelAndEnablement(getIntData(button2), getLimitTo());
						updateOKStatus();
					}
				}
			});
        }

        return container;
    }

    private Control createLimitTo(Composite parent) {
        Composite container = new Composite(parent, SWT.NONE);
        container.setLayout(new GridLayout(1, true));

        Group result = new Group(container, SWT.NONE);
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
        
        Group scope = new Group(container, SWT.NONE);
        scope.setText("Scope");
        scope.setLayout(new GridLayout(2, false));
        
        projectScopeCheckBox = createButton(scope, SWT.CHECK, "Project", -1, false);
        projectScopeCheckBox.addSelectionListener(new SelectionAdapter() {
			@Override
    		public void widgetSelected(SelectionEvent e) {
				projectSelector.setEnabled(projectScopeCheckBox.getSelection());
			}
		});
        projectSelector = new Combo(scope, SWT.READ_ONLY);
        fillProjectList(projectSelector);
        return container;
    }
    
    private void fillProjectList(Combo combo) {
    	TreeSet<String> projectNames = new TreeSet<String>(String.CASE_INSENSITIVE_ORDER);
    	
    	IProject externalPrologFiles = null;
    	try {
			externalPrologFiles = ExternalPrologFilesProjectUtils.getExternalPrologFilesProject();
		} catch (CoreException e) {
			Debug.report(e);
		}
    	for (IProject project : ResourcesPlugin.getWorkspace().getRoot().getProjects()) {
    		if (project.isAccessible() && !project.equals(externalPrologFiles)) {
    			projectNames.add(project.getName());
    		}
    	}
    	for (String projectName : projectNames) {
    		combo.add(projectName);
    	}
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
        if (getSearchFor() >= 2) {
        	return true;
        }
    	String pattern = getPattern();
        if (pattern.length() == 0) {
            return false;
        } else if (getSearchFor() == MODULE) {
        	return true;
        }
        return SearchPattern.createSearchPattern(pattern) != null;
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
    	setProject(lastProject);
		setProjectScope(lastProjectScope);
		setCreateMarkers(lastCreateMarkers);
    }

    private SearchPatternData trySimpleTextSelection(ITextSelection selection) {
        String selectedText = selection.getText();
        if (selectedText != null && selectedText.length() > 0) {
            int i = 0;
            while (i < selectedText.length() && !IndentManipulation.isLineDelimiterChar(selectedText.charAt(i))) {
                i++;
            }
            if (i > 0) {
                return new SearchPatternData(PREDICATE, DECLARATIONS, selectedText.substring(0, i), true);
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
        	lastProject = s.get(LAST_PROJECT);
        	lastProjectScope = !s.getBoolean(PROJECT_SCOPE);
        	lastCreateMarkers = s.getBoolean(LAST_CREATE_MARKERS);
        	
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
    	s.put(LAST_PROJECT, getProject());
        s.put(PROJECT_SCOPE, !isProjectScope());
        s.put(LAST_CREATE_MARKERS, isCreateMarkers());

        int historySize = Math.min(previousSearchPatterns.size(), HISTORY_SIZE);
        s.put(STORE_HISTORY_SIZE, historySize);
        for (int i = 0; i < historySize; i++) {
            IDialogSettings histSettings = s.addNewSection(STORE_HISTORY + i);
            SearchPatternData data = ((SearchPatternData) previousSearchPatterns.get(i));
            data.store(histSettings);
        }
    }
}


