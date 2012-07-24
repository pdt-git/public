/* $LICENSE_MSG$(ld) */

/*
 * Created on 23.08.2004
 *
 */
package org.cs3.pdt.internal.search;

import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.internal.structureElements.PrologMatch;
import org.cs3.pdt.internal.structureElements.SearchFileTreeElement;
import org.cs3.pdt.internal.structureElements.SearchPredicateElement;
import org.cs3.pdt.metadata.SourceLocation;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.ColumnViewerToolTipSupport;
import org.eclipse.jface.viewers.DecoratingLabelProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeViewerListener;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeExpansionEvent;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.ToolTip;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.search.ui.text.AbstractTextSearchViewPage;
import org.eclipse.search.ui.text.Match;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;


public class PrologSearchResultPage extends AbstractTextSearchViewPage {

	private PrologSearchContentProvider fContentProvider;
	protected static final Image IMAGE = ImageRepository.getImage(ImageRepository.PE_PUBLIC);

	public PrologSearchResultPage(){
		super(AbstractTextSearchViewPage.FLAG_LAYOUT_TREE);
		init(NewSearchUI.getSearchResultView().getActivePage().getSite());
	}
	
	@Override
	protected void elementsChanged(Object[] objects) {
		if (fContentProvider != null)
			fContentProvider.elementsChanged(objects);
		StructuredViewer viewer = getViewer();
		if(viewer!=null){
		    viewer.refresh();
		}
	}

	@Override
	protected void clear() {
		if (fContentProvider != null)
			fContentProvider.clear();
		StructuredViewer viewer = getViewer();
		if(viewer!=null){
		    viewer.refresh();
		}
	}

	@Override
	protected void configureTreeViewer(TreeViewer viewer) {
		viewer.setLabelProvider(new DecoratingPrologSearchLabelProvider(new PrologSearchLabelProvider()));
		ColumnViewerToolTipSupport.enableFor(viewer, ToolTip.NO_RECREATE);
//		viewer.setLabelProvider(new DecoratingLabelProvider(new PrologSearchLabelProvider(), 
//				PlatformUI.getWorkbench().getDecoratorManager().getLabelDecorator()));
		fContentProvider= new PrologSearchTreeContentProvider(this);
		viewer.setContentProvider(fContentProvider);
		viewer.addDoubleClickListener(new IDoubleClickListener() {
			@Override
			public void doubleClick(DoubleClickEvent event) {
				ISelection selection = event.getSelection();
				if (selection instanceof TreeSelection) {
					Object firstElement = ((TreeSelection) selection).getFirstElement();
					Match m = null;
					if (firstElement instanceof SearchPredicateElement) {
						m = ((SearchPredicateElement) firstElement).getFirstOccurrence();
					} else if (firstElement instanceof SearchFileTreeElement) {
						m = ((SearchFileTreeElement) firstElement).getFirstMatch();
					}
					if (m != null) {
						final Match match = m;
						UIJob job = new UIJob("Show Match") {
							@Override
							public IStatus runInUIThread(IProgressMonitor monitor) {
								try {
									showMatch(match, match.getOffset(), match.getLength(), true);
								} catch (PartInitException e) {
									Debug.report(e);
								}
								return Status.OK_STATUS;
							}
						};
						job.schedule();
					}
				}
			}
		});
		viewer.addTreeListener(new ITreeViewerListener() {
			@Override
			public void treeExpanded(TreeExpansionEvent event) {
				final AbstractTreeViewer treeViewer = event.getTreeViewer();
				PrologSearchTreeContentProvider contentProvider = (PrologSearchTreeContentProvider) treeViewer.getContentProvider();
				final Object element = event.getElement();
				int expandLevel = 1;
				Object[] children = contentProvider.getChildren(element);
				while (children.length == 1) {
					expandLevel++;
					children = contentProvider.getChildren(children[0]);
				}
				if (expandLevel > 1) {
					final int finalExpandLevel = expandLevel;
					treeViewer.getControl().getDisplay().asyncExec(new Runnable() {
						@Override
						public void run() {
							treeViewer.expandToLevel(element, finalExpandLevel);
						}
					});
				}
			}
			
			@Override
			public void treeCollapsed(TreeExpansionEvent event) {}
		});
	}
	
	@Override
	public StructuredViewer getViewer() {		
		return super.getViewer();
	}

	@Override
	protected void configureTableViewer(TableViewer viewer) {
		viewer.setLabelProvider(new DecoratingLabelProvider(new PrologSearchLabelProvider(), 
				PlatformUI.getWorkbench().getDecoratorManager().getLabelDecorator()));
		fContentProvider= new PrologSearchTableContentProvider(this);
		viewer.setContentProvider(fContentProvider);
	}
	
	@Override
	protected void showMatch(Match match, int currentOffset, int currentLength, boolean activate) throws PartInitException {
		PrologMatch prologMatch = (PrologMatch)match;
		SearchPredicateElement element = prologMatch.getPredicateElement();
		IFile file = prologMatch.getFile();
		if(prologMatch.isLineLocation()) {
			SourceLocation loc = createLocation(element, file, prologMatch);
			PDTUtils.showSourceLocation(loc);
			return;
		}
		UIUtils.selectInPrologEditor(currentOffset, currentLength, file, activate);
	}

	private SourceLocation createLocation(SearchPredicateElement element, IFile file, PrologMatch prologMatch) {
		SourceLocation loc = new SourceLocation(file.getRawLocation().toPortableString(), false);
		loc.isWorkspacePath = file.isAccessible();
		
		loc.setLine(prologMatch.getLine());
		loc.setPredicateName(element.getFunctor());
		loc.setArity(element.getArity());
		return loc;
	}

}

