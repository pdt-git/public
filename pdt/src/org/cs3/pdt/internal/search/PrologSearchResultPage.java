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
import org.eclipse.jface.viewers.ColumnViewerToolTipSupport;
import org.eclipse.jface.viewers.DecoratingLabelProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TableViewer;
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