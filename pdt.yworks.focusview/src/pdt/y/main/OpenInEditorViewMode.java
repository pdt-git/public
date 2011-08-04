package pdt.y.main;

import java.awt.event.MouseEvent;
import java.io.FileNotFoundException;
import java.util.Map;

import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PartInitException;

import pdt.y.model.GraphDataHolder;
import y.base.Node;
import y.view.ViewMode;

public class OpenInEditorViewMode extends ViewMode {

	private PDTGraphSwingStandalone view;
	private GraphPIFCoordinator pifCoordinator;

	public OpenInEditorViewMode(PDTGraphSwingStandalone view, GraphPIFCoordinator pifCoordinator) {
		this.view = view;
		this.pifCoordinator = pifCoordinator;
	}

	@Override
	public void mouseClicked(MouseEvent event) {
		if(event.getClickCount() >= 2) {

			// Retrieve the node that has been hit at the location.
			Node node = getHitInfo(event).getHitNode();

			if (node == null)
				return;
			GraphDataHolder dataHolder = view.getDataHolder();
			if (!dataHolder.isPredicate(node))
				return;
			String idInt = dataHolder.getNodeText(node);

			String query = "predicateT("+idInt+",FileId,_,_,_),fileT(FileId,FileName,_),slT("+idInt+",Pos,Len).";
			Map<String,Object> result = null;
			try {
				result = pifCoordinator.sendQueryToCurrentPiF(query);
			} catch (PrologInterfaceException e1) {
				e1.printStackTrace();
			}

			if(result==null)
				return;

			final String filename = (String) result.get("FileName");
			final int start = Integer.parseInt((String) result.get("Pos"));
			final int length = Integer.parseInt((String) result.get("Len"));


			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					try {
						PDTCoreUtils.selectInEditor(start, length, filename);
					} catch (PartInitException e) {
						e.printStackTrace();
					} catch (FileNotFoundException e) {
						e.printStackTrace();
					}
				}
			});
		}
	}
}
