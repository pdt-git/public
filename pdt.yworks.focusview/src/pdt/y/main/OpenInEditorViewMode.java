package pdt.y.main;

import java.awt.event.MouseEvent;
import java.io.FileNotFoundException;
import java.util.Map;

import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.eclipse.ui.PartInitException;

import pdt.y.model.GraphDataHolder;
import y.base.Node;
import y.view.ViewMode;

public class OpenInEditorViewMode extends ViewMode {

	private GraphDataHolder graphDataHolder;
	private GraphPIFCoordinator pifCoordinator;

	public OpenInEditorViewMode(GraphDataHolder graphDataholder, GraphPIFCoordinator pifCoordinator) {
		graphDataHolder = graphDataholder;
		this.pifCoordinator = pifCoordinator;
	}

	@Override
	public void mouseClicked(MouseEvent event) {

		// Retrieve the node that has been hit at the location.
		Node node = getHitInfo(event).getHitNode();

		if (node == null)
			return;

		if (!graphDataHolder.isPredicate(node))
			return;
		int id = node.index();
		System.out.println(id);

		String query = "slT("+id+",Pos,Len),predicateT("+id+",FileId,_,_,_),fileT(FileT,FileName,_).";
		Map<String,Object> result = null;
		try {
			result = pifCoordinator.sendQueryToCurrentPiF(query);
		} catch (PrologInterfaceException e1) {
			e1.printStackTrace();
		}
		String filename = "";
		int start=0;
		int length=0;

		if(result!=null) {
			filename = (String) result.get("FileName");
			start = Integer.parseInt((String) result.get("Pos"));
			length = Integer.parseInt((String) result.get("Len"));
		}

		try {
			UIUtils.selectInEditor(start, length, filename);
		} catch (PartInitException e) {
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}
}
