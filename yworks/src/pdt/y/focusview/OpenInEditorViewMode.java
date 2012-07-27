package pdt.y.focusview;

import java.awt.event.MouseEvent;
import java.util.Map;

import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PartInitException;

import pdt.y.main.PDTGraphView;
import pdt.y.model.GraphDataHolder;
import y.base.Node;
import y.view.ViewMode;

public class OpenInEditorViewMode extends ViewMode {

	private PDTGraphView view;
	private GraphPIFLoader pifLoader;

	public OpenInEditorViewMode(PDTGraphView view, GraphPIFLoader pifLoader) {
		this.view = view;
		this.pifLoader = pifLoader;
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

			String query = "parse_util:predicateT("+idInt+",FileId,_,_,_),parse_util:fileT(FileId,FileName,_),parse_util:filePosT("+idInt+",Pos,Len).";
			Map<String,Object> result = null;
			try {
				result = pifLoader.sendQueryToCurrentPiF(query);
			} catch (PrologInterfaceException e1) {
				e1.printStackTrace();
			}

			if(result==null)
				return;

			final String filename = result.get("FileName").toString();
			final int start = Integer.parseInt(result.get("Pos").toString());
			final int length = Integer.parseInt(result.get("Len").toString());

			//			ExecutorService executor = Executors.newCachedThreadPool();
			//			FutureTask<String> futureParser = new FutureTask<String>(new Runnable() {
			//				@Override
			//				public void run() {
			//					try {
			//						//Display.getDefault().
			//						PDTCoreUtils.selectInEditor(start, length, filename);
			//					} catch (Exception e) {
			//
			//					}
			//				}
			//			},null);
			//
			//
			//			executor.execute(futureParser);
			//
			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					try {
						UIUtils.selectInPrologEditor(start, length, filename);
					} catch (PartInitException e) {
						e.printStackTrace();
					}
				}
			});
		}
	}
}
