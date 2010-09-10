package pdt.y.view.modes;

import java.awt.event.MouseEvent;

import y.base.Node;
import y.view.Graph2D;
import y.view.Graph2DViewActions;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.ProxyShapeNodeRealizer;
import y.view.ViewMode;
import y.view.hierarchy.GroupNodeRealizer;
import y.view.hierarchy.HierarchyManager;

public class ToggleOpenClosedStateViewMode extends ViewMode {  
  HierarchyManager hierarchyManager;  

  @Override
public void mouseClicked(MouseEvent event) {  
    // Convert the mouse event's coordinates from view to world coordinates.   
    double x = translateX(event.getX());  
    double y = translateY(event.getY());  
      
    // Retrieve the node that has been hit at the location.   
    Node node = getHitInfo(event).getHitNode();  
    
    if(node == null)
    	return;
    
    // Test if the node is rendered by GroupNodeRealizer.   
    GroupNodeRealizer groupNodeRealizer = getGroupNodeRealizer(node);
  
    if(groupNodeRealizer == null)
    	return;
    
    // Get the state label.   
    NodeLabel stateLabel = groupNodeRealizer.getStateLabel();  
    // Test, if the mouse event occurred on the state icon.   
    if (stateLabel.getBox().contains(x, y)) {  
    	// Retrieve the HierarchyManager of the hierarchically organized graph.   
    	hierarchyManager = HierarchyManager.getInstance(view.getGraph2D());  

    	if (hierarchyManager.isFolderNode(node)) {  
    		// Invokes a customized OpenFolders Action implementation.  
    		new Graph2DViewActions.OpenFoldersAction().openFolders(view);  
    	}  
    	else {  
    		// Invokes a customized CloseGroups Action implementation.  
    		new Graph2DViewActions.CloseGroupsAction().closeGroups(view);  
    	}  
    }  
      
  }

  private GroupNodeRealizer getGroupNodeRealizer(Node node) {
	  GroupNodeRealizer groupNodeRealizer = null;
	  Graph2D graph =  view.getGraph2D();

	  NodeRealizer nodeRealizer = graph.getRealizer(node);
	  if (nodeRealizer instanceof GroupNodeRealizer) {
		  groupNodeRealizer = (GroupNodeRealizer) nodeRealizer;
	  }else if (nodeRealizer instanceof ProxyShapeNodeRealizer &&
			  ((ProxyShapeNodeRealizer) nodeRealizer).getRealizerDelegate() instanceof GroupNodeRealizer) {
		  groupNodeRealizer = (GroupNodeRealizer) ((ProxyShapeNodeRealizer) nodeRealizer).getRealizerDelegate();
	  }
	  return groupNodeRealizer;
  }  
}  