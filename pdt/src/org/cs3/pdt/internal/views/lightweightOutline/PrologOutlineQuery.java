package org.cs3.pdt.internal.views.lightweightOutline;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.prolog.PrologSession;


public class PrologOutlineQuery {

	@SuppressWarnings("unchecked")
		public static List<OutlinePredicate> getPredicatesForFile(String fileName/*, Shell shell*/) {
			List<OutlinePredicate> predicates= new ArrayList<OutlinePredicate>();
			PrologSession session=null;
			try {
				PrologConsole console = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole();
				if(console==null){
	//				MessageBox messageBox = new MessageBox(
	//						shell, SWT.ICON_WARNING| SWT.OK);
	//
	//				messageBox.setText("Outline");
	//				messageBox.setMessage("Cannot open outline, no active Prolog Console found.");
	//				messageBox.open();
					return predicates;
				}
				session = console.getPrologInterface().getSession();
				
				String query = "find_definition_contained_in('" + fileName+"',"+"Module,Name,Arity,Line,PropertyList)";
				List<Map<String, Object>> result = session.queryAll(query);
	
				Set<String> names = new HashSet<String>();
				for (Map<String, Object> predicate : result) {
					String module=(String)predicate.get("Module");
					String name=(String)predicate.get("Name");
					int arity=Integer.parseInt((String)predicate.get("Arity"));
					String signature = name+arity;
					if(!names.contains(signature)){
						names.add(signature);
						
						List<String> properties = null;
						Object prop = predicate.get("PropertyList");
						if (prop instanceof Vector<?>) {
							properties = (Vector<String>)prop;
						}
						int line = Integer.parseInt((String)predicate.get("Line"));
						OutlinePredicate prologPredicate = new OutlinePredicate( module, name, arity, 
																properties, line);
						predicates.add(prologPredicate);
				}
				}
			}catch(Exception e){
				Debug.report(e);
			} finally {
				if(session!=null)session.dispose();
			}
			return predicates;
		}
	
}