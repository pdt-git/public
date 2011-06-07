package org.cs3.pdt.internal.views.lightweightOutline;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.prolog.PrologSession;


public class PrologOutlineQuery {

	public static Map<String, ModuleElement> getProgramElementsForFile(String fileName/*, Shell shell*/) {	
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
				return new HashMap<String, ModuleElement>();
			}
			session = console.getPrologInterface().getSession();

			String query = "find_definition_contained_in('" + fileName+"',"+"Entity, KindOfEntity, Functor, Arity, TypeOfDef, Line, PropertyList)";
			List<Map<String, Object>> result = session.queryAll(query);

			if(! result.isEmpty()) {
				return extractResults(result);
			}
		}catch(Exception e){
			Debug.report(e);
		} finally {
			if(session!=null)session.dispose();
		}
		return new HashMap<String, ModuleElement>();
	}

	@SuppressWarnings("unchecked")
	private static Map<String, ModuleElement> extractResults(List<Map<String, Object>> result) {
		Map<String, ModuleElement> modules= new HashMap<String, ModuleElement>();	
		String module = "user";
		for (Map<String, Object> predicate : result) {
			module=(String)predicate.get("Entity");
			String name=(String)predicate.get("Functor");
			String kindOfEntity = (String)predicate.get("KindOfEntity");
			int arity=Integer.parseInt((String)predicate.get("Arity"));
			int line = Integer.parseInt((String)predicate.get("Line"));
			String type = (String)predicate.get("TypeOfDef");
			Object prop = predicate.get("PropertyList");
			List<String> properties = null;
			if (prop instanceof Vector<?>) {
				properties = (Vector<String>)prop;
			}				
			if (!modules.containsKey(module)) {
				modules.put(module, new ModuleElement(module, kindOfEntity));
			}
			ModuleElement currentModuleElem = modules.get(module);
			String label = module+":"+name+"/"+arity;

			OutlinePredicate prologPredicate;
			if (currentModuleElem.hasPredicate(label)) {
				prologPredicate = currentModuleElem.getPredicate(label);
			} else {
				prologPredicate = new OutlinePredicate(module, name, arity, properties);
				currentModuleElem.addChild(label, prologPredicate);
			}
			
			StringBuffer occuranceLabel = calculateOccuranceLabel(line, type,
					properties);
			prologPredicate.addOccurence(new PredicateOccuranceElement(occuranceLabel.toString(), line, type, prologPredicate));
		}

		return modules;
	}

	public static StringBuffer calculateOccuranceLabel(int line, String type,
			List<String> properties) {
		StringBuffer occuranceLabel = new StringBuffer("Line: ");
		occuranceLabel.append(Integer.toString(line));
		occuranceLabel.append(" (");
		occuranceLabel.append(type);
		if (type.equals("multifile")) {
			occuranceLabel.append(" @ ");
			for (String property : properties) {
				if (property.startsWith("from(")) {
					occuranceLabel.append((String) property.subSequence(5, property.length()-1));
				}
			}
		}
		occuranceLabel.append(")");
		return occuranceLabel;
	}
	
}