package org.cs3.pdt.internal.queries;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.internal.structureElements.OutlineModuleElement;
import org.cs3.pdt.internal.structureElements.OutlinePredicate;
import org.cs3.pdt.internal.structureElements.PredicateOccuranceElement;
import org.cs3.pl.common.Debug;
import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.prolog.PrologSession;


public class PDTOutlineQuery {

	public static Map<String, OutlineModuleElement> getProgramElementsForFile(String fileName/*, Shell shell*/) {	
		PrologSession session=null;
		try {
			PrologConsole console = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole();
			if(console==null || console.getPrologInterface() == null){
				//				MessageBox messageBox = new MessageBox(
				//						shell, SWT.ICON_WARNING| SWT.OK);
				//
				//				messageBox.setText("Outline");
				//				messageBox.setMessage("Cannot open outline, no active Prolog Console found.");
				//				messageBox.open();
				return new HashMap<String, OutlineModuleElement>();
			}
			session = console.getPrologInterface().getSession();
			
			session.queryOnce("pdt_editor_reload:wait_for_reload_finished");

			String query = "pdt_search:find_definition_contained_in('" + fileName+"',"+"Entity, EntityLine, KindOfEntity, Functor, Arity, TypeOfDef, Line, PropertyList)";
			List<Map<String, Object>> result = session.queryAll(query);

			if(! result.isEmpty()) {
				return extractResults(result, fileName);
			}
		}catch(Exception e){
			Debug.report(e);
		} finally {
			if(session!=null)session.dispose();
		}
		return new HashMap<String, OutlineModuleElement>();
	}

	@SuppressWarnings("unchecked")
	private static Map<String, OutlineModuleElement> extractResults(List<Map<String, Object>> result, String fileName) {
		Map<String, OutlineModuleElement> modules= new HashMap<String, OutlineModuleElement>();	

		for (Map<String, Object> predicate : result) {
			String module = predicate.get("Entity").toString();
			int entityLine = Integer.parseInt( predicate.get("EntityLine").toString() );
			String name = predicate.get("Functor").toString();
			String kindOfEntity = predicate.get("KindOfEntity").toString();
			int arity=Integer.parseInt(predicate.get("Arity").toString());
			int line = Integer.parseInt(predicate.get("Line").toString());
			String type = predicate.get("TypeOfDef").toString();
			Object prop = predicate.get("PropertyList");
			List<String> properties = null;
			if (prop instanceof Vector<?>) {
				properties = (Vector<String>)prop;
			}				
			if (!modules.containsKey(module)) {
				modules.put(module, new OutlineModuleElement(fileName, module, entityLine, kindOfEntity));
			}
			OutlineModuleElement currentModuleElem = modules.get(module);
			String label = module+":"+name+"/"+arity;

			OutlinePredicate prologPredicate;
			if (currentModuleElem.hasPredicate(label)) {
				prologPredicate = currentModuleElem.getPredicate(label);
			} else {
				prologPredicate = new OutlinePredicate(module, name, arity, properties, fileName);
				currentModuleElem.addChild(label, prologPredicate);
			}
			
			StringBuffer occuranceLabel = calculateOccuranceLabel(line, type,
					properties);
			String occuranceFile = getOccuranceFileName(properties, fileName);
			
			prologPredicate.addOccurence(new PredicateOccuranceElement(occuranceLabel.toString(), occuranceFile, line, type, prologPredicate));
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
			for (String property : properties) {
				if (property.startsWith("from(")) {
					occuranceLabel.append(" @ ");
					occuranceLabel.append((String) property.subSequence(5, property.length()-1));
				} else if (property.startsWith("for(")) {
					occuranceLabel.append(" @ ");
					occuranceLabel.append((String) property.subSequence(4, property.length()-1));
				}
			}
		}
		occuranceLabel.append(")");
		return occuranceLabel;
	}
	
	private static String getOccuranceFileName(List<String> properties, String file) {
		String selectedFile="";
		if (properties.contains("multifile")) {
			for (String property : properties) {
				if (property.startsWith("defining_file(")) {
					selectedFile = property.substring(15, property.length()-2);
				}
			}
		}
		if (selectedFile.equals("")) {
			selectedFile = file;
		}
		return selectedFile;
	}
	
}