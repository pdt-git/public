package org.cs3.pdt.internal.queries;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.internal.structureElements.OutlineModuleElement;
import org.cs3.pdt.internal.structureElements.OutlinePredicate;
import org.cs3.pdt.internal.structureElements.PredicateOccuranceElement;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.session.PrologSession;


public class PDTOutlineQuery {

	public static Map<String, OutlineModuleElement> getProgramElementsForFile(String fileName/*, Shell shell*/) {	
		PrologSession session=null;
		try {
			session = PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().getActivePrologInterface().getSession();
			
			session.queryOnce("pdt_editor_reload:wait_for_reload_finished");

			String query = "pdt_search:find_definition_contained_in('" + fileName+"',"+"Entity, KindOfEntity, Functor, Arity, TypeOfDef, Line, PropertyList)";
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
		String module = "user";
		for (Map<String, Object> predicate : result) {
			module = predicate.get("Entity").toString();
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
				modules.put(module, new OutlineModuleElement(module, kindOfEntity));
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