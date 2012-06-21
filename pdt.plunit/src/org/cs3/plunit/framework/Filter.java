package org.cs3.plunit.framework;

import java.util.HashSet;
import java.util.Set;

public class Filter {

	Set<Class<?>> annotations = null;
	Set<String> packageFilters = null;
	
	public void addAnnotation(Class<?> annotation){
		initAnnotations();
		annotations.add(annotation);
	}

	private void initAnnotations() {
		if(annotations==null){
			annotations = new HashSet<Class<?>>();
		}
	}

	/**
	 * Use the full name of a package, or the prefix plus a '*'.
	 * Use 'a.b.c' to match 'a.b.c'. 
	 * Use 'a.b.*' to match 'a.b.x' and 'a.b.y' but not 'a.b'. 
	 * Use 'a.b*' to match 'a.b.x' and 'a.b.y' and also 'a.b'. 
	 * 
	 * @param packageFilter Prefix or full name of a package.
	 */
	public void addPackage(String packageFilter) {
		initPackageFilters();
		packageFilters.add(packageFilter);	
	}

	private void initPackageFilters() {
		if(packageFilters==null){
			packageFilters = new HashSet<String>();
		}
	}
	
	public Set<String> getPackageFilters() {
		return packageFilters;
	}
	
	public Set<Class<?>> getAnnotations() {
		return annotations;
	}

}
