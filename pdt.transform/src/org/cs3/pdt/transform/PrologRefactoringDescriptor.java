package org.cs3.pdt.transform;

import java.io.File;
import java.util.Map;

import org.cs3.pl.common.Option;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchPart;

public abstract class PrologRefactoringDescriptor {
	private String[] dependencies;
	private File[] definitions;
	private String id;
	private String label;
	private Class<?> objectClass;
	private boolean adaptable;
	private String description;

	public final String[] getDependencies() {
		return dependencies;
	}

	public final void setDependencies(String[] dependencies) {
		this.dependencies = dependencies;
	}

	public final File[] getDefinitions() {
		return definitions;
	}

	public final void setDefinitions(File[] definitions) {
		this.definitions = definitions;
	}

	public final String getId() {
		return id;
	}

	public final void setId(String id) {
		this.id = id;
	}

	public final String getLabel() {
		return label;
	}

	public final void setLabel(String label) {
		this.label = label;
	}

	public final Class<?> getObjectClass() {
		return objectClass;
	}

	public final void setObjectClass(Class<?> objectClass) {
		this.objectClass = objectClass;
	}

	public final boolean isAdaptable() {
		return adaptable;
	}

	public final void setAdaptable(boolean adaptable) {
		this.adaptable = adaptable;
	}

	public final String getDescription() {
		return description;
	}

	public final void setDescription(String description) {
		this.description = description;
	}

	public abstract String getSelectionTerm(ISelection selection,
			IWorkbenchPart activePart) throws CoreException;

	
	public abstract Option[] getParameters(ISelection selection,
			IWorkbenchPart activePart)throws CoreException;

	public abstract PrologInterface getPrologInterface(ISelection selection,
			IWorkbenchPart activePart)throws CoreException;
	
	/**
	 * generate a data term representing the parameters of the refactoring.
	 * 
	 * The exact format of the term is only known to the refactoring. It is
	 * however guaranteed to be a syntactical valid prolog term.
	 * 
	 * Implementations should represent parameters for which the value is not
	 * yet known using meta variables with. The names of those variables should
	 * equal the key of the respective Option returned by getParameters.
	 */
	public abstract String getParametersTerm(Map<String, String> parameterValues);

	

}
