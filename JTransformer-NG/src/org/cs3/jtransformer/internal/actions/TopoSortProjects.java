package org.cs3.jtransformer.internal.actions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;

import salvo.jesus.graph.DirectedAcyclicGraph;
import salvo.jesus.graph.DirectedAcyclicGraphImpl;
import salvo.jesus.graph.DirectedEdgeImpl;
import salvo.jesus.graph.Vertex;
import salvo.jesus.graph.VertexImpl;
import salvo.jesus.graph.algorithm.TopologicalSorting;

public class TopoSortProjects {

	Hashtable vertexes;

	public List sort(boolean includeReferencedProjects, List projects) throws Exception {
			List toProcess = new ArrayList(projects);
			DirectedAcyclicGraph dag = new DirectedAcyclicGraphImpl();
			vertexes = new Hashtable();
			while(toProcess.size() > 0) {
				IProject project = (IProject)toProcess.get(0);
				toProcess.remove(project);
				Vertex vertex = getVertexForProject(toProcess,project);
				dag.add(vertex);
				final IJavaProject javaProject = (IJavaProject) project.getNature(JavaCore.NATURE_ID);
				IClasspathEntry[] referenced = javaProject.getResolvedClasspath(true);
				for (int i = 0; i < referenced.length; i++) {
					IClasspathEntry entry = referenced[i];
					if(entry.getEntryKind() == IClasspathEntry.CPE_PROJECT) {
						if(entry.getPath().segmentCount() > 1) {
							throw new Error("NOT A PROJECT" + entry);
						}
						IProject refProject = ResourcesPlugin.getWorkspace().getRoot().getProject(entry.getPath().segment(0));
						if(refProject.getName().endsWith("-output")) {
							continue;
						}
						if(!includeReferencedProjects && !projects.contains(refProject)) {
							continue;
						}
						Vertex refVertex = getVertexForProject(toProcess,refProject);
						dag.addEdge(new DirectedEdgeImpl(vertex, refVertex));
					}
					
				}
			}
			TopologicalSorting topoSort = new TopologicalSorting(dag);
			List sorted = topoSort.traverse();
			Collections.reverse(sorted);
			List finalList = new ArrayList();
			for (Iterator iter = sorted.iterator(); iter
					.hasNext();) {
				VertexImpl element = (VertexImpl) iter.next();
				finalList.add(element.getObject());
			}
			return finalList;
		}

	private Vertex getVertexForProject(List projects, IProject project) {
		Vertex vertex = (Vertex)vertexes.get(project);
		if(vertex != null) {
			return vertex;
		}
		if(!projects.contains(project)) {
			projects.add(project);
		}

		vertex = new VertexImpl(project);
		vertexes.put(project, vertex);
		return vertex;
	}
}
