package org.cs3.jlmp.natures;

import java.io.IOException;
import java.io.PrintStream;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.cs3.jlmp.astvisitor.DefaultGenerationToolbox;
import org.cs3.jlmp.astvisitor.FactGenerationToolBox;
import org.cs3.jlmp.astvisitor.FactGenerator;
import org.cs3.jlmp.astvisitor.PrologWriter;
import org.cs3.jlmp.builders.JLMPProjectBuilder;
import org.cs3.jlmp.bytecode.ByteCodeFactGeneratorIType;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.CompilationUnit;

/**
 * @see IProjectNature
 */
public class JLMPProjectNature implements IProjectNature {
    /**
     * @deprecated use <code>JLMP.NATURE_ID</code> instead
     */
    public final static String NATURE_ID = "org.cs3.jlmp.JLMPProjectNature";

    //The project for which this nature was requested,
    //see IProject.getNature(String)
    private IProject project;

    /**
     *  
     */
    public JLMPProjectNature() {
    }

    /**
     * @see IProjectNature#configure
     */
    public void configure() throws CoreException {
        Debug.debug("configure was called");
        IProjectDescription descr = project.getDescription();
        ICommand sheepBuilder = descr.newCommand();
        sheepBuilder.setBuilderName(JLMPProjectBuilder.BUILDER_ID);
        ICommand builders[] = descr.getBuildSpec();
        for (int i = 0; i < builders.length; i++) {
            if (builders[i].getBuilderName().equals(
                    JLMPProjectBuilder.BUILDER_ID)) {
                return;
            }
        }
        ICommand newBuilders[] = new ICommand[builders.length + 1];
        System.arraycopy(builders, 0, newBuilders, 0, builders.length);
        newBuilders[builders.length] = sheepBuilder;
        descr.setBuildSpec(newBuilders);
        project.setDescription(descr, null);
    }

    /**
     * @see IProjectNature#deconfigure
     */
    public void deconfigure() throws CoreException {
        IProjectDescription descr = project.getProject().getDescription();
        Debug.debug("deconfigure was called");
        ICommand builders[] = descr.getBuildSpec();
        int index = -1;
        for (int i = 0; i < builders.length; i++) {
            if (builders[i].getBuilderName().equals(
                    JLMPProjectBuilder.BUILDER_ID)) {
                index = i;
                break;
            }
        }
        if (index != -1) {
            ICommand newBuilders[] = new ICommand[builders.length - 1];
            System.arraycopy(builders, 0, newBuilders, 0, index);
            System.arraycopy(builders, index + 1, newBuilders, index,
                    builders.length - index - 1);
            descr.setBuildSpec(newBuilders);
        }
    }

    /**
     * @see IProjectNature#getProject
     */
    public IProject getProject() {
        return project;
    }

    /**
     * @see IProjectNature#setProject
     */
    public void setProject(IProject project) {
        this.project = project;
    }

    public PrologInterface getPrologInterface() {
        PrologInterface pif = PDTPlugin.getDefault().getPrologInterface(
                getProject().getName());

        //	    List libraries = pif.getBootstrapLibraries();
        //	    ResourceFileLocator l =
        // JLMPPlugin.getDefault().getResourceLocator(JLMP.ENGINE);
        //	    String name = Util.prologFileName(l.resolve(JLMP.MAIN_PL));
        //	    if(!libraries.contains(name)){
        //	        libraries.add(name);
        //	        if(pif.isUp());
        //	        PrologSession s = pif.getSession();
        //	        s.queryOnce("['"+name+"']");
        //	    }
        return pif;
    }

    /**
     * generate prolog element facts for a given compilation unit.
     * <p>
     * More than a convenience method, this should become <b>The Way(tm) </b> to
     * generate prolog representation of java source code on the facade level.
     * @param an compilation unit in working copy mode.
     * @param out a writer to which the facts should be written. It is a good idea to
     * use a buffered writer. The writer will not be closed.
     */
    public  void writeFacts(ICompilationUnit icu,PrintStream out) throws IOException,
            CoreException {
                CompilationUnit cu = null;

        IResource resource = icu.getResource();
        String path = resource.getFullPath().removeFileExtension()
                .addFileExtension("pl").toString();

     
        StringWriter sw = new StringWriter();
        PrologWriter plw = new PrologWriter(sw, true);//metaDataSRC.getPrependablePrologWriter(path);
        FactGenerationToolBox box = new DefaultGenerationToolbox();
        FactGenerator visitor = new FactGenerator(icu, resource.getFullPath()
                .toString(), box, plw);

        ASTParser parser = ASTParser.newParser(AST.JLS2);
        parser.setSource(icu);
        parser.setResolveBindings(true);
        cu = (CompilationUnit) parser.createAST(null);
        
            cu.accept(visitor);
            plw.writeQuery("retractLocalSymtab");
            plw.flush();
            String data = sw.getBuffer().toString();
            sw.getBuffer().setLength(0);
            Map mapping = box.getFQNTranslator().getFQNMapping();
            writeSymTab(plw, mapping);
            plw.flush();
            plw.close();
            String header = sw.getBuffer().toString();
            out.println(header);
            out.println(data);
      
        
    }
    public void writeFacts(String typeName,PrintStream out) throws JavaModelException, CoreException, ClassNotFoundException {
        writeFacts(project,typeName,out);
    }
    public void writeFacts(IProject project, String typeName,PrintStream out) throws JavaModelException, CoreException, ClassNotFoundException {
        StringWriter sw = new StringWriter();
        PrologWriter plw = new PrologWriter(sw, true);
        
        FactGenerationToolBox box = new DefaultGenerationToolbox();
        new ByteCodeFactGeneratorIType(project, plw, typeName,
                box).writeAllFacts();
        plw.writeQuery("retractLocalSymtab");
        plw.flush();
        String data = sw.toString();
        sw.getBuffer().setLength(0);
        Map mapping = box.getFQNTranslator().getFQNMapping();
        writeSymTab(plw, mapping);
        plw.flush();
        String header = sw.toString();
        sw.getBuffer().setLength(0);
        String fileName = typeName.replace('.', '/') + ".pl";
        //out = metaDataEXT.getOutputStream(fileName);
        out.println(header);
        out.println(data);
    }
    private  void writeSymTab(PrologWriter plw, Map mapping) {
        Set set = mapping.keySet();
        boolean temp = plw.getInterpretMode();
        plw.setInterpretMode(false);
        try {
            for (Iterator it = set.iterator(); it.hasNext();) {

                String fqn = (String) it.next();
                String id = (String) mapping.get(fqn);
                plw.writeFact("local2FQN", new String[] { id, fqn });
            }
        } finally {
            plw.setInterpretMode(temp);
        }
    }
}