/*
 */
package org.cs3.pdt.internal.natures;

import java.util.HashSet;
import java.util.Set;

import org.cs3.pdt.IPrologProject;
import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pl.common.Debug;
import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.preferences.IPreferencesService;

/**
 */
public class PrologProjectNature implements IProjectNature, IPrologProject {

    private IProject project;

    /**
     * @see IProjectNature#configure
     */
    public void configure() throws CoreException {
        try {
            Debug.debug("configure was called");
            IProjectDescription descr = project.getDescription();
            ICommand builder = descr.newCommand();
            builder.setBuilderName(PDT.BUILDER_ID);
            ICommand builders[] = descr.getBuildSpec();
            for (int i = 0; i < builders.length; i++) {
                if (builders[i].getBuilderName().equals(PDT.BUILDER_ID)) {
                    return;
                }
            }
            ICommand newBuilders[] = new ICommand[builders.length + 1];
            System.arraycopy(builders, 0, newBuilders, 0, builders.length);
            newBuilders[builders.length] = builder;
            descr.setBuildSpec(newBuilders);
            project.setDescription(descr, null);
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t);
        }
    }

    /**
     * @see IProjectNature#deconfigure
     */
    public void deconfigure() throws CoreException {
        try {
            IProjectDescription descr = project.getProject().getDescription();
            org.cs3.pl.common.Debug.debug("deconfigure was called");
            ICommand builders[] = descr.getBuildSpec();
            int index = -1;
            for (int i = 0; i < builders.length; i++) {
                if (builders[i].getBuilderName().equals(PDT.BUILDER_ID)) {
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
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.resources.IProjectNature#getProject()
     */
    public IProject getProject() {
        return this.project;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
     */
    public void setProject(IProject project) {
        this.project = project;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pdt.IPrologProject#getSourcePath()
     */
    public String getSourcePath() throws CoreException {
        String val = null;
        val = getProject().getPersistentProperty(
                new QualifiedName("", PDT.PROP_SOURCE_PATH));
        if (val == null) {
            IPreferencesService service = Platform.getPreferencesService();
            String qualifier = PDTPlugin.getDefault().getBundle()
                    .getSymbolicName();
             val = service.getString(qualifier,
                    PDT.PREF_SOURCE_PATH_DEFAULT, "", null);
        }
        return val;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pdt.IPrologProject#setSourcePath(java.lang.String)
     */
    public void setSourcePath(String path) throws CoreException {
        getProject().setPersistentProperty(
                new QualifiedName("", PDT.PROP_SOURCE_PATH), path);

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pdt.IPrologProject#getExistingSourcePathEntries()
     */
    public Set getExistingSourcePathEntries() throws CoreException {
        Set r = new HashSet();
        String[] elms = getSourcePath().split(
                System.getProperty("path.separator"));
        for (int i = 0; i < elms.length; i++) {
            IProject p = getProject();
            IFolder folder = p.getFolder(elms[i]);
            if (folder.exists()) {
                r.add(folder);
            }
        }
        return r;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pdt.IPrologProject#isPrologSource(org.eclipse.core.resources.IResource)
     */
    public boolean isPrologSource(IResource resource) throws CoreException {
        Set sourcePathEntries = getExistingSourcePathEntries();

        if (!resource.exists()) {
            return false;
        }
        if (sourcePathEntries.contains(resource)) {
            return true;
        }
        if (resource.getType() == IResource.FILE) {
            String ext = resource.getFileExtension();
            return ext!=null&&ext.equals("pl")
                    && isPrologSource(resource.getParent());
        } else if (resource.getType() == IResource.FOLDER) {
            return isPrologSource(resource.getParent());
        }
        return false;
    }

}
