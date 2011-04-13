package org.cs3.pdt.internal.search;

import java.io.InputStream;
import java.io.Reader;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Map;

import org.eclipse.core.internal.resources.Resource;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFileState;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceProxy;
import org.eclipse.core.resources.IResourceProxyVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourceAttributes;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.content.IContentDescription;
import org.eclipse.core.runtime.jobs.ISchedulingRule;

public class ExternalFile implements IFile {
	
	private IPath path;

	public ExternalFile(IPath path) {
		this.path = path;
	}
	
	public int hashCode() {
		return getFullPath().hashCode();
	}

	public boolean equals(Object target) {
		if (this == target)
			return true;
		if(!(target instanceof ExternalFile)){
			return false;
		}
		return path.equals(((ExternalFile)target).path);
	}
	@Override
	public void accept(IResourceProxyVisitor visitor, int memberFlags)
			throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void accept(IResourceVisitor visitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void accept(IResourceVisitor visitor, int depth,
			boolean includePhantoms) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void accept(IResourceVisitor visitor, int depth, int memberFlags)
			throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void clearHistory(IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void copy(IPath destination, boolean force, IProgressMonitor monitor)
			throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void copy(IPath destination, int updateFlags,
			IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void copy(IProjectDescription description, boolean force,
			IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void copy(IProjectDescription description, int updateFlags,
			IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public IMarker createMarker(String type) throws CoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public IResourceProxy createProxy() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void delete(boolean force, IProgressMonitor monitor)
			throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void delete(int updateFlags, IProgressMonitor monitor)
			throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void deleteMarkers(String type, boolean includeSubtypes, int depth)
			throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean exists() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public IMarker findMarker(long id) throws CoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public IMarker[] findMarkers(String type, boolean includeSubtypes, int depth)
			throws CoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int findMaxProblemSeverity(String type, boolean includeSubtypes,
			int depth) throws CoreException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public String getFileExtension() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public long getLocalTimeStamp() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public IPath getLocation() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public URI getLocationURI() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public IMarker getMarker(long id) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public long getModificationStamp() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public IPathVariableManager getPathVariableManager() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public IContainer getParent() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Map getPersistentProperties() throws CoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getPersistentProperty(QualifiedName key) throws CoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public IProject getProject() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public IPath getProjectRelativePath() {
		return path;
	}

	@Override
	public IPath getRawLocation() {
		return path;
	}

	@Override
	public URI getRawLocationURI() {
		try {
			return new URI("file", path.toPortableString(), null);
		} catch (URISyntaxException e) {
			e.printStackTrace();
			return null;
		}
	}

	@Override
	public ResourceAttributes getResourceAttributes() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Map getSessionProperties() throws CoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Object getSessionProperty(QualifiedName key) throws CoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getType() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public IWorkspace getWorkspace() {
		return null;
	}

	@Override
	public boolean isAccessible() {
		return false;
	}

	@Override
	public boolean isDerived() {
		return false;
	}

	@Override
	public boolean isDerived(int options) {
		return false;
	}

	@Override
	public boolean isHidden() {
		return false;
	}

	@Override
	public boolean isHidden(int options) {
		return false;
	}

	@Override
	public boolean isLinked() {
		return false;
	}

	@Override
	public boolean isVirtual() {
		return false;
	}

	@Override
	public boolean isLinked(int options) {
		return false;
	}

	@Override
	public boolean isLocal(int depth) {
		return true;
	}

	@Override
	public boolean isPhantom() {
		return false;
	}

	@Override
	public boolean isSynchronized(int depth) {
		return true;
	}

	@Override
	public boolean isTeamPrivateMember() {
		return false;
	}

	@Override
	public boolean isTeamPrivateMember(int options) {
		return false;
	}

	@Override
	public void move(IPath destination, boolean force, IProgressMonitor monitor)
			throws CoreException {
		throw new CoreException(Status.CANCEL_STATUS);
	}

	@Override
	public void move(IPath destination, int updateFlags,
			IProgressMonitor monitor) throws CoreException {
		throw new CoreException(Status.CANCEL_STATUS);
	}

	@Override
	public void move(IProjectDescription description, boolean force,
			boolean keepHistory, IProgressMonitor monitor) throws CoreException {
		throw new CoreException(Status.CANCEL_STATUS);

	}

	@Override
	public void move(IProjectDescription description, int updateFlags,
			IProgressMonitor monitor) throws CoreException {
		throw new CoreException(Status.CANCEL_STATUS);

	}

	@Override
	public void refreshLocal(int depth, IProgressMonitor monitor)
			throws CoreException {

	}

	@Override
	public void revertModificationStamp(long value) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setDerived(boolean isDerived) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setDerived(boolean isDerived, IProgressMonitor monitor)
			throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setHidden(boolean isHidden) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setLocal(boolean flag, int depth, IProgressMonitor monitor)
			throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public long setLocalTimeStamp(long value) throws CoreException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void setPersistentProperty(QualifiedName key, String value)
			throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setReadOnly(boolean readOnly) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setResourceAttributes(ResourceAttributes attributes)
			throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setSessionProperty(QualifiedName key, Object value)
			throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setTeamPrivateMember(boolean isTeamPrivate)
			throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void touch(IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public Object getAdapter(Class adapter) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean contains(ISchedulingRule rule) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isConflicting(ISchedulingRule rule) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void appendContents(InputStream source, boolean force,
			boolean keepHistory, IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void appendContents(InputStream source, int updateFlags,
			IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void create(InputStream source, boolean force,
			IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void create(InputStream source, int updateFlags,
			IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void createLink(IPath localLocation, int updateFlags,
			IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void createLink(URI location, int updateFlags,
			IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void delete(boolean force, boolean keepHistory,
			IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public String getCharset() throws CoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getCharset(boolean checkImplicit) throws CoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getCharsetFor(Reader reader) throws CoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public IContentDescription getContentDescription() throws CoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public InputStream getContents() throws CoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public InputStream getContents(boolean force) throws CoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getEncoding() throws CoreException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public IPath getFullPath() {
		return path;
	}

	@Override
	public IFileState[] getHistory(IProgressMonitor monitor)
			throws CoreException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getName() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isReadOnly() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void move(IPath destination, boolean force, boolean keepHistory,
			IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setCharset(String newCharset) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setCharset(String newCharset, IProgressMonitor monitor)
			throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setContents(InputStream source, boolean force,
			boolean keepHistory, IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setContents(IFileState source, boolean force,
			boolean keepHistory, IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setContents(InputStream source, int updateFlags,
			IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setContents(IFileState source, int updateFlags,
			IProgressMonitor monitor) throws CoreException {
		// TODO Auto-generated method stub

	}

}
