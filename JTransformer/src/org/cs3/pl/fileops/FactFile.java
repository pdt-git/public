package org.cs3.pl.fileops;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import org.cs3.pl.exceptions.ExceptionHandler;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;


/**
 * 
 * This class provides an abstraction for the files generated for Prolog
 * Facts. It can represent both compressed and incompressed files, and provides
 * a clean and managable way to handle them. It also abstracts from file
 * extensions, allowing the fact file to be called exactly as the class it
 * represents, and provides support for a repository of Prolog Facts. In case there
 * exists both a compressed and an uncompressed version of the file, the FactFile
 * uses the one that matches the current compression setting and deletes the other.
 * 
 * @author Stefan Schulz
 */

public class FactFile {
	
	private static boolean compressOnNew = true;
	private boolean compress;
	
	protected File target;
	protected String classname;
	
	/**
	 * set if new Files should be compressed by default. This value is used 
	 * if the 1-Argument constructor is used to create a new Instance.
	 */
	
	public static void setCompression(boolean val){
		compressOnNew = val;
	}
	
	/**
	 * get the default compression behaviour when creating a new File.
	 * 
	 * @return true if new files will be compressed by default.
	 */
	
	public static boolean getCompression(){
		return compressOnNew;
	}
	
	/**
	 * creates a new FactFile Object that will represent the class named
	 * classname. This should be the actual classname of the class, without
	 * any extensions. If there already exists such a FactFile, the current
	 * representation will be used, regardless of compression flag. classname
	 * is assumed to be fully qualified.
	 * 
	 * @param classname the name of the class this FactFile represents
	 */	
	
	
	public FactFile(IProject project, String classname){
		this(project, classname, compressOnNew);
	}
	
	/**
	 * Creates a new FactFile Object, corresponding to the class whose 
	 * source IFile object is passed to the constructor.
	 * 
	 * @param sourcefile An IFile handle for the source file
	 */
	
	public FactFile(IFile sourcefile){
		this(sourcefile.getProject(),sourcefile.getName(), compressOnNew);
	}
	
	/**
	 * creates a new FactFile Object that will represent the class named
	 * classname. This should be the actual classname of the class, without
	 * any extensions. If there already exists such a FactFile, the current
	 * representation will be used, regardless of compression flag. classname
	 * is assumed to be fully qualified.
	 * 
	 * @param classname the name of the class this FactFile represents
	 * @param compress if a new file should be compressed when generated.
	 */
	
	public FactFile(IProject current, String classname, boolean compress){
		//IProject current = PDTPlugin.getDefault().getProject();
		IFolder plfolder = current.getFolder("pl");
		
		classname = classname.replace('.', '/');
				
		IFile compressed = plfolder.getFile(classname + ".pl.gz");
		IFile plain = plfolder.getFile(classname + ".pl");
		
		File compressedFile = compressed.getLocation().toFile();
		File plainFile = plain.getLocation().toFile();
		
		if (plainFile.exists() && compressedFile.exists()){
			if (compress){
				target = compressedFile;
				plainFile.delete();
				this.compress = true;
			} else {
				target = plainFile;
				compressedFile.delete();
				this.compress = false;
			}
		} else if (plainFile.exists()){
			target = plainFile;
			this.compress = false;
		} else if (compressedFile.exists()){
			target = compressedFile;
			this.compress = true;
		} else {
			if (compress){
				this.compress = true;
				target = compressedFile;
			} else {
				this.compress = false;
				target = plainFile;
			}
			
			target.mkdirs();
		}
	}
	
	/**
	 * creates a Writer instance fit to write into the FactFile. This
	 * transparantly handles compression on compressed Files. Returns null
	 * on Error, and a suitable Writer instance otherwise.
	 * 
	 * @return a writer suitable to write into the file.
	 */
	
	public Writer getWriter(){
		OutputStream out;
		
		try {
			if (compress)				
				 out = new GZIPOutputStream(new FileOutputStream(target));
			else 
				 out = new FileOutputStream(target);
			
		} catch (IOException e) {
			ExceptionHandler.handle(e);
			return null;
		}
		
		return new OutputStreamWriter(out);	
	}
	
	/**
	 * creates a Reader fit to read from the File, transparently handling
	 * compression if enabled on this file. Returns null on error.
	 * 
	 * @return a reader suitable to read from the file.
	 */
	
	public Reader getReader(){
		InputStream in;
		
		try {
			if (compress)
				in = new GZIPInputStream(new FileInputStream(target));
			else 
				in = new FileInputStream(target);
		} catch (IOException e){
			ExceptionHandler.handle(e);
			return null;
		}
		
		return new InputStreamReader(in);
	}
	
	/**
	 * outputs the contents of the FactFile on the Stream passed, uncompressing
	 * it if necessary. This methid avoids using a tempoary file.
	 * 
	 * @param outstream the stream to which data is written.
	 * @throws IOException An IO Operation fails 
	 */
	
	public void outputContentsToStream(OutputStream outstream) throws IOException{
		FileInputStream fin = new FileInputStream(target);
		Writer out = new OutputStreamWriter(outstream);
		Reader in;
		
		if (compress){
			in = new InputStreamReader(new GZIPInputStream(fin));
		} else
			in = new InputStreamReader(fin);
		
		char buffer[] = new char[1024];
		
		int read = in.read(buffer);
		
		while (read != -1){
			out.write(buffer, 0, read);
			read = in.read(buffer);
		}	
	}
	
	/**
	 * returns a String that represents a Path that can be passed to
	 * consult(). This may create a temporary uncompressed copy of this
	 * FactFile, which is then flagged deleteOnExit. May return null if
	 * there is an IOException.
	 * 
	 * @return a String that can be passed to consult() to add the
	 * facts contained in this FactFile.
	 * @throws IllegalStateException the File does not yet exist in 
	 * the Filesystem
	 */
	
	public String getConsultablePath(){
		
		if (!target.exists())
			throw new IllegalStateException("File does not exist yet");
		
		if (!compress)
			return target.getAbsolutePath();
		
		File tempPath = new File(System.getProperty("java.io.tempdir"));
		File tempFile = new File(tempPath, classname + ".pl");
		
		tempFile.deleteOnExit();
		
		try {
			char buffer[] = new char[1024];
			int read;
			
			Writer out = new FileWriter(tempFile);
			Reader in = getReader();
			
			read = in.read(buffer);
			
			while (read >= 0){
				out.write(buffer, 0, read);
				read = in.read(buffer);
			}
			
			in.close();
			out.close();
		} catch (IOException e){
			ExceptionHandler.handle(e);
			return null;
		}
		
		return tempFile.getAbsolutePath();
	}
	
	/**
	 * returns true if and only if a File actually exists for this
	 * FactFile object. If a FactFile does not exist, it can still be
	 * used (for example, to get a Writer that will fill it with data),
	 * but an attempt to getConsultableString() will throw an 
	 * IllegalStateException.
	 */
	
	public boolean exists(){
		 return target.exists();
	}
	
	/**
	 * deletes the underlying file, making sure that the file is not 
	 * accidently partially overwritten.
	 * 
	 * @return true if the file was deleted
	 */
	
	public boolean delete(){
		return target.delete();
	}
	
	public boolean isCompressed(){
		return compress;
	}
}