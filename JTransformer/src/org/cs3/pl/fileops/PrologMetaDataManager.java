/*
 * Created on 21.04.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.fileops;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;

import java.util.Iterator;
import java.util.List;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.parser.*;
import org.cs3.pl.prolog.*;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

/**
 * Provides Prolog Operations on MetaDataManager-handled files.
 *  
 * @author windeln
 */
public class PrologMetaDataManager extends MetaDataManager {
	
	static public final String MODEL = "model" + File.separatorChar;
	static public final String EXT = "ext" + File.separatorChar;
	static public final String PL = "pl" + File.separatorChar;
	
	
	
	private boolean useCompiled = false;
	static private final String PROLOG_EXTENSION = "pl";
	private IPrologClient client;
	
	
		
	/**
	 * constructs a new PrologMetaDataManager in the specified directory. This
	 * <u>must</u> bei either MODEL, EXT or PL.
	 * @param dir one of MODEL, EXT or PL
	 * @throws IOException an IO error occured
	 */
	PrologMetaDataManager(IPrologClient client, String dir) throws IOException {
		super(dir,PROLOG_EXTENSION);
		this.client = client;
	}

//	/**
//	 * loads the currently saved MetaData into the Prolog System.
//	 * 
//	 * @param dir One of MODEL, EXT or PL
//	 * @throws IOException an IO Error occurs
//	 */
//
//	/* XXX: Shouldn't this one really be static? The dir argument is unused, too */
//	
//	void loadMetaData(String dir) throws IOException {
//		reloadMetaData(PrologManager.getInstance().getHiddenClient());
//	}

	
	private void compile(String filename) throws IOException {
		filename = getFileNameWithNewExtension(filename);
		String metafile = getMetaDataFileLocation(filename);
		client.query("qcompile('"+
				PrologHelper.makeFilenameSWIConform(metafile)+"')");

		new File(metafile).delete();
	}
	
	/** 
	 * loads the handle's backing file into the Prolog System. Please not that this
	 * method returns true if it <u>looks like</u> the consult has succeeded. If any
	 * queries in the consulted file fail, there is no way to check it here. Best
	 * efford it taken to ensure a truthfull return value, and if false is 
	 * returned, the consult has definitly failed.
	 * 
	 * @param filename the handle to be consulted
	 * @return false if consultation definitly failed, true otherwise.
	 * @throws IOException an IO Error occured.
	 */
	
	public boolean consult(String filename) throws IOException {
		String qlfName = getMetaDataFileLocation(getQLFFileForName(filename));
		String plName = getMetaDataFileLocation(getFileNameWithNewExtension(filename));
		String target;
		
		boolean plOk = new File(plName).exists();
		boolean qlfOk = new File(qlfName).exists();
		
		if (!plOk || (useCompiled &&!qlfOk))
			return false;
		
		if (useCompiled) {
			if (!qlfOk)
				compile(filename);
			target = qlfName;
		} else
			target = plName;
		
		
		new PrologHelper(client).consult(target);
		Debug.info("Consulted file " + target);
		return true;
	}
		


	/**
	 * causes the PrologMetaDataManager reload the meta data it knows.
	 * 
	 * @param a IPrologManager object used to consult the files
	 * @exception IOException an IO Error occured
	 */
	
	/* XXX Only used in another public method. Could be merged to it! */
	
	public void reloadMetaData() throws IOException {
		//File loc = new File(getStateLocation());
		
		loadFiles(new File(getStateLocation()),client);		
	}

	private void loadFiles(File dir,IPrologClient client) throws IOException {
		File[] files = dir.listFiles();
		//System.err.println("DEBUG: dir:"+dir);
		for (int i = 0; i < files.length; i++) {
			if (files[i].isDirectory())
				loadFiles(files[i],client);
			else {
				String path = files[i].getAbsolutePath();
				if (path.endsWith(".qlf") || path.endsWith(".pl"))
					new PrologHelper(client).consult(path);
			}
		}
		
	}
	
	/**
	 * parses the file, and generates MetaData for the clauses encountered. These are generated for the
	 * facts described in the file, and saved for future reference. They are loaded into the Prolog Engine.
	 * 
	 * @param filename the filename to be added to the repository.
	 * @param checker the compiler used to check the facts for consistency
	 */
	
	public void saveMetaDataForClauses(String filename, PrologCompiler checker) {
		try {
			//PrologMetaDataManager metaDataManager = new PrologMetaDataManager(client,PrologMetaDataManager.MODEL);
			IWorkspaceRoot root = PDTPlugin.getDefault().getWorkspace().getRoot();
			BufferedWriter writer = getMetaDataWriter(filename);
			writer.write(":- style_check(-atom).\n");
			String module = checker.getModuleName();
			List clauses = checker.getClauses();
			String moduleHelp = checker.getModuleHelp();
			if (moduleHelp == null)
				moduleHelp = "";
			writer.write(PrologClient.METADATAMODULE+ "('"+filename+"','" + 
					module + "', \"" + moduleHelp+"\").\n"); 
			//HashMap publicElements = checker.getPublicModulePredicates();
			for (Iterator iter = clauses.iterator(); iter.hasNext();) {
				ASTClause clause = (ASTClause) iter.next();
				PrologElementData data = checker.getPrologElementData(clause);
				writer.write(PrologClient.METADATA+"('"+filename+"'," + module 
						+","+ data.getLabel()
						+","+ data.getArity()
						+","+ data.isPublic()
						+","+ data.getPosition()
						+","+ data.getLength()					
						+","+ data.isDynamic()
						+","+ data.isMultifile()
						+").\n");
				String comment = clause.getComment(data.getLabel());
				if (comment != null)
				writer.write(PrologClient.METADATAHELP 
						+"("
						+ module
						+"," +data.getLabel()
						+","+ data.getArity()
						+","+ comment
						+").\n");
			}
			saveMetaDataHelpForDynamicPredicates(writer, checker);
			writer.close();
			consult(filename);
		} catch (IOException e1) {
			Debug.report(e1);
		}		
	}
	
	private void saveMetaDataHelpForDynamicPredicates(BufferedWriter writer, PrologCompiler checker) throws IOException {
		ASTNamedCall[] dynamic = checker.getDynamic();
		for (int i = 0; i < dynamic.length; i++) {
			if(dynamic[i].jjtGetNumChildren() == 1) {
				ASTPredicateSignature sig = (ASTPredicateSignature)dynamic[i].jjtGetChild(0);
				String name = sig.getName();
				String help = dynamic[i].getComment(name);
				if (help != null) { 
					String module = sig.getModule();
					int arity = sig.getArity();
					writer.write(PrologClient.METADATAHELP+ "("+
							module +
							", " + name + 
							", " + arity+
							", " + help + ").\n");
				}		
			}
			
		}
		
	}

	//FIXME: das muss auch irgendwann gemacht werden: init von factbase.pl meta data
	public void readFactbaseMetaData() throws CoreException, IOException {
		String filename = "/"+PDTPlugin.JTRANFORMERENGINE + "/factbase.pl";
		if(exists(filename) && client != null) {
		    try {
                consult(filename);
			} catch (IOException e1) {
				Debug.report(e1);
			}		
		    return;
		}
		    
		PrologCompilerBackend checker = new PrologCompilerBackend();
		//IFile factbase = PDTPlugin.getDefault().getFile(new Path(filename ));
		String engineDir = PrologManager.getEngineDir();
		
		checker.compile(new File(engineDir + "factbase.pl"));
		
		saveMetaDataForClauses(filename,checker);
	}



	/**
	 * returns the timestamp the facts were last updated on.
	 * @param filename
	 * @return the modification stamp, or 0 if the file was not located
	 */
	public long getUpdateTime(String filename) {
		String target = getMetaDataFileLocation(useCompiled? 
				getQLFFileForName(filename): 
				getFileNameWithNewExtension(filename));
		
		IPath toFile = new Path(target);
		
		
		/* HACK, Eclipse File API does not work as it should outside fot the
		 * workspace */
		
		File jFile = toFile.toFile();
		
		if (jFile.exists())
			return jFile.lastModified();
		else if (useCompiled){
			target = getFileNameWithNewExtension(filename);
			toFile = new Path(getMetaDataFileLocation(target));
			jFile = toFile.toFile();
		
			if (jFile.exists())
				return jFile.lastModified();
		}
		
		return 0;
	}

	/**
	 * deletes the fact-file indicated by the passed metaName.
	 * @param metaName the file to be deleted.
	 */
	public void delete(String metaName) {
		String plname = getFileNameWithNewExtension(metaName);
		String qlfname = getQLFFileForName(metaName);
		
		File plfile = new File(getMetaDataFileLocation(plname));
		File qlffile = new File(getMetaDataFileLocation(qlfname));
		
		plfile.delete();
		qlffile.delete();
	}

	/**
	 * creates a Writer for the object referenced by the name. This Writer is an instance
	 * of PrependablePrologWriter, and can be used to write Prolog Facts easily. It is 
	 * normally used in the FactGenerator
	 * @param path the handle the writer will point to
	 * @return a PrependablePrologWriter
	 */
	public PrependablePrologWriter getPrependablePrologWriter(String path) {
		
		String plname = getFileNameWithNewExtension(path);
		
		PrologWriter writer = PrologWriter.getWriter(getMetaDataFileLocation(plname));
		
		return new PrependablePrologWriter(writer,path, this);	
	}

	private String getQLFFileForName(String path) {
		int indexOfDot = path.lastIndexOf(".");
		
		path = path.replace('$', '_');
		
		/* we could have a raw class name, the dread forth case ;) */
		if (indexOfDot != -1)
			path = path.substring(0, indexOfDot);
		
		return path + ".qlf";
	}
	
	private void setCompiled(boolean useCompiled) {
		if (useCompiled)
			Debug.warning("Use of compiled data is highly experimental!");
		
		this.useCompiled = useCompiled;
	}

	private boolean isCompiled() {
		return useCompiled;
	}
}
