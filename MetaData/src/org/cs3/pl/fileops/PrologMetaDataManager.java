package org.cs3.pl.fileops;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.DefaultMetaInfoProvider;
import org.cs3.pl.metadata.PrologElementData;
import org.cs3.pl.parser.ASTClause;
import org.cs3.pl.parser.ASTNamedCall;
import org.cs3.pl.parser.ASTPredicateSignature;
import org.cs3.pl.parser.PrologCompiler;
import org.cs3.pl.prolog.IPrologInterface;
import org.cs3.pl.prolog.SessionException;


/**
 * Provides Prolog Operations on MetaDataManager-handled files.
 *  
 * @author windeln
 */
public class PrologMetaDataManager extends MetaDataManager {
    public static final String METADATA = "meta_data";

    public static final String METADATAHELP = METADATA + "_help";

    public static final String METADATAMODULE = METADATA + "_module";
	static public final String MODEL = "model" + File.separatorChar;
	static public final String EXT = "ext" + File.separatorChar;
	static public final String PL = "pl" + File.separatorChar;
	
	
	
	private boolean useCompiled = false;
	static private final String PROLOG_EXTENSION = "pl";
    private IPrologInterface prologInterface;
	
	
		
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
		try {
            new DefaultMetaInfoProvider(prologInterface).consult(metafile);
        } catch (SessionException e) {
            Debug.report(e);
        }
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
		
		
		try {
            new DefaultMetaInfoProvider(prologInterface).consult(target);
        } catch (SessionException e) {
            Debug.report(e);
        }
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
		
		loadFiles(new File(getLocation()),prologInterface);		
	}

	private void loadFiles(File dir,IPrologInterface client) throws IOException {
		File[] files = dir.listFiles();
		//System.err.println("DEBUG: dir:"+dir);
		for (int i = 0; i < files.length; i++) {
			if (files[i].isDirectory())
				loadFiles(files[i],client);
			else {
				String path = files[i].getAbsolutePath();
				if (path.endsWith(".qlf") || path.endsWith(".pl"))
                    try {
                        new DefaultMetaInfoProvider(client).consult(path);
                    } catch (SessionException e) {
                    Debug.report(e);
                    }
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
			//IWorkspaceRoot root = PDTPlugin.getDefault().getWorkspace().getRoot();
			BufferedWriter writer = getMetaDataWriter(filename);
			writer.write(":- style_check(-atom).\n");
			String module = checker.getModuleName();
			List clauses = checker.getClauses();
			String moduleHelp = checker.getModuleHelp();
			if (moduleHelp == null)
				moduleHelp = "";
			writer.write(METADATAMODULE+ "('"+filename+"','" + 
					module + "', \"" + moduleHelp+"\").\n"); 
			//HashMap publicElements = checker.getPublicModulePredicates();
			for (Iterator iter = clauses.iterator(); iter.hasNext();) {
				ASTClause clause = (ASTClause) iter.next();
				PrologElementData data = checker.getPrologElementData(clause);
				writer.write(METADATA+"('"+filename+"'," + module 
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
				writer.write(METADATAHELP 
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
					writer.write(METADATAHELP+ "("+
							module +
							", " + name + 
							", " + arity+
							", " + help + ").\n");
				}		
			}
			
		}
		
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
    public IPrologInterface getPrologInterface() {
        return prologInterface;
    }
    public void setPrologInterface(IPrologInterface prologInterface) {
        this.prologInterface = prologInterface;
    }
    public boolean isUseCompiled() {
        return useCompiled;
    }
    public void setUseCompiled(boolean useCompiled) {
        this.useCompiled = useCompiled;
    }
}
