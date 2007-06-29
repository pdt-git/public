package org.cs3.jtransformer.internal.bytecode;

import org.cs3.jtransformer.JTDebug;
import org.cs3.jtransformer.internal.astvisitor.FQNTranslator;
import org.cs3.jtransformer.internal.astvisitor.FactGenerationToolBox;
import org.cs3.jtransformer.internal.astvisitor.IPrologWriter;
import org.cs3.jtransformer.internal.astvisitor.IdentityFQNTranslator;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;


/**
 * @author Schulz, Elguennouni
 *
 * This class generates the Interface facts to describe an Binary-Only class
 * (like java.lang.System, or any of the standart classes). The Class uses the
 * Java Reflection framework to generate the facts, and therefore the referenced
 * classes need to be accessable to the ClassLoader. The facts are generated 
 * according to the types explained in the documentation of the FactGenerator
 * 
 * @see org.cs3.jtransformer.internal.astvisitor.FactGenerator
 */
public class ByteCodeFactGeneratorIType {
	
	private IPrologWriter writer;
	private IType targetClass;
	private IDManagerIType idManager;
	private FQNTranslator fqn;
	
/*	/**
	 * Constructs a new ByteCodeFactGenerator. The Reflection Interface is called
	 * to generate the Class-Object, and the facts will be generated for <u>that</u>
	 * class. Further changes in the File System will not take effect.
	 * 
	 * @param plw A IPrologWriter instance used to write the facts.
	 * @param classname A fully qualified java class name.
	 * @throws ClassNotFoundException The classname could not be resolved.
	 */
/*
	public ITypeByteCodeFactGenerator(IPrologWriter plw, String classname) throws ClassNotFoundException{
		writer = plw;
		idManager = new IDManager();
		targetClass = Class.forName(classname);
	}
*/	
	public ByteCodeFactGeneratorIType(IPrologWriter plw, IDManagerIType idm, String classname, IJavaProject project, FQNTranslator fqn) throws ClassNotFoundException {
		writer = plw;
		idManager = idm;
		
		try {
			targetClass = loadClass(project, classname);
		} catch (JavaModelException e) {
			throw new RuntimeException("Bad Exception", e);
		}
		
		idm.setEnvironment(targetClass);
		
		this.fqn = fqn;
	}
	/**
	 * Constructs a new ByteCodeFactGenerator. This version of the constructor
	 * is moste useful for testing with MockIDManager Object. The Reflection 
	 * Interface is called to generate the Class-Object, and the fact s will be
	 * generated for <u>that</u> class. Further changes in the file system
	 * will not affect the outcome.
	 * 
	 * @param plw An IPrologWriter instance used to write the Facts 
	 * @param idm An IDManager instance to resolve the IDs
	 * @param classname A fully qualified Java class name
	 * @throws ClassNotFoundException thrown if the class is unknown.
	 */
	
	public ByteCodeFactGeneratorIType(IPrologWriter plw, IDManagerIType idm, String classname,IJavaProject javaProject) throws CoreException, ClassNotFoundException {
		this(plw, idm, classname, javaProject, new IdentityFQNTranslator());
	}

	/**
	 * Constructs a new ByteCodeFactGenerator. The preferred way to create one, in fact. It wraps 
	 * all the error-prone choice of the proper implementation of the various interfaces neatly.
	 * 
	 * @param pwriter
	 * @param fqn2
	 * @param javaProject
	 * @param box
	 */
	public ByteCodeFactGeneratorIType(IPrologWriter pwriter, String fqn2, IJavaProject javaProject, FactGenerationToolBox box) throws ClassNotFoundException, CoreException {
		this(pwriter, box.getIDManager(), fqn2, javaProject, box.getFQNTranslator());
	}
	//ld: regarding the PDTPlugin.getProject issue: the BCFG is instantiated by a builder.
	// a builder knows for which project it runs (it is associated to exactly one project
	// via the JTransformer Nature) so i changed the signiture of this constructor.
	public ByteCodeFactGeneratorIType(IProject project, IPrologWriter pwriter, String classname, FactGenerationToolBox box) throws JavaModelException, CoreException, ClassNotFoundException {
		
		IJavaProject javaProject = (IJavaProject) project.getNature(JavaCore.NATURE_ID);
		
		writer = pwriter;
		idManager = box.getIDManager();
		targetClass = loadClass(javaProject, classname);
		
		idManager.setEnvironment(targetClass);
		
		this.fqn = box.getFQNTranslator();

	}
	
	public static IType loadClass(IJavaProject javaProject, String classname) throws ClassNotFoundException, JavaModelException {
		IType targetClass = javaProject.findType(classname.replace('$','.'));
		
		if (targetClass == null)
			throw new ClassNotFoundException("Class " +classname+ " not found");
		return targetClass;
	}
	
	/**
	 * Generates the facts for the class that has been passed to the Constructor,
	 * writing them to the IPrologWriter passed at Object generation. Inner classes
	 * are not followed, and if they are referenced, must be generated later. The
	 * layout is fields, constructors, methods. Private members are not 
	 * transformed.
	 */
	
	public void writeAllFacts() throws JavaModelException{
		if (targetClass == null){
			JTDebug.error("Bad Class");
		}
		
		IType [] inner = targetClass.getTypes();
		IField [] fields = targetClass.getFields();
		IMethod [] methods = targetClass.getMethods();
		//IInitializer[] inits = targetClass.getInitializers();
		
		writeTopLevel();
		
		for (int i = 0; i < inner.length; i++){
			if (syntheticOrPrivate(inner[i].getFlags()))
					continue;
			
		}
		
//		if(targetClass.getElementName().contains("Licences"))
//			System.err.println("DEBUG");
		for (int i = 0; i < fields.length; i++) {
			if (syntheticOrPrivate(fields[i].getFlags()))
				continue;
			
			writeField(fields[i]);
		}
		
		for (int i = 0; i < methods.length; i++) {
			if (syntheticOrPrivate(methods[i].getFlags()))
				continue;
			
			if (!methods[i].getElementName().equals("<clinit>"))
					writeMethod(methods[i]);
		}
		//ld: TODO: correct me: since ids are now all local, this is none of
		//	  our concern any more.
		//idManager.writebackID();
		
		//instead we should do this. right?
		// no, we want to reuse the Symbol table for the entire file
		// writer.writeQuery("retractLocalSymtab");
	}
	

	/**
	 * Writes the facts representing a single Method. This method is called from
	 * generateAllFacts for each non-private Method Object.
	 * 
	 * @param method the Method to be transformed into facts
	 */
	protected void writeMethod(IMethod method) throws JavaModelException {
		String id = idManager.getID(method);
		IType father = method.getDeclaringType();
		String classRef = idManager.getID(father);
		String name = method.isConstructor() ? "'<init>'" : "'"+method.getElementName()+"'";
		
		StringBuffer paramBuffer = new StringBuffer("[");

		String type = typeForClass(method, method.getReturnType());
		StringBuffer exceptionBuffer = new StringBuffer("[");
		String body = "'null'";
		boolean first = true;
		
		String[] cls = method.getParameterTypes();
		
		for (int i = 0; i < cls.length; i++){ 
			String element = cls[i];
			
			if (!first)
				paramBuffer.append(", ");
			else
				first = false;
			
			String newID = idManager.newID();
			
			paramBuffer.append(newID);
						 
						
			String [] paramdefargs = new String [] {
					newID, 
					id, 
					typeForClass(method, cls[i]), 
					"'__" + i + "'"
			};
				
			writer.writeFact("paramDefT", paramdefargs);
			
		}
		
		paramBuffer.append("]");
		
		first=true;
		cls = method.getExceptionTypes();
		
		for (int i = 0; i < cls.length; i++){ 
			String element = cls[i];
			
			if (!first)
				exceptionBuffer.append(", ");
			else
				first = false;
			
			exceptionBuffer.append(idManager.getID(method, element));
		}	
		exceptionBuffer.append("]");
		
		String [] args = new String [] {
				id,
				classRef,
				name,
				paramBuffer.toString(),
				type,
				exceptionBuffer.toString(),
				body
		};
		
		writer.writeFact("methodDefT", args);
		writeModifiers(id, method.getFlags());
		
	}


	
	/**
	 * Writes the facts representing a single Field. This method is called from
	 * generateAllFacts for each non-private Field Object.
	 * 
	 * @param constructor the Field to be transformed into facts
	 */
	
	protected void writeField(IField field) throws JavaModelException {
		String id = idManager.getID(field);
		String cId = idManager.getID(field.getDeclaringType());	
		String type = typeForClass(null, field.getTypeSignature());
		String name = "'" + field.getElementName() + "'";
		String init = "'null'";
//		if(name.equals("JEM_ESCAPE")){
//			System.err.println("DEBUG");
//		}
		try {
			if(field.getConstant() == null)
				init = "'null'";
			else {
				String newId = idManager.newID();
				init =""+newId;
				String value = field.getConstant().toString();
				if(value.equals("\\")){
					value = "\\\\";
				} else {
					value = value.replaceAll("\\n","\\\\\\\\n");
					value = value.replaceAll("\\r","\\\\\\\\r");
					value = value.replaceAll("\\t","\\\\\\\\t");
					value = value.replaceAll("\"","\\\\\\\\\"");
				}
				value = value.replaceAll("'","\\\\'");
				
				String [] args = new String [] {
						newId, 
						id,
						id,
						type,
						"'" + value +"'"
				};
				writer.writeFact("literalT", args);
				
			}
		} catch(NumberFormatException nfe){
		    //This seems to be a bug in  org.eclipse.jdt.internal.core.SourceField.getConstant()
		    JTDebug.report(nfe);
		}
		String [] args = new String [] {
				id, 
				cId,
				type,
				name,
				init
		};
		
		writer.writeFact("fieldDefT", args);
		writeModifiers(id, field.getFlags());
	}
	
	/**
	 * Generates the "top level" facts for the class. It starts by writing a 
	 * packageT (if the package is not yet know to the Prolog System), and
	 * then adds classDefT, interfaceT and modifierT as needed.
	 *
	 */

	protected void writeTopLevel() throws JavaModelException{
		String packageName = targetClass.getPackageFragment().getElementName();


		String [] args;
		Object[] member;
		
		String id = idManager.getID(targetClass);
		String owner;
		String name;
		
		StringBuffer defBuffer = new StringBuffer("[");
		
		
		boolean first = true;
		String quotedPackage= "'"+packageName+ "'";
		String packageFqn = fqn.transformFQN("fqn("+quotedPackage+")");
		args = new String []{packageFqn, quotedPackage};

		writer.writeFact("packageT", args);
		
		
		
		if (targetClass.getDeclaringType() != null){
			IType owningClass = targetClass.getDeclaringType();
			owner = idManager.getID(owningClass);
		} else 
			owner = packageFqn;
		
		name = getSimpleName(targetClass.getElementName());
		first = true;

		member = targetClass.getFields();
	
		
		for (int i = 0; i < member.length; i++){
			IField f = (IField) member[i];
			
			if (syntheticOrPrivate(f.getFlags()))
				continue;
			
			if (!first)
				defBuffer.append(", ");
			else
				first = false;
			
			defBuffer.append(idManager.getID(f));
		}
		
		member = targetClass.getMethods();
	
		
		for (int i = 0; i < member.length; i++){
			IMethod m = (IMethod) member[i];
		
			if (syntheticOrPrivate(m.getFlags()))
				continue;
			if (m.getElementName().equals("<clinit>"))
				continue;
				
			if (!first)
				defBuffer.append(", ");
			else
				first = false;
			
			defBuffer.append(idManager.getID(m));
		}
		
		
		member = targetClass.getTypes();
		
		
		for (int i = 0; i < member.length; i++){
			IType c = (IType) member[i];
			
//			if (syntheticOrPrivate(c.getFlags()))
			if (Flags.isSynthetic(c.getFlags()))				
				continue;
			
			if (!first)
				defBuffer.append(", ");
			else
				first = false;
			
			defBuffer.append(idManager.getID(c));
		}
		
		
		defBuffer.append("]");
		
		args = new String [] {
				id,
				owner,
				name,
				defBuffer.toString()
		};
		//IAdaptable a;
		writer.writeFact("classDefT", args);
		writer.writeFact("externT", new String [] {id});
		if (targetClass.getSuperclassName() != null) {
			String resolvedType = idManager.resolveType(targetClass.getSuperclassName());
			resolvedType = fqn.transformFQN(resolvedType);
			writer.writeFact("extendsT",new String[] {id, resolvedType});
		}
		for (int i = 0; i < targetClass.getSuperInterfaceNames().length; i++) {
			String resolvedType = idManager.resolveType(targetClass.getSuperInterfaceNames()[i]);
			resolvedType = fqn.transformFQN(resolvedType);
			writer.writeFact("implementsT",new String[] {id, resolvedType});
			//resolvedType = idManager.resolveType(targetClass.getSuperInterfaceNames()[i]);
		}
		writeModifiers(id, targetClass.getFlags());
	}

	/**
	 * @param m
	 * @return
	 * @throws JavaModelException
	 */
	private boolean syntheticOrPrivate(int flags) throws JavaModelException {
		return Flags.isPrivate(flags) || Flags.isSynthetic(flags);
	}

	private String getSimpleName(String classname){
		try {
			classname = classname.substring(classname.lastIndexOf(".") + 1);
		} catch (IndexOutOfBoundsException e) {

		}
		return "'" +classname + "'";
	}
	
	private void writeModifiers(String id, int i) {
		if ((Flags.AccInterface & i) > 0)
			writer.writeFact("interfaceT", new String [] {id});
		if ((Flags.AccAbstract & i) > 0)
			writer.writeFact("modifierT", new String [] {id, "'abstract'"});
		if ((Flags.AccFinal & i) > 0)
			writer.writeFact("modifierT", new String [] {id, "'final'"});
		if ((Flags.AccNative & i) > 0)
			writer.writeFact("modifierT", new String [] {id, "'native'"});
		if ((Flags.AccPrivate & i) > 0)
			writer.writeFact("modifierT", new String [] {id, "'private'"});
		if ((Flags.AccProtected & i) > 0)
			writer.writeFact("modifierT", new String [] {id, "'protected'"});
		if ((Flags.AccPublic & i) > 0)
			writer.writeFact("modifierT", new String [] {id, "'public'"});
		if ((Flags.AccStatic & i) > 0)
			writer.writeFact("modifierT", new String [] {id, "'static'"});
		if ((Flags.AccStrictfp & i) > 0)
			writer.writeFact("modifierT", new String [] {id, "'strictfp'"});
		if ((Flags.AccSynchronized & i) > 0)
			writer.writeFact("modifierT", new String [] {id, "'synchronized'"});
		if ((Flags.AccTransient & i) > 0)
			writer.writeFact("modifierT", new String [] {id, "'transient'"});
		if ((Flags.AccVolatile & i) > 0)
			writer.writeFact("modifierT", new String [] {id, "'volatile'"});
		if ((Flags.AccSynthetic & i) > 0)
			writer.writeFact("modifierT", new String [] {id, "'synthetic'"});
		if ((Flags.AccDeprecated & i) > 0)
			writer.writeFact("modifierT", new String [] {id, "'deprecated'"});
	}
	/**
	 * 
	 * @param method the enclosing method, if it exists. Otherwise null.
	 * @param s
	 * @return
	 * @throws JavaModelException
	 */
	private String typeForClass(IMethod method, String s) throws JavaModelException{
		
		String kind;

		int dim = IDManagerIType.getArrayDim(s);
		String vmtypename = s.substring(dim);
		String name;        
//		System.out.println(""+JavaCore.getDefaultOptions());

		String type = idManager.getTypeName(method, vmtypename);
		
		if(IDManagerIType.isTypeParameter(vmtypename)) {
			// FIXME: in the future change this to
			// kind = "typevar";
			//name = "'" + type + "'";

			kind = "class";
			name = "fqn('" + type + "')";
			name = fqn.transformFQN(name);
		} else if(IDManagerIType.isPrimitive(vmtypename)) {
			kind = "basic";
			name = "'" + type + "'";
		} else {
			kind = "class";
			name = "fqn('" + type + "')";
			name = fqn.transformFQN(name);
		}
		
		return "type("+kind+"," + name + ", " + dim+")";
	}
}
