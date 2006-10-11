/*
 * tuProlog - Copyright (C) 2001-2002  aliCE team at deis.unibo.it
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package alice.tuprolog;

import java.lang.reflect.*;

/**
 * BuiltIn class
 * referring to a builtin predicate or functor
 *
 * @see Struct
 *
 *
 *
 */
class BuiltIn {

    static final int LIBRARY_PREDICATE = -100;
    static final int LIBRARY_FUNCTOR   = -200;

    /**
     *  identify the buit-in type:
     *  code == LIBRARY_PREDICATE -> builtin predicate defined in a library
     *  code == LIBRARY_FUNCTOR   -> builtin fucntor   defined in a library
     *  code > 0                  -> builtin resolved by the core
     */
    private int code;
    /** method to be call when evaluating the built-in*/
    private Method method;
    /** lib object where the builtin is defined */
    private Library library;
    /** for optimization purposes */
    private Term[] builtin_args;
	private Struct builtin_struct;

    /** Buils a built in whose behaviour is defined in the core engine */
    public BuiltIn(int code){
        this.code=code;
    }

    /** Buils a built in whose behaviour is defined in a Library */
    public BuiltIn(int type,Struct s,Library lib) throws NoSuchMethodException {
        this.code = type;
        library=lib;
        //
		method=library.getLinkedMethod(s);
		if (method==null){
			throw new NoSuchMethodException();
		}
		//
        builtin_struct = s;
        builtin_args=new Term[s.getArity()];
        
        /*
    	for (int i=0; i<builtin_args.length; i++){
        	builtin_args[i]=s.getArg(i);
        }*/
    }

	/**
	 * Creates a copy of this builtins, linking
	 * it to the arguments of a new structure. 
	 * 
	 * @param ns
	 * @return
	 */
    public BuiltIn getCopy(Struct ns){
        BuiltIn bt=new BuiltIn(code);
        bt.library=library;
        bt.method=method;
		bt.builtin_struct = ns;
		bt.builtin_args=new Term[ns.getArity()];
		/*
		for (int i=0; i<ns.getArity(); i++){
			bt.builtin_args[i]=ns.getArg(i);
		}*/
        return bt;
    }

    /**
     * evaluates the builtin as a predicate
     */
    public boolean evalAsPredicate(){
        try {
			// be sure that bound var are treated properly
			//for (int i=0; i<builtin_args.length; i++){
			//	builtin_args[i]=builtin_args[i].getTerm();
			//}
			for (int i=0; i<builtin_args.length; i++){
				builtin_args[i]=builtin_struct.getTerm(i);
			}
            return ((Boolean)method.invoke(library,builtin_args)).booleanValue();
        } catch (Exception ex){
            ex.printStackTrace();
            return false;
        }
    }

    /**
     * evaluates the builtin as a functor
     */
    public Term evalAsFunctor(){
        try {
        	// be sure that bound var are treated properly
        	//for (int i=0; i<builtin_args.length; i++){
        	//	builtin_args[i]=builtin_args[i].getTerm();
        	//}
			for (int i=0; i<builtin_args.length; i++){
				builtin_args[i]=builtin_struct.getTerm(i);
			}
            return ((Term)method.invoke(library,builtin_args));
        } catch (Exception ex){
            return null;
        }
    }

    /** gets the code of the builtin defined in the core*/
    public int getCode() {
        return code;
    }

    /**
     * tests if the builtin behaviour is defined in the core
     */
    public boolean isCorePredicate(){
        return code!=LIBRARY_PREDICATE && code!=LIBRARY_FUNCTOR;
    }

    /**
     * tests if the builtin behaviour is defined in a library as predicate
     */
    public boolean isLibraryPredicate(){
        return code==LIBRARY_PREDICATE;
    }

    /**
     * tests if the builtin behaviour is defined in a library as functor
     */
    public boolean isLibraryFunctor(){
        return code==LIBRARY_FUNCTOR;
    }

    public String toString(){
        if (isLibraryFunctor()||isLibraryPredicate()){
            return "[ built-in: method "+method.getName()+" - "+builtin_args+" - N args: "+builtin_args.length+" - "+library.getClass().getName()+" ]\n";
        } else {
            return "[ built-in: code "+code+" ]";
        }
    }
}