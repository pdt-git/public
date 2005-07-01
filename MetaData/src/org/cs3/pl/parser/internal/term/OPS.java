package org.cs3.pl.parser.internal.term;

import java.util.HashMap;
import java.util.Stack;

public class OPS {
	public final static int PREC_MIN=0;
	public final static int PREC_MAX=1200;
	public final static int PREC_INVALID=-1;
	
	public void clear() {
		ops.clear();
	}
	
	public  void pushReset() {
		ops.push(null);
	}
	public void pushInfixOp(String image){
		if(image.startsWith("'")){
			image=image.substring(1,image.length()-1);
		}
		if(image==null){
			ops.push(null);
		}
		else{
			ops.push(infix_map.get(image));
		}
	}
	public void pushPrefixOp(String image){
		if(image==null){
			ops.push(null);
		}		
		else{
			if(image.startsWith("'")){
				image=image.substring(1,image.length()-1);
			}
			ops.push(prefix_map.get(image));
		}
	}
	public void popOp(){
		ops.pop();
	}
	public int peekLHPrec(){
		int[] p = (int[])ops.peek();
		if(p==null){
			return PREC_MAX;
		}
		if(p.length==3){ //infix
			return p[1];
		}
		return PREC_INVALID;
	}
	
	public int peekRHPrec(){
		if(ops.isEmpty()){
			return PREC_MAX;
		}
		int[] p = (int[])ops.peek();
		if(p==null){
			return PREC_MAX;
		}
		if(p.length==3){ //infix
			return p[2];
		}
		if(p.length==2){ //prefix
			return p[1];
		}
		return PREC_INVALID;
	}
	
	public int lookupInfixPrec(String image){
		if(image.startsWith("'")){
			image=image.substring(1,image.length()-1);
		}
		int[] p=(int[]) infix_map.get(image);
		if(p==null){
			return PREC_INVALID;
		}
		
		return p[0];
	}
	
	public int lookupPrefixPrec(String image){
		if(image.startsWith("'")){
			image=image.substring(1,image.length()-1);
		}
		int[] p=(int[]) prefix_map.get(image);
		if(p==null){
			return PREC_INVALID;
		}
		return p[0];
	}
	public boolean isInfixOp(String image){
		if(image.startsWith("'")&&image.length()>2){
			image=image.substring(1,image.length()-1);
		}
		return lookupInfixPrec(image)!=PREC_INVALID;
	}
	
	public boolean isPrefixOp(String image){
		return lookupPrefixPrec(image)!=PREC_INVALID;
	}
	
	public boolean isValidRHInfixOp(String image,int lhprec){
		if(image.startsWith("'")){
			image=image.substring(1,image.length()-1);
		}
		//System.err.println("isValidRhInfixOp: "+image);
		int op=lookupInfixPrec(image);
	//	System.err.println("  precedence is "+op);
//		is image an Infix Operator?
		if (op==PREC_INVALID){ 
			return false;
		}
//		is op within the right hand bound of the top operator?
		int topRHBound = peekRHPrec();
		//System.err.println("  top rh bound is "+topRHBound);
		if(op>topRHBound){
			return false;
		}
		 
	//	System.err.println("  lhprec is "+lhprec);
		int lhBound = lookupInfixLHBound(image);
		//System.err.println("  lhbound is "+lhBound);
		
		if(lhprec>lhBound){
			return false;
		}
		//System.err.println("ok, we can use "+image);
		return true;
	}
	
	public boolean isValidRHPrefixOp(String image, String nextImage){
		if(image.startsWith("'")){
			image=image.substring(1,image.length()-1);
		}
		int op=lookupPrefixPrec(image);
		
//		is image a Prefix Operator?
		if (op==PREC_INVALID){ 
			return false;
		}
//		is op within the right hand bound of the top operator?
		if(op>peekRHPrec()){
			return false;
		}
		/*
		 * special rule: what if the next token is a climbing infix op?
		 * 
		 * In the swi source code, this is realized by first assuming a 
		 * prefix op, and later, when it turns out, that the next token is 
		 * an infix op that climbs over the prefix op, this prefix op is
		 * converted to an atom.
		 * 
		 * We cannot do this the a posteriori way, as with our current gramma
		 * the infix op would not be accepted once the parser decided that the
		 * preciding prefix op is a prefix op.
		 * 
		 * Instead we simulate the acceptance of the prefix op within a 
		 * lookahead step. (righ here)
		 */

		
		//return true;
		
		
		if(isValidRHInfixOp(nextImage,PREC_MIN)){
			return false;
		}
		
		return true;
	}
	
	public int lookupInfixLHBound(String image) {
		if(image.startsWith("'")){
			image=image.substring(1,image.length()-1);
		}
		int[] p = (int[]) infix_map.get(image);
		if(p==null){
			return PREC_INVALID;
		}
		return p[1];
	}
	
	public int lookupInfixRHBound(String image) {
		if(image.startsWith("'")){
			image=image.substring(1,image.length()-1);
		}
		int[] p = (int[]) infix_map.get(image);
		if(p==null){
			return PREC_INVALID;
		}
		return p[2];
	}
	
	public int lookupPrefixRHBound(String image) {
		if(image.startsWith("'")){
			image=image.substring(1,image.length()-1);
		}
		int[] p = (int[]) prefix_map.get(image);
		if(p==null){
			return PREC_INVALID;
		}
		return p[1];
	}
	
	
	
	HashMap infix_map=new HashMap();
	HashMap prefix_map=new HashMap();
	Stack ops=new Stack();
	public OPS(){
		infix_map.put("<",new int[]{700,700-1,700-1});
		infix_map.put(">",new int[]{700,700-1,700-1});
		infix_map.put("\\==",new int[]{700,700-1,700-1});
		infix_map.put(">=",new int[]{700,700-1,700-1});
		infix_map.put("=<",new int[]{700,700-1,700-1});
		infix_map.put("**",new int[]{200,200-1,200-1});
		infix_map.put(":=",new int[]{990,990-1,990-1});
		infix_map.put("\\=",new int[]{700,700-1,700-1});
		infix_map.put("=..",new int[]{700,700-1,700-1});
		infix_map.put("==",new int[]{700,700-1,700-1});
		infix_map.put("=:=",new int[]{700,700-1,700-1});
		infix_map.put("=\\=",new int[]{700,700-1,700-1});
		infix_map.put("@>=",new int[]{700,700-1,700-1});
		infix_map.put("\\=@=",new int[]{700,700-1,700-1});
		infix_map.put("-->",new int[]{1200,1200-1,1200-1});
		infix_map.put("@<",new int[]{700,700-1,700-1});
		infix_map.put("@=<",new int[]{700,700-1,700-1});
		infix_map.put(":-",new int[]{1200,1200-1,1200-1});
		infix_map.put("is",new int[]{700,700-1,700-1});
		infix_map.put("=",new int[]{700,700-1,700-1});
		infix_map.put("=@=",new int[]{700,700-1,700-1});
		infix_map.put("@>",new int[]{700,700-1,700-1});
		infix_map.put("xor",new int[]{400,400,400-1});
		infix_map.put("-",new int[]{500,500,500-1});
		infix_map.put("mod",new int[]{400,400,400-1});
		infix_map.put("//",new int[]{400,400,400-1});
		infix_map.put("?",new int[]{150,150,150-1});
		infix_map.put("/",new int[]{400,400,400-1});
		infix_map.put("*",new int[]{400,400,400-1});
		infix_map.put(">>",new int[]{400,400,400-1});
		infix_map.put("/\\",new int[]{500,500,500-1});
		infix_map.put("rem",new int[]{400,400,400-1});
		infix_map.put("+",new int[]{500,500,500-1});
		infix_map.put("<<",new int[]{400,400,400-1});
		infix_map.put("\\/",new int[]{500,500,500-1});
		infix_map.put("^",new int[]{200,200-1,200});
		infix_map.put("|",new int[]{1100,1100-1,1100});
		infix_map.put("->",new int[]{1050,1050-1,1050});
		infix_map.put("*->",new int[]{1050,1050-1,1050});
		infix_map.put(":",new int[]{600,600-1,600});
		infix_map.put(",",new int[]{1000,1000-1,1000});
		infix_map.put(";",new int[]{1100,1100-1,1100});
		prefix_map.put("\\+",new int[]{900,900});
		prefix_map.put("@",new int[]{100,100-1});
		prefix_map.put("-",new int[]{500,500-1});
		prefix_map.put("meta_predicate",new int[]{1150,1150-1});
		prefix_map.put("discontiguous",new int[]{1150,1150-1});
		prefix_map.put("thread_local",new int[]{1150,1150-1});
		prefix_map.put("?",new int[]{500,500-1});
		prefix_map.put("+",new int[]{500,500-1});
		prefix_map.put("\\",new int[]{500,500-1});
		prefix_map.put("dynamic",new int[]{1150,1150-1});
		prefix_map.put("$",new int[]{1,1-1});
		prefix_map.put("initialization",new int[]{1150,1150-1});
		prefix_map.put("?-",new int[]{1200,1200-1});
		prefix_map.put("multifile",new int[]{1150,1150-1});
		prefix_map.put(":-",new int[]{1200,1200-1});
		prefix_map.put("volatile",new int[]{1150,1150-1});
		prefix_map.put("module_transparent",new int[]{1150,1150-1});
	}	
	
}
