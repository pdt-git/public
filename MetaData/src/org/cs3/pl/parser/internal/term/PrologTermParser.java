/* Generated By:JJTree&JavaCC: Do not edit this line. PrologTermParser.java */
package org.cs3.pl.parser.internal.term;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import java.util.ArrayList;

class PrologTermParser/*@bgen(jjtree)*/implements PrologTermParserTreeConstants, PrologTermParserConstants {/*@bgen(jjtree)*/
  protected JJTPrologTermParserState jjtree = new JJTPrologTermParserState();public OPS ops = new OPS();

        boolean test(Token t1, Token t2) {
                return t1.beginOffset == t2.endOffset;
//		System.err.println("begin: "+t1.image +","+ t1.beginOffset + ", end: "+ t2.endOffset);
//		return true;
        }

        void jjtreeOpenNodeScope(Node n)
    {
        Token t = getToken(1);
        //System.err.prinln("open node of type "+n);
        //System.err.prinln("   first token: "+t.image+" at line "+t.beginLine+", column "+t.beginColumn+".");
      ((SimpleNode)n).setFirstToken(t);

    }

    void jjtreeCloseNodeScope(Node n)
    {
      Token t = getToken(0);
        //System.err.prinln("closing node of type "+n);
        //System.err.prinln("   last token: "+t.image+" at line "+t.beginLine+", column "+t.beginColumn+".");      
      ((SimpleNode)n).setLastToken(t);



                //need to "manualy" figure out the start token, as node scope hooks are
                //called "to late" for nodes like ASTInfixTerm 
                //if the nodes children have already been poped, this should work:
                SimpleNode s = (SimpleNode)n;
                if(s.jjtGetNumChildren()>0){
                         SimpleNode firstChild = (SimpleNode)s.jjtGetChild(0);
                         s.setFirstToken(firstChild.getFirstToken());
                }
                //System.err.println("created: "+ s+" : "+s.getImage());


                if(n instanceof ASTInfixTerm){
                 ((ASTInfixTerm)n).flatten();

          }
    }
        private List errors = new ArrayList();

        public List getErrors() {
                return errors;
        }
        public void error_skipto(int kind) {
          ParseException e = generateParseException();
          errors.add(e);
          Token t;
          do {
            t = getNextToken();
          } while (t.kind != kind && t.kind != EOF);
        }
        public ASTCompilationUnit getASTRoot(){
                return (ASTCompilationUnit) jjtree.rootNode();
        }
  public static void main(String args[]) throws FileNotFoundException {
        InputStream stream = null;
        if(args==null||args.length==0){
            //System.err.println("Reading from standard input...");
            stream=System.in;
        }
        else{
                stream=PrologTermParser.class.getResourceAsStream(args[0]);
        }
    PrologTermParser t = new PrologTermParser(stream);
    try {
      SimpleNode n = t.CompilationUnit();
      n.dump("");
      System.err.println("Thank you.");
    } catch (Exception e) {
      System.err.println("Oops.");
      System.err.println(e.getMessage());
      e.printStackTrace();
    }
  }

  final public SimpleNode CompilationUnit() throws ParseException {
                                /*@bgen(jjtree) CompilationUnit */
  ASTCompilationUnit jjtn000 = new ASTCompilationUnit(this, JJTCOMPILATIONUNIT);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      label_1:
      while (true) {
        if (jj_2_1(1)) {
          ;
        } else {
          break label_1;
        }
        try {
          Member();
        } catch (ParseException e) {
                e.printStackTrace();
            error_skipto(DOT);
            //pop nodes from the stack down to the last
            //successfully parsed term
                ops.clear();
                while(jjtree.nodeArity()>0 &&!(jjtree.peekNode() instanceof ASTMember)){

                        //System.err.println("popped: "+jjtree.popNode());
                }
//		System.err.println("next i will read: "+getToken(1));

        }
      }
      jj_consume_token(0);
    jjtree.closeNodeScope(jjtn000, true);
    jjtc000 = false;
    jjtreeCloseNodeScope(jjtn000);
    {if (true) return jjtn000;}
    } catch (Throwable jjte000) {
    if (jjtc000) {
      jjtree.clearNodeScope(jjtn000);
      jjtc000 = false;
    } else {
      jjtree.popNode();
    }
    if (jjte000 instanceof RuntimeException) {
      {if (true) throw (RuntimeException)jjte000;}
    }
    if (jjte000 instanceof ParseException) {
      {if (true) throw (ParseException)jjte000;}
    }
    {if (true) throw (Error)jjte000;}
    } finally {
    if (jjtc000) {
      jjtree.closeNodeScope(jjtn000, true);
      jjtreeCloseNodeScope(jjtn000);
    }
    }
    throw new Error("Missing return statement in function");
  }

  final public void Member() throws ParseException {
               /*@bgen(jjtree) Member */
  ASTMember jjtn000 = new ASTMember(this, JJTMEMBER);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      Term();
      jj_consume_token(DOT);
    } catch (Throwable jjte000) {
          if (jjtc000) {
            jjtree.clearNodeScope(jjtn000);
            jjtc000 = false;
          } else {
            jjtree.popNode();
          }
          if (jjte000 instanceof RuntimeException) {
            {if (true) throw (RuntimeException)jjte000;}
          }
          if (jjte000 instanceof ParseException) {
            {if (true) throw (ParseException)jjte000;}
          }
          {if (true) throw (Error)jjte000;}
    } finally {
          if (jjtc000) {
            jjtree.closeNodeScope(jjtn000, true);
            jjtreeCloseNodeScope(jjtn000);
          }
    }
  }

  final public void Term() throws ParseException {
 String opImage;
    PrefixBoundTerm();
    label_2:
    while (true) {
      if (ops.isValidRHInfixOp(getToken(1).image,(SimpleNode)jjtree.peekNode())) {
        ;
      } else {
        break label_2;
      }
      InfixOperator();
                        opImage=getToken(0).image;
                        ops.pushInfixOp(opImage);
      Term();
                  ASTInfixTerm jjtn001 = new ASTInfixTerm(this, JJTINFIXTERM);
                  boolean jjtc001 = true;
                  jjtree.openNodeScope(jjtn001);
                  jjtreeOpenNodeScope(jjtn001);
      try {
                  jjtree.closeNodeScope(jjtn001,  3);
                  jjtc001 = false;
                  jjtreeCloseNodeScope(jjtn001);
                        ops.popOp();
      } finally {
                  if (jjtc001) {
                    jjtree.closeNodeScope(jjtn001,  3);
                    jjtreeCloseNodeScope(jjtn001);
                  }
      }
    }
  }

  final public void PrefixBoundTerm() throws ParseException {
    if (jj_2_2(2147483647) && (test(getToken(2),getToken(1)) &&
                    !",".equals(getToken(1).image))) {
      CompoundTerm();
    } else if (jj_2_3(2147483647) && (ops.isValidRHPrefixOp(getToken(1).image)
                                    &&  ( ops.isPrefixOp(getToken(2).image)
                                                    &&  ops.lookupPrefixRHBound(getToken(1).image)
                                                        >=ops.lookupPrefixPrec(getToken(2).image)
                                            ||ops.isInfixOp(getToken(2).image)
                                                    &&      ops.lookupInfixLHBound(getToken(2).image)
                                                            <ops.lookupPrefixPrec(getToken(1).image)
                                            || !ops.isInfixOp(getToken(2).image)
                                                    && ! ops.isPrefixOp(getToken(2).image)
                                            ))) {
      PrefixOperator();
                                ops.pushPrefixOp(getToken(0).image);
      Term();
                          ASTPrefixTerm jjtn001 = new ASTPrefixTerm(this, JJTPREFIXTERM);
                          boolean jjtc001 = true;
                          jjtree.openNodeScope(jjtn001);
                          jjtreeOpenNodeScope(jjtn001);
      try {
                          jjtree.closeNodeScope(jjtn001,  2);
                          jjtc001 = false;
                          jjtreeCloseNodeScope(jjtn001);
                                ops.popOp();
      } finally {
                          if (jjtc001) {
                            jjtree.closeNodeScope(jjtn001,  2);
                            jjtreeCloseNodeScope(jjtn001);
                          }
      }
    } else {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case 29:
        ParanthesisTerm();
        break;
      case 25:
        BracesTerm();
        break;
      case 27:
        ListTerm();
        break;
      case OPERATOR:
      case CHARACTER_ATOM:
      case IDENTIFIER:
      case CUT:
      case DOT:
        Atom();
        break;
      case DECIMAL_LITERAL:
      case BIN_LITERAL:
      case OCT_LITERAL:
      case HEX_LITERAL:
      case FLOATING_POINT_LITERAL:
        Number();
        break;
      case STRING_LITERAL:
        String();
        break;
      case VARIABLE:
        Variable();
        break;
      default:
        jj_la1[0] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
  }

  final public void BracesTerm() throws ParseException {
 /*@bgen(jjtree) BracesTerm */
  ASTBracesTerm jjtn000 = new ASTBracesTerm(this, JJTBRACESTERM);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      jj_consume_token(25);
         ops.pushReset();
      Term();
         ops.popOp();
      jj_consume_token(26);
    } catch (Throwable jjte000) {
          if (jjtc000) {
            jjtree.clearNodeScope(jjtn000);
            jjtc000 = false;
          } else {
            jjtree.popNode();
          }
          if (jjte000 instanceof RuntimeException) {
            {if (true) throw (RuntimeException)jjte000;}
          }
          if (jjte000 instanceof ParseException) {
            {if (true) throw (ParseException)jjte000;}
          }
          {if (true) throw (Error)jjte000;}
    } finally {
          if (jjtc000) {
            jjtree.closeNodeScope(jjtn000, true);
            jjtreeCloseNodeScope(jjtn000);
          }
    }
  }

  final public void ListTerm() throws ParseException {
 /*@bgen(jjtree) ListTerm */
  ASTListTerm jjtn000 = new ASTListTerm(this, JJTLISTTERM);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      jj_consume_token(27);
         ops.pushReset();
      if (jj_2_4(1)) {
        Term();
      } else {
        ;
      }
         ops.popOp();
      jj_consume_token(28);
    } catch (Throwable jjte000) {
          if (jjtc000) {
            jjtree.clearNodeScope(jjtn000);
            jjtc000 = false;
          } else {
            jjtree.popNode();
          }
          if (jjte000 instanceof RuntimeException) {
            {if (true) throw (RuntimeException)jjte000;}
          }
          if (jjte000 instanceof ParseException) {
            {if (true) throw (ParseException)jjte000;}
          }
          {if (true) throw (Error)jjte000;}
    } finally {
          if (jjtc000) {
            jjtree.closeNodeScope(jjtn000, true);
            jjtreeCloseNodeScope(jjtn000);
          }
    }
  }

  final public void ParanthesisTerm() throws ParseException {
 /*@bgen(jjtree) ParanthesisTerm */
  ASTParanthesisTerm jjtn000 = new ASTParanthesisTerm(this, JJTPARANTHESISTERM);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      jj_consume_token(29);
         ops.pushReset();
      Term();
         ops.popOp();
      jj_consume_token(30);
    } catch (Throwable jjte000) {
          if (jjtc000) {
            jjtree.clearNodeScope(jjtn000);
            jjtc000 = false;
          } else {
            jjtree.popNode();
          }
          if (jjte000 instanceof RuntimeException) {
            {if (true) throw (RuntimeException)jjte000;}
          }
          if (jjte000 instanceof ParseException) {
            {if (true) throw (ParseException)jjte000;}
          }
          {if (true) throw (Error)jjte000;}
    } finally {
          if (jjtc000) {
            jjtree.closeNodeScope(jjtn000, true);
            jjtreeCloseNodeScope(jjtn000);
          }
    }
  }

  final public void CompoundTerm() throws ParseException {
 /*@bgen(jjtree) CompoundTerm */
  ASTCompoundTerm jjtn000 = new ASTCompoundTerm(this, JJTCOMPOUNDTERM);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      Atom();
      jj_consume_token(29);
         ops.pushReset();
      Term();
         ops.popOp();
      jj_consume_token(30);
    } catch (Throwable jjte000) {
          if (jjtc000) {
            jjtree.clearNodeScope(jjtn000);
            jjtc000 = false;
          } else {
            jjtree.popNode();
          }
          if (jjte000 instanceof RuntimeException) {
            {if (true) throw (RuntimeException)jjte000;}
          }
          if (jjte000 instanceof ParseException) {
            {if (true) throw (ParseException)jjte000;}
          }
          {if (true) throw (Error)jjte000;}
    } finally {
          if (jjtc000) {
            jjtree.closeNodeScope(jjtn000, true);
            jjtreeCloseNodeScope(jjtn000);
          }
    }
  }

  final public void Atom() throws ParseException {
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case CUT:
      Cut();
      break;
    case OPERATOR:
    case IDENTIFIER:
    case DOT:
      Identifier();
      break;
    case CHARACTER_ATOM:
      Characters();
      break;
    default:
      jj_la1[1] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
  }

  final public void Cut() throws ParseException {
             /*@bgen(jjtree) Cut */
  ASTCut jjtn000 = new ASTCut(this, JJTCUT);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      jj_consume_token(CUT);
    } finally {
          if (jjtc000) {
            jjtree.closeNodeScope(jjtn000, true);
            jjtreeCloseNodeScope(jjtn000);
          }
    }
  }

  final public void Number() throws ParseException {
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case DECIMAL_LITERAL:
    case BIN_LITERAL:
    case OCT_LITERAL:
    case HEX_LITERAL:
      Integer();
      break;
    case FLOATING_POINT_LITERAL:
      Float();
      break;
    default:
      jj_la1[2] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
  }

  final public void String() throws ParseException {
                /*@bgen(jjtree) String */
  ASTString jjtn000 = new ASTString(this, JJTSTRING);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      jj_consume_token(STRING_LITERAL);
    } finally {
          if (jjtc000) {
            jjtree.closeNodeScope(jjtn000, true);
            jjtreeCloseNodeScope(jjtn000);
          }
    }
  }

  final public void Variable() throws ParseException {
                  /*@bgen(jjtree) Variable */
  ASTVariable jjtn000 = new ASTVariable(this, JJTVARIABLE);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      jj_consume_token(VARIABLE);
    } finally {
          if (jjtc000) {
            jjtree.closeNodeScope(jjtn000, true);
            jjtreeCloseNodeScope(jjtn000);
          }
    }
  }

  final public void Identifier() throws ParseException {
                     /*@bgen(jjtree) Identifier */
  ASTIdentifier jjtn000 = new ASTIdentifier(this, JJTIDENTIFIER);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case OPERATOR:
        jj_consume_token(OPERATOR);
        break;
      case IDENTIFIER:
        jj_consume_token(IDENTIFIER);
        break;
      case DOT:
        jj_consume_token(DOT);
        break;
      default:
        jj_la1[3] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    } finally {
          if (jjtc000) {
            jjtree.closeNodeScope(jjtn000, true);
            jjtreeCloseNodeScope(jjtn000);
          }
    }
  }

  final public void Integer() throws ParseException {
                  /*@bgen(jjtree) Integer */
  ASTInteger jjtn000 = new ASTInteger(this, JJTINTEGER);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case DECIMAL_LITERAL:
        jj_consume_token(DECIMAL_LITERAL);
        break;
      case BIN_LITERAL:
        jj_consume_token(BIN_LITERAL);
        break;
      case OCT_LITERAL:
        jj_consume_token(OCT_LITERAL);
        break;
      case HEX_LITERAL:
        jj_consume_token(HEX_LITERAL);
        break;
      default:
        jj_la1[4] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    } finally {
    if (jjtc000) {
      jjtree.closeNodeScope(jjtn000, true);
      jjtreeCloseNodeScope(jjtn000);
    }
    }
  }

  final public void Float() throws ParseException {
               /*@bgen(jjtree) Float */
  ASTFloat jjtn000 = new ASTFloat(this, JJTFLOAT);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      jj_consume_token(FLOATING_POINT_LITERAL);
    } finally {
          if (jjtc000) {
            jjtree.closeNodeScope(jjtn000, true);
            jjtreeCloseNodeScope(jjtn000);
          }
    }
  }

  final public void Characters() throws ParseException {
                    /*@bgen(jjtree) Characters */
  ASTCharacters jjtn000 = new ASTCharacters(this, JJTCHARACTERS);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      jj_consume_token(CHARACTER_ATOM);
    } finally {
          if (jjtc000) {
            jjtree.closeNodeScope(jjtn000, true);
            jjtreeCloseNodeScope(jjtn000);
          }
    }
  }

  final public void PrefixOperator() throws ParseException {
                        /*@bgen(jjtree) PrefixOperator */
  ASTPrefixOperator jjtn000 = new ASTPrefixOperator(this, JJTPREFIXOPERATOR);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      jj_consume_token(OPERATOR);
          jjtree.closeNodeScope(jjtn000, true);
          jjtc000 = false;
          jjtreeCloseNodeScope(jjtn000);
                Token t = getToken(0);
//  	System.err.println("Operator "+t.image+" at line "+t.beginLine+" col "+t.beginColumn+" was interpreted as Prefix Operator.");

    } finally {
          if (jjtc000) {
            jjtree.closeNodeScope(jjtn000, true);
            jjtreeCloseNodeScope(jjtn000);
          }
    }
  }

  final public void InfixOperator() throws ParseException {
                       /*@bgen(jjtree) InfixOperator */
  ASTInfixOperator jjtn000 = new ASTInfixOperator(this, JJTINFIXOPERATOR);
  boolean jjtc000 = true;
  jjtree.openNodeScope(jjtn000);
  jjtreeOpenNodeScope(jjtn000);
    try {
      jj_consume_token(OPERATOR);
          jjtree.closeNodeScope(jjtn000, true);
          jjtc000 = false;
          jjtreeCloseNodeScope(jjtn000);
                Token t = getToken(0);
//  	System.err.println("Operator "+t.image+" at line "+t.beginLine+" col "+t.beginColumn+" was interpreted as Infix Operator.");

    } finally {
          if (jjtc000) {
            jjtree.closeNodeScope(jjtn000, true);
            jjtreeCloseNodeScope(jjtn000);
          }
    }
  }

  final private boolean jj_2_1(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_1(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(0, xla); }
  }

  final private boolean jj_2_2(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_2(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(1, xla); }
  }

  final private boolean jj_2_3(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_3(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(2, xla); }
  }

  final private boolean jj_2_4(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_4(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(3, xla); }
  }

  final private boolean jj_3R_34() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_scan_token(8)) {
    jj_scanpos = xsp;
    if (jj_scan_token(9)) {
    jj_scanpos = xsp;
    if (jj_scan_token(10)) {
    jj_scanpos = xsp;
    if (jj_scan_token(11)) return true;
    }
    }
    }
    return false;
  }

  final private boolean jj_3R_29() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_scan_token(6)) {
    jj_scanpos = xsp;
    if (jj_scan_token(17)) {
    jj_scanpos = xsp;
    if (jj_scan_token(24)) return true;
    }
    }
    return false;
  }

  final private boolean jj_3R_8() {
    if (jj_3R_4()) return true;
    return false;
  }

  final private boolean jj_3R_19() {
    if (jj_3R_30()) return true;
    return false;
  }

  final private boolean jj_3R_33() {
    if (jj_3R_35()) return true;
    return false;
  }

  final private boolean jj_3R_27() {
    if (jj_scan_token(VARIABLE)) return true;
    return false;
  }

  final private boolean jj_3R_5() {
    Token xsp;
    xsp = jj_scanpos;
    lookingAhead = true;
    jj_semLA = test(getToken(2),getToken(1)) &&
                !",".equals(getToken(1).image);
    lookingAhead = false;
    if (!jj_semLA || jj_3R_8()) {
    jj_scanpos = xsp;
    lookingAhead = true;
    jj_semLA = ops.isValidRHPrefixOp(getToken(1).image)
                                &&  ( ops.isPrefixOp(getToken(2).image)
                                                &&  ops.lookupPrefixRHBound(getToken(1).image)
                                                    >=ops.lookupPrefixPrec(getToken(2).image)
                                        ||ops.isInfixOp(getToken(2).image)
                                                &&      ops.lookupInfixLHBound(getToken(2).image)
                                                        <ops.lookupPrefixPrec(getToken(1).image)
                                        || !ops.isInfixOp(getToken(2).image)
                                                && ! ops.isPrefixOp(getToken(2).image)
                                        );
    lookingAhead = false;
    if (!jj_semLA || jj_3R_9()) {
    jj_scanpos = xsp;
    if (jj_3R_10()) {
    jj_scanpos = xsp;
    if (jj_3R_11()) {
    jj_scanpos = xsp;
    if (jj_3R_12()) {
    jj_scanpos = xsp;
    if (jj_3R_13()) {
    jj_scanpos = xsp;
    if (jj_3R_14()) {
    jj_scanpos = xsp;
    if (jj_3R_15()) {
    jj_scanpos = xsp;
    if (jj_3R_16()) return true;
    }
    }
    }
    }
    }
    }
    }
    }
    return false;
  }

  final private boolean jj_3R_26() {
    if (jj_scan_token(STRING_LITERAL)) return true;
    return false;
  }

  final private boolean jj_3R_25() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_32()) {
    jj_scanpos = xsp;
    if (jj_3R_33()) return true;
    }
    return false;
  }

  final private boolean jj_3R_32() {
    if (jj_3R_34()) return true;
    return false;
  }

  final private boolean jj_3R_18() {
    if (jj_3R_29()) return true;
    return false;
  }

  final private boolean jj_3R_28() {
    if (jj_scan_token(CUT)) return true;
    return false;
  }

  final private boolean jj_3R_20() {
    if (jj_3R_31()) return true;
    if (jj_3R_6()) return true;
    return false;
  }

  final private boolean jj_3R_7() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_17()) {
    jj_scanpos = xsp;
    if (jj_3R_18()) {
    jj_scanpos = xsp;
    if (jj_3R_19()) return true;
    }
    }
    return false;
  }

  final private boolean jj_3R_17() {
    if (jj_3R_28()) return true;
    return false;
  }

  final private boolean jj_3R_6() {
    if (jj_3R_5()) return true;
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_20()) { jj_scanpos = xsp; break; }
    }
    return false;
  }

  final private boolean jj_3R_4() {
    if (jj_3R_7()) return true;
    if (jj_scan_token(29)) return true;
    if (jj_3R_6()) return true;
    if (jj_scan_token(30)) return true;
    return false;
  }

  final private boolean jj_3R_3() {
    if (jj_3R_6()) return true;
    return false;
  }

  final private boolean jj_3R_22() {
    if (jj_scan_token(29)) return true;
    if (jj_3R_6()) return true;
    if (jj_scan_token(30)) return true;
    return false;
  }

  final private boolean jj_3_4() {
    if (jj_3R_6()) return true;
    return false;
  }

  final private boolean jj_3R_24() {
    if (jj_scan_token(27)) return true;
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3_4()) jj_scanpos = xsp;
    if (jj_scan_token(28)) return true;
    return false;
  }

  final private boolean jj_3R_31() {
    if (jj_scan_token(OPERATOR)) return true;
    return false;
  }

  final private boolean jj_3R_16() {
    if (jj_3R_27()) return true;
    return false;
  }

  final private boolean jj_3R_15() {
    if (jj_3R_26()) return true;
    return false;
  }

  final private boolean jj_3R_23() {
    if (jj_scan_token(25)) return true;
    if (jj_3R_6()) return true;
    if (jj_scan_token(26)) return true;
    return false;
  }

  final private boolean jj_3R_14() {
    if (jj_3R_25()) return true;
    return false;
  }

  final private boolean jj_3R_13() {
    if (jj_3R_7()) return true;
    return false;
  }

  final private boolean jj_3_1() {
    if (jj_3R_3()) return true;
    return false;
  }

  final private boolean jj_3R_12() {
    if (jj_3R_24()) return true;
    return false;
  }

  final private boolean jj_3R_11() {
    if (jj_3R_23()) return true;
    return false;
  }

  final private boolean jj_3R_10() {
    if (jj_3R_22()) return true;
    return false;
  }

  final private boolean jj_3R_21() {
    if (jj_scan_token(OPERATOR)) return true;
    return false;
  }

  final private boolean jj_3_3() {
    if (jj_scan_token(OPERATOR)) return true;
    if (jj_3R_5()) return true;
    return false;
  }

  final private boolean jj_3R_30() {
    if (jj_scan_token(CHARACTER_ATOM)) return true;
    return false;
  }

  final private boolean jj_3R_35() {
    if (jj_scan_token(FLOATING_POINT_LITERAL)) return true;
    return false;
  }

  final private boolean jj_3R_9() {
    if (jj_3R_21()) return true;
    if (jj_3R_6()) return true;
    return false;
  }

  final private boolean jj_3_2() {
    if (jj_3R_4()) return true;
    return false;
  }

  public PrologTermParserTokenManager token_source;
  SimpleCharStream jj_input_stream;
  public Token token, jj_nt;
  private int jj_ntk;
  private Token jj_scanpos, jj_lastpos;
  private int jj_la;
  public boolean lookingAhead = false;
  private boolean jj_semLA;
  private int jj_gen;
  final private int[] jj_la1 = new int[5];
  static private int[] jj_la1_0;
  static {
      jj_la1_0();
   }
   private static void jj_la1_0() {
      jj_la1_0 = new int[] {0x2b83df40,0x1824040,0x1f00,0x1020040,0xf00,};
   }
  final private JJCalls[] jj_2_rtns = new JJCalls[4];
  private boolean jj_rescan = false;
  private int jj_gc = 0;

  public PrologTermParser(java.io.InputStream stream) {
    jj_input_stream = new SimpleCharStream(stream, 1, 1);
    token_source = new PrologTermParserTokenManager(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 5; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public void ReInit(java.io.InputStream stream) {
    jj_input_stream.ReInit(stream, 1, 1);
    token_source.ReInit(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jjtree.reset();
    jj_gen = 0;
    for (int i = 0; i < 5; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public PrologTermParser(java.io.Reader stream) {
    jj_input_stream = new SimpleCharStream(stream, 1, 1);
    token_source = new PrologTermParserTokenManager(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 5; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public void ReInit(java.io.Reader stream) {
    jj_input_stream.ReInit(stream, 1, 1);
    token_source.ReInit(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jjtree.reset();
    jj_gen = 0;
    for (int i = 0; i < 5; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public PrologTermParser(PrologTermParserTokenManager tm) {
    token_source = tm;
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 5; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public void ReInit(PrologTermParserTokenManager tm) {
    token_source = tm;
    token = new Token();
    jj_ntk = -1;
    jjtree.reset();
    jj_gen = 0;
    for (int i = 0; i < 5; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  final private Token jj_consume_token(int kind) throws ParseException {
    Token oldToken;
    if ((oldToken = token).next != null) token = token.next;
    else token = token.next = token_source.getNextToken();
    jj_ntk = -1;
    if (token.kind == kind) {
      jj_gen++;
      if (++jj_gc > 100) {
        jj_gc = 0;
        for (int i = 0; i < jj_2_rtns.length; i++) {
          JJCalls c = jj_2_rtns[i];
          while (c != null) {
            if (c.gen < jj_gen) c.first = null;
            c = c.next;
          }
        }
      }
      return token;
    }
    token = oldToken;
    jj_kind = kind;
    throw generateParseException();
  }

  static private final class LookaheadSuccess extends java.lang.Error { }
  final private LookaheadSuccess jj_ls = new LookaheadSuccess();
  final private boolean jj_scan_token(int kind) {
    if (jj_scanpos == jj_lastpos) {
      jj_la--;
      if (jj_scanpos.next == null) {
        jj_lastpos = jj_scanpos = jj_scanpos.next = token_source.getNextToken();
      } else {
        jj_lastpos = jj_scanpos = jj_scanpos.next;
      }
    } else {
      jj_scanpos = jj_scanpos.next;
    }
    if (jj_rescan) {
      int i = 0; Token tok = token;
      while (tok != null && tok != jj_scanpos) { i++; tok = tok.next; }
      if (tok != null) jj_add_error_token(kind, i);
    }
    if (jj_scanpos.kind != kind) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) throw jj_ls;
    return false;
  }

  final public Token getNextToken() {
    if (token.next != null) token = token.next;
    else token = token.next = token_source.getNextToken();
    jj_ntk = -1;
    jj_gen++;
    return token;
  }

  final public Token getToken(int index) {
    Token t = lookingAhead ? jj_scanpos : token;
    for (int i = 0; i < index; i++) {
      if (t.next != null) t = t.next;
      else t = t.next = token_source.getNextToken();
    }
    return t;
  }

  final private int jj_ntk() {
    if ((jj_nt=token.next) == null)
      return (jj_ntk = (token.next=token_source.getNextToken()).kind);
    else
      return (jj_ntk = jj_nt.kind);
  }

  private java.util.Vector jj_expentries = new java.util.Vector();
  private int[] jj_expentry;
  private int jj_kind = -1;
  private int[] jj_lasttokens = new int[100];
  private int jj_endpos;

  private void jj_add_error_token(int kind, int pos) {
    if (pos >= 100) return;
    if (pos == jj_endpos + 1) {
      jj_lasttokens[jj_endpos++] = kind;
    } else if (jj_endpos != 0) {
      jj_expentry = new int[jj_endpos];
      for (int i = 0; i < jj_endpos; i++) {
        jj_expentry[i] = jj_lasttokens[i];
      }
      boolean exists = false;
      for (java.util.Enumeration e = jj_expentries.elements(); e.hasMoreElements();) {
        int[] oldentry = (int[])(e.nextElement());
        if (oldentry.length == jj_expentry.length) {
          exists = true;
          for (int i = 0; i < jj_expentry.length; i++) {
            if (oldentry[i] != jj_expentry[i]) {
              exists = false;
              break;
            }
          }
          if (exists) break;
        }
      }
      if (!exists) jj_expentries.addElement(jj_expentry);
      if (pos != 0) jj_lasttokens[(jj_endpos = pos) - 1] = kind;
    }
  }

  public ParseException generateParseException() {
    jj_expentries.removeAllElements();
    boolean[] la1tokens = new boolean[31];
    for (int i = 0; i < 31; i++) {
      la1tokens[i] = false;
    }
    if (jj_kind >= 0) {
      la1tokens[jj_kind] = true;
      jj_kind = -1;
    }
    for (int i = 0; i < 5; i++) {
      if (jj_la1[i] == jj_gen) {
        for (int j = 0; j < 32; j++) {
          if ((jj_la1_0[i] & (1<<j)) != 0) {
            la1tokens[j] = true;
          }
        }
      }
    }
    for (int i = 0; i < 31; i++) {
      if (la1tokens[i]) {
        jj_expentry = new int[1];
        jj_expentry[0] = i;
        jj_expentries.addElement(jj_expentry);
      }
    }
    jj_endpos = 0;
    jj_rescan_token();
    jj_add_error_token(0, 0);
    int[][] exptokseq = new int[jj_expentries.size()][];
    for (int i = 0; i < jj_expentries.size(); i++) {
      exptokseq[i] = (int[])jj_expentries.elementAt(i);
    }
    return new ParseException(token, exptokseq, tokenImage);
  }

  final public void enable_tracing() {
  }

  final public void disable_tracing() {
  }

  final private void jj_rescan_token() {
    jj_rescan = true;
    for (int i = 0; i < 4; i++) {
      JJCalls p = jj_2_rtns[i];
      do {
        if (p.gen > jj_gen) {
          jj_la = p.arg; jj_lastpos = jj_scanpos = p.first;
          switch (i) {
            case 0: jj_3_1(); break;
            case 1: jj_3_2(); break;
            case 2: jj_3_3(); break;
            case 3: jj_3_4(); break;
          }
        }
        p = p.next;
      } while (p != null);
    }
    jj_rescan = false;
  }

  final private void jj_save(int index, int xla) {
    JJCalls p = jj_2_rtns[index];
    while (p.gen > jj_gen) {
      if (p.next == null) { p = p.next = new JJCalls(); break; }
      p = p.next;
    }
    p.gen = jj_gen + xla - jj_la; p.first = token; p.arg = xla;
  }

  static final class JJCalls {
    int gen;
    Token first;
    int arg;
    JJCalls next;
  }

}
