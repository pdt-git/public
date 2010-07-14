/* Generated By:JJTree&JavaCC: Do not edit this line. CanonicalTermParserTokenManager.java */
package org.cs3.pl.cterm.internal.parser;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

/** Token Manager. */
public class CanonicalTermParserTokenManager implements CanonicalTermParserConstants
{

  /** Debug output. */
  public  java.io.PrintStream debugStream = System.out;
  /** Set debug output. */
  public  void setDebugStream(java.io.PrintStream ds) { debugStream = ds; }
private final int jjStopStringLiteralDfa_0(int pos, long active0)
{
   switch (pos)
   {
      default :
         return -1;
   }
}
private final int jjStartNfa_0(int pos, long active0)
{
   return jjMoveNfa_0(jjStopStringLiteralDfa_0(pos, active0), pos + 1);
}
private int jjStopAtPos(int pos, int kind)
{
   jjmatchedKind = kind;
   jjmatchedPos = pos;
   return pos + 1;
}
private int jjMoveStringLiteralDfa0_0()
{
   switch(curChar)
   {
      case 33:
         return jjStopAtPos(0, 15);
      case 40:
         return jjStopAtPos(0, 7);
      case 41:
         return jjStopAtPos(0, 8);
      case 44:
         return jjStopAtPos(0, 9);
      case 46:
         return jjStartNfaWithStates_0(0, 10, 9);
      case 91:
         return jjStopAtPos(0, 11);
      case 93:
         return jjStopAtPos(0, 12);
      case 123:
         return jjStopAtPos(0, 13);
      case 125:
         return jjStopAtPos(0, 14);
      default :
         return jjMoveNfa_0(0, 0);
   }
}
private int jjStartNfaWithStates_0(int pos, int kind, int state)
{
   jjmatchedKind = kind;
   jjmatchedPos = pos;
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) { return pos + 1; }
   return jjMoveNfa_0(state, pos + 1);
}
static final long[] jjbitVec0 = {
   0xfffffffefffffffeL, 0xffffffffffffffffL, 0xffffffffffffffffL, 0x7fffffff00ffffffL
};
static final long[] jjbitVec2 = {
   0x0L, 0x0L, 0xfffffffe00000000L, 0xffffffffffffffffL
};
static final long[] jjbitVec3 = {
   0xffffffffffff0000L, 0xffffffffffffffffL, 0xffffffffffffffffL, 0xffffffffffffffffL
};
static final long[] jjbitVec4 = {
   0xffffffffffffffffL, 0xffffffffffffffffL, 0xffffffffffffffffL, 0xffffffffffffL
};
static final long[] jjbitVec5 = {
   0xfffe0000fffffffeL, 0xffffffffffffffffL, 0xffffffffffffffffL, 0x7fffffff00ffffffL
};
static final long[] jjbitVec6 = {
   0x0L, 0x0L, 0x0L, 0xffffffffffffffffL
};
static final long[] jjbitVec7 = {
   0x0L, 0xffffffffffffffffL, 0xffffffffffffffffL, 0xffffffffffffffffL
};
static final long[] jjbitVec8 = {
   0xfffffffffffffffeL, 0xffffffffffffffffL, 0xffffffffffffffffL, 0xffffffffffffffffL
};
static final long[] jjbitVec9 = {
   0x0L, 0x0L, 0xffffffffffffffffL, 0xffffffffffffffffL
};
private int jjMoveNfa_0(int startState, int curPos)
{
   int startsAt = 0;
   jjnewStateCnt = 64;
   int i = 1;
   jjstateSet[0] = startState;
   int kind = 0x7fffffff;
   for (;;)
   {
      if (++jjround == 0x7fffffff)
         ReInitRounds();
      if (curChar < 64)
      {
         long l = 1L << curChar;
         do
         {
            switch(jjstateSet[--i])
            {
               case 0:
                  if ((0xfc00ec5800000000L & l) != 0L)
                  {
                     if (kind > 17)
                        kind = 17;
                     jjCheckNAdd(9);
                  }
                  else if ((0x3ff000000000000L & l) != 0L)
                  {
                     if (kind > 25)
                        kind = 25;
                     jjCheckNAddStates(0, 9);
                  }
                  else if (curChar == 34)
                     jjCheckNAddStates(10, 13);
                  else if (curChar == 39)
                     jjCheckNAddStates(14, 17);
                  if ((0x3ff000000000000L & l) != 0L)
                  {
                     if (kind > 33)
                        kind = 33;
                     jjCheckNAdd(29);
                  }
                  else if (curChar == 47)
                     jjAddStates(18, 19);
                  else if (curChar == 60)
                     jjstateSet[jjnewStateCnt++] = 1;
                  if (curChar == 48)
                     jjstateSet[jjnewStateCnt++] = 12;
                  break;
               case 7:
                  if (curChar == 62 && kind > 16)
                     kind = 16;
                  break;
               case 8:
                  if ((0xfc00ec5800000000L & l) == 0L)
                     break;
                  if (kind > 17)
                     kind = 17;
                  jjCheckNAdd(9);
                  break;
               case 9:
                  if ((0xffffec5800000000L & l) == 0L)
                     break;
                  if (kind > 17)
                     kind = 17;
                  jjCheckNAdd(9);
                  break;
               case 11:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 18)
                     kind = 18;
                  jjstateSet[jjnewStateCnt++] = 11;
                  break;
               case 12:
                  if (curChar == 39)
                     jjstateSet[jjnewStateCnt++] = 13;
                  break;
               case 13:
                  if ((0xffffec5800000000L & l) != 0L && kind > 25)
                     kind = 25;
                  break;
               case 14:
                  if (curChar == 48)
                     jjstateSet[jjnewStateCnt++] = 12;
                  break;
               case 15:
               case 19:
                  if (curChar == 39)
                     jjCheckNAddStates(14, 17);
                  break;
               case 16:
                  if ((0xffffff7fffffdbffL & l) != 0L)
                     jjCheckNAddStates(14, 17);
                  break;
               case 18:
                  if ((0x8400000000L & l) != 0L)
                     jjCheckNAddStates(14, 17);
                  break;
               case 20:
                  if (curChar == 39)
                     jjstateSet[jjnewStateCnt++] = 19;
                  break;
               case 21:
                  if (curChar == 39 && kind > 31)
                     kind = 31;
                  break;
               case 22:
                  if (curChar == 34)
                     jjCheckNAddStates(10, 13);
                  break;
               case 23:
                  if ((0xfffffffbffffdbffL & l) != 0L)
                     jjCheckNAddStates(10, 13);
                  break;
               case 25:
                  if ((0x8400000000L & l) != 0L)
                     jjCheckNAddStates(10, 13);
                  break;
               case 26:
                  if (curChar == 39)
                     jjCheckNAddStates(10, 13);
                  break;
               case 27:
                  if (curChar == 39)
                     jjstateSet[jjnewStateCnt++] = 26;
                  break;
               case 28:
                  if (curChar == 34 && kind > 32)
                     kind = 32;
                  break;
               case 29:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 33)
                     kind = 33;
                  jjCheckNAdd(29);
                  break;
               case 30:
                  if (curChar == 47)
                     jjAddStates(18, 19);
                  break;
               case 31:
                  if (curChar == 47)
                     jjCheckNAddStates(20, 22);
                  break;
               case 32:
                  if ((0xffffffffffffdbffL & l) != 0L)
                     jjCheckNAddStates(20, 22);
                  break;
               case 33:
                  if ((0x2400L & l) != 0L && kind > 5)
                     kind = 5;
                  break;
               case 34:
                  if (curChar == 10 && kind > 5)
                     kind = 5;
                  break;
               case 35:
                  if (curChar == 13)
                     jjstateSet[jjnewStateCnt++] = 34;
                  break;
               case 36:
                  if (curChar == 42)
                     jjCheckNAddTwoStates(37, 38);
                  break;
               case 37:
                  if ((0xfffffbffffffffffL & l) != 0L)
                     jjCheckNAddTwoStates(37, 38);
                  break;
               case 38:
                  if (curChar == 42)
                     jjAddStates(23, 24);
                  break;
               case 39:
                  if ((0xffff7fffffffffffL & l) != 0L)
                     jjCheckNAddTwoStates(40, 38);
                  break;
               case 40:
                  if ((0xfffffbffffffffffL & l) != 0L)
                     jjCheckNAddTwoStates(40, 38);
                  break;
               case 41:
                  if (curChar == 47 && kind > 6)
                     kind = 6;
                  break;
               case 42:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 25)
                     kind = 25;
                  jjCheckNAddStates(0, 9);
                  break;
               case 43:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 25)
                     kind = 25;
                  jjCheckNAdd(43);
                  break;
               case 45:
                  if ((0x3000000000000L & l) == 0L)
                     break;
                  if (kind > 26)
                     kind = 26;
                  jjstateSet[jjnewStateCnt++] = 45;
                  break;
               case 47:
                  if ((0xff000000000000L & l) == 0L)
                     break;
                  if (kind > 27)
                     kind = 27;
                  jjstateSet[jjnewStateCnt++] = 47;
                  break;
               case 49:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 28)
                     kind = 28;
                  jjstateSet[jjnewStateCnt++] = 49;
                  break;
               case 50:
                  if ((0x3ff000000000000L & l) != 0L)
                     jjCheckNAddTwoStates(50, 51);
                  break;
               case 51:
                  if (curChar == 46)
                     jjCheckNAdd(52);
                  break;
               case 52:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 29)
                     kind = 29;
                  jjCheckNAddTwoStates(52, 53);
                  break;
               case 54:
                  if ((0x280000000000L & l) != 0L)
                     jjCheckNAdd(55);
                  break;
               case 55:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 29)
                     kind = 29;
                  jjCheckNAdd(55);
                  break;
               case 56:
                  if ((0x3ff000000000000L & l) != 0L)
                     jjCheckNAddTwoStates(56, 57);
                  break;
               case 58:
                  if ((0x280000000000L & l) != 0L)
                     jjCheckNAdd(59);
                  break;
               case 59:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 29)
                     kind = 29;
                  jjCheckNAdd(59);
                  break;
               case 60:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 29)
                     kind = 29;
                  jjCheckNAddTwoStates(60, 61);
                  break;
               case 62:
                  if ((0x280000000000L & l) != 0L)
                     jjCheckNAdd(63);
                  break;
               case 63:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 29)
                     kind = 29;
                  jjCheckNAdd(63);
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      else if (curChar < 128)
      {
         long l = 1L << (curChar & 077);
         do
         {
            switch(jjstateSet[--i])
            {
               case 0:
                  if ((0x47fffffe50000001L & l) != 0L)
                  {
                     if (kind > 17)
                        kind = 17;
                     jjCheckNAdd(9);
                  }
                  else if ((0x87fffffeL & l) != 0L)
                  {
                     if (kind > 33)
                        kind = 33;
                     jjCheckNAdd(29);
                  }
                  if ((0x87fffffeL & l) != 0L)
                  {
                     if (kind > 18)
                        kind = 18;
                     jjCheckNAdd(11);
                  }
                  break;
               case 1:
                  if (curChar == 99)
                     jjstateSet[jjnewStateCnt++] = 2;
                  break;
               case 2:
                  if (curChar == 108)
                     jjstateSet[jjnewStateCnt++] = 3;
                  break;
               case 3:
                  if (curChar == 97)
                     jjstateSet[jjnewStateCnt++] = 4;
                  break;
               case 4:
                  if (curChar == 117)
                     jjstateSet[jjnewStateCnt++] = 5;
                  break;
               case 5:
                  if (curChar == 115)
                     jjstateSet[jjnewStateCnt++] = 6;
                  break;
               case 6:
                  if (curChar == 101)
                     jjstateSet[jjnewStateCnt++] = 7;
                  break;
               case 8:
                  if ((0x47fffffe50000001L & l) == 0L)
                     break;
                  if (kind > 17)
                     kind = 17;
                  jjCheckNAdd(9);
                  break;
               case 9:
                  if ((0x47fffffed7ffffffL & l) == 0L)
                     break;
                  if (kind > 17)
                     kind = 17;
                  jjCheckNAdd(9);
                  break;
               case 10:
                  if ((0x87fffffeL & l) == 0L)
                     break;
                  if (kind > 18)
                     kind = 18;
                  jjCheckNAdd(11);
                  break;
               case 11:
                  if ((0x7fffffe87fffffeL & l) == 0L)
                     break;
                  if (kind > 18)
                     kind = 18;
                  jjCheckNAdd(11);
                  break;
               case 13:
                  if ((0x47fffffed7ffffffL & l) != 0L && kind > 25)
                     kind = 25;
                  break;
               case 16:
                  if ((0xffffffffefffffffL & l) != 0L)
                     jjCheckNAddStates(14, 17);
                  break;
               case 17:
                  if (curChar == 92)
                     jjstateSet[jjnewStateCnt++] = 18;
                  break;
               case 18:
                  if ((0x14404410000000L & l) != 0L)
                     jjCheckNAddStates(14, 17);
                  break;
               case 23:
                  if ((0xffffffffefffffffL & l) != 0L)
                     jjCheckNAddStates(10, 13);
                  break;
               case 24:
                  if (curChar == 92)
                     jjstateSet[jjnewStateCnt++] = 25;
                  break;
               case 25:
                  if ((0x14404410000000L & l) != 0L)
                     jjCheckNAddStates(10, 13);
                  break;
               case 29:
                  if ((0x87fffffeL & l) == 0L)
                     break;
                  if (kind > 33)
                     kind = 33;
                  jjCheckNAdd(29);
                  break;
               case 32:
                  jjAddStates(20, 22);
                  break;
               case 37:
                  jjCheckNAddTwoStates(37, 38);
                  break;
               case 39:
               case 40:
                  jjCheckNAddTwoStates(40, 38);
                  break;
               case 44:
                  if (curChar == 98)
                     jjstateSet[jjnewStateCnt++] = 45;
                  break;
               case 46:
                  if (curChar == 111)
                     jjstateSet[jjnewStateCnt++] = 47;
                  break;
               case 48:
                  if (curChar == 120)
                     jjCheckNAdd(49);
                  break;
               case 49:
                  if ((0x7e0000007eL & l) == 0L)
                     break;
                  if (kind > 28)
                     kind = 28;
                  jjCheckNAdd(49);
                  break;
               case 53:
                  if ((0x2000000020L & l) != 0L)
                     jjAddStates(25, 26);
                  break;
               case 57:
                  if ((0x2000000020L & l) != 0L)
                     jjAddStates(27, 28);
                  break;
               case 61:
                  if ((0x2000000020L & l) != 0L)
                     jjAddStates(29, 30);
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      else
      {
         int hiByte = (int)(curChar >> 8);
         int i1 = hiByte >> 6;
         long l1 = 1L << (hiByte & 077);
         int i2 = (curChar & 0xff) >> 6;
         long l2 = 1L << (curChar & 077);
         do
         {
            switch(jjstateSet[--i])
            {
               case 0:
               case 9:
                  if (!jjCanMove_0(hiByte, i1, i2, l1, l2))
                     break;
                  if (kind > 17)
                     kind = 17;
                  jjCheckNAdd(9);
                  break;
               case 11:
                  if (!jjCanMove_1(hiByte, i1, i2, l1, l2))
                     break;
                  if (kind > 18)
                     kind = 18;
                  jjstateSet[jjnewStateCnt++] = 11;
                  break;
               case 13:
                  if (jjCanMove_0(hiByte, i1, i2, l1, l2) && kind > 25)
                     kind = 25;
                  break;
               case 16:
                  if (jjCanMove_2(hiByte, i1, i2, l1, l2))
                     jjAddStates(14, 17);
                  break;
               case 23:
                  if (jjCanMove_2(hiByte, i1, i2, l1, l2))
                     jjAddStates(10, 13);
                  break;
               case 32:
                  if (jjCanMove_3(hiByte, i1, i2, l1, l2))
                     jjAddStates(20, 22);
                  break;
               case 37:
                  if (jjCanMove_3(hiByte, i1, i2, l1, l2))
                     jjCheckNAddTwoStates(37, 38);
                  break;
               case 39:
               case 40:
                  if (jjCanMove_3(hiByte, i1, i2, l1, l2))
                     jjCheckNAddTwoStates(40, 38);
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      if (kind != 0x7fffffff)
      {
         jjmatchedKind = kind;
         jjmatchedPos = curPos;
         kind = 0x7fffffff;
      }
      ++curPos;
      if ((i = jjnewStateCnt) == (startsAt = 64 - (jjnewStateCnt = startsAt)))
         return curPos;
      try { curChar = input_stream.readChar(); }
      catch(java.io.IOException e) { return curPos; }
   }
}
static final int[] jjnextStates = {
   43, 44, 46, 48, 50, 51, 56, 57, 60, 61, 23, 24, 27, 28, 16, 17, 
   20, 21, 31, 36, 32, 33, 35, 39, 41, 54, 55, 58, 59, 62, 63, 
};
private static final boolean jjCanMove_0(int hiByte, int i1, int i2, long l1, long l2)
{
   switch(hiByte)
   {
      case 0:
         return ((jjbitVec2[i2] & l2) != 0L);
      case 32:
         return ((jjbitVec3[i2] & l2) != 0L);
      case 255:
         return ((jjbitVec4[i2] & l2) != 0L);
      default :
         if ((jjbitVec0[i1] & l1) != 0L)
            return true;
         return false;
   }
}
private static final boolean jjCanMove_1(int hiByte, int i1, int i2, long l1, long l2)
{
   switch(hiByte)
   {
      case 0:
         return ((jjbitVec6[i2] & l2) != 0L);
      case 48:
         return ((jjbitVec7[i2] & l2) != 0L);
      case 255:
         return ((jjbitVec4[i2] & l2) != 0L);
      default :
         if ((jjbitVec5[i1] & l1) != 0L)
            return true;
         return false;
   }
}
private static final boolean jjCanMove_2(int hiByte, int i1, int i2, long l1, long l2)
{
   switch(hiByte)
   {
      case 0:
         return ((jjbitVec9[i2] & l2) != 0L);
      default :
         if ((jjbitVec8[i1] & l1) != 0L)
            return true;
         return false;
   }
}
private static final boolean jjCanMove_3(int hiByte, int i1, int i2, long l1, long l2)
{
   switch(hiByte)
   {
      case 0:
         return ((jjbitVec9[i2] & l2) != 0L);
      default :
         return false;
   }
}

/** Token literal values. */
public static final String[] jjstrLiteralImages = {
"", null, null, null, null, null, null, "\50", "\51", "\54", "\56", "\133", 
"\135", "\173", "\175", "\41", null, null, null, null, null, null, null, null, null, 
null, null, null, null, null, null, null, null, null, };

/** Lexer state names. */
public static final String[] lexStateNames = {
   "DEFAULT",
};
static final long[] jjtoToken = {
   0x3be07ff81L, 
};
static final long[] jjtoSkip = {
   0x7eL, 
};
protected SimpleCharStream input_stream;
private final int[] jjrounds = new int[64];
private final int[] jjstateSet = new int[128];
protected char curChar;
/** Constructor. */
public CanonicalTermParserTokenManager(SimpleCharStream stream){
   if (SimpleCharStream.staticFlag)
      throw new Error("ERROR: Cannot use a static CharStream class with a non-static lexical analyzer.");
   input_stream = stream;
}

/** Constructor. */
public CanonicalTermParserTokenManager(SimpleCharStream stream, int lexState){
   this(stream);
   SwitchTo(lexState);
}

/** Reinitialise parser. */
public void ReInit(SimpleCharStream stream)
{
   jjmatchedPos = jjnewStateCnt = 0;
   curLexState = defaultLexState;
   input_stream = stream;
   ReInitRounds();
}
private void ReInitRounds()
{
   int i;
   jjround = 0x80000001;
   for (i = 64; i-- > 0;)
      jjrounds[i] = 0x80000000;
}

/** Reinitialise parser. */
public void ReInit(SimpleCharStream stream, int lexState)
{
   ReInit(stream);
   SwitchTo(lexState);
}

/** Switch to specified lex state. */
public void SwitchTo(int lexState)
{
   if (lexState >= 1 || lexState < 0)
      throw new TokenMgrError("Error: Ignoring invalid lexical state : " + lexState + ". State unchanged.", TokenMgrError.INVALID_LEXICAL_STATE);
   else
      curLexState = lexState;
}

protected Token jjFillToken()
{
   final Token t;
   final String curTokenImage;
   final int beginLine;
   final int endLine;
   final int beginColumn;
   final int endColumn;
   String im = jjstrLiteralImages[jjmatchedKind];
   curTokenImage = (im == null) ? input_stream.GetImage() : im;
   beginLine = input_stream.getBeginLine();
   beginColumn = input_stream.getBeginColumn();
   endLine = input_stream.getEndLine();
   endColumn = input_stream.getEndColumn();
   t = Token.newToken(jjmatchedKind, curTokenImage);

   t.beginLine = beginLine;
   t.endLine = endLine;
   t.beginColumn = beginColumn;
   t.endColumn = endColumn;

   return t;
}

int curLexState = 0;
int defaultLexState = 0;
int jjnewStateCnt;
int jjround;
int jjmatchedPos;
int jjmatchedKind;

/** Get the next Token. */
public Token getNextToken() 
{
  Token matchedToken;
  int curPos = 0;

  EOFLoop :
  for (;;)
  {
   try
   {
      curChar = input_stream.BeginToken();
   }
   catch(java.io.IOException e)
   {
      jjmatchedKind = 0;
      matchedToken = jjFillToken();
      return matchedToken;
   }

   try { input_stream.backup(0);
      while (curChar <= 32 && (0x100002600L & (1L << curChar)) != 0L)
         curChar = input_stream.BeginToken();
   }
   catch (java.io.IOException e1) { continue EOFLoop; }
   jjmatchedKind = 0x7fffffff;
   jjmatchedPos = 0;
   curPos = jjMoveStringLiteralDfa0_0();
   if (jjmatchedKind != 0x7fffffff)
   {
      if (jjmatchedPos + 1 < curPos)
         input_stream.backup(curPos - jjmatchedPos - 1);
      if ((jjtoToken[jjmatchedKind >> 6] & (1L << (jjmatchedKind & 077))) != 0L)
      {
         matchedToken = jjFillToken();
         return matchedToken;
      }
      else
      {
         continue EOFLoop;
      }
   }
   int error_line = input_stream.getEndLine();
   int error_column = input_stream.getEndColumn();
   String error_after = null;
   boolean EOFSeen = false;
   try { input_stream.readChar(); input_stream.backup(1); }
   catch (java.io.IOException e1) {
      EOFSeen = true;
      error_after = curPos <= 1 ? "" : input_stream.GetImage();
      if (curChar == '\n' || curChar == '\r') {
         error_line++;
         error_column = 0;
      }
      else
         error_column++;
   }
   if (!EOFSeen) {
      input_stream.backup(1);
      error_after = curPos <= 1 ? "" : input_stream.GetImage();
   }
   throw new TokenMgrError(EOFSeen, curLexState, error_line, error_column, error_after, curChar, TokenMgrError.LEXICAL_ERROR);
  }
}

private void jjCheckNAdd(int state)
{
   if (jjrounds[state] != jjround)
   {
      jjstateSet[jjnewStateCnt++] = state;
      jjrounds[state] = jjround;
   }
}
private void jjAddStates(int start, int end)
{
   do {
      jjstateSet[jjnewStateCnt++] = jjnextStates[start];
   } while (start++ != end);
}
private void jjCheckNAddTwoStates(int state1, int state2)
{
   jjCheckNAdd(state1);
   jjCheckNAdd(state2);
}

private void jjCheckNAddStates(int start, int end)
{
   do {
      jjCheckNAdd(jjnextStates[start]);
   } while (start++ != end);
}

}
