% Author: Günter Kniesel
% Date: 17.06.2005

 /**
  * Hier soll analog zu javaSyntax.pl alles herein, was die
  * LogicAJ-Syntax beschreibt.
  *
  * Folgendes ist nur ein vereinsamtes Fakt aus "languageIndependent.pl"
  * das ich schon mal rübergeholt habe. Alles andere ist noch zu definieren.
  * -- gk, 17.06.2005
  */
  
:- multifile attribSignature/2.

attribSignature(aspect,1).


/*
  aspect(#aspectClass)
*/
:-dynamic aspect/1.
:-multifile aspect/1.
