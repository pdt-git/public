:- [
  % language independent:
    languageIndependentSyntax, % must be loaded first because it contains all 'multifile' definitions.
    languageIndependentSemantics,
    languageAbstractions,
   
  % definitions of language specific ASTs: 
    'java/main.pl'                   % Java (hard-coded)
   ].