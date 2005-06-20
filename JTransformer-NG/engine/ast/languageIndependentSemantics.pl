% Author: Günter Kniesel
% Date: 17.06.2005

/**
 * Hier soll langfristig alles abstrahiert werden, was mit der statischen
 * Semantik einer Sprache zu tun hat (Typen, Scopes), ähnlich wie das mit
 * der Syntax geschehen ist in der Datei "languageIndependentSyntax.pl"
 */
 
/* So war es bisher: ------------------------------------------------------ */

basicType(char).
basicType(int).
basicType(float).
basicType(double).
basicType(void).
basicType(long).
basicType(short).
basicType(byte).
basicType(boolean).

/* So soll es in Zukunft sein: -------------------------------------------- */
   
/*
 * TODO für Sebastian: fq_api.pl so anpassen, dass diese Definition genuzt wird.
 * Danach den performancefeindlichen 'Java'-Parameter an den Aufrufstellen
 * durch PE eliminieren: basicType('Java',Type) ---> basicType_Java(Type)
 * -- GK, 17.6.2005
 */

basicType('Java', char).
basicType('Java', int).
basicType('Java', float).
basicType('Java', double).
basicType('Java', void).
basicType('Java', long).
basicType('Java', short).
basicType('Java', byte).
basicType('Java', boolean).
