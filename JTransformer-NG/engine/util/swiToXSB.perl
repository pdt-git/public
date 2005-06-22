#!perl -w 

$file = $ARGV[0]; 
$outfile = $ARGV[1]; 

open( CURRENTFILE, "$file"); 
open( OUTFILE, ">$outfile" ); 

while ( <CURRENTFILE> ) { 

# \b : word boundary, (term) sorgt dafür das term an $1..$9 gebunden wird, so kann term im replace part wiederverwendet werden

	$_ =~ s/\b_([a-zA-Z0-9])/VAR$1/g; 
	
	print OUTFILE $_; 


} 

close( CURRENTFILE ); 
close( OUTFILE ); 