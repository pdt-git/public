#!perl -w 

$file = $ARGV[0]; 
$outfile = $ARGV[1]; 

open( CURRENTFILE, "$file"); 
open( OUTFILE, ">$outfile" ); 
$text = "";
while ( <CURRENTFILE> ) { 
	$text .= $_;
}
$_ = $text;

$_ =~ s/\/\*([^\/]|[\/][^\*])*\*\/|\/\/(^\n)*//g;
#$_ =~ s/\n\n//g;
print OUTFILE $_; 

close( CURRENTFILE ); 
close( OUTFILE ); 