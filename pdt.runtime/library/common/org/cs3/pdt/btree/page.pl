:- module(pdt_btree_page,[]).

/*

To store the entries of a page, we need a data structure that provides the following operations

the usual non-destructive map operations:

empty(-Out) 
put_entry(+In,+Key,+Value,-Out)
get_entry(+In,+Key,+Value)
remove_entry(+In,+Key,-Out)

plus the following operations that probably suggest a tree-based data structure:

split(+In,-Key,-Value,-Left,-Right)
concat(+Left,+Right,+Key,+Value,-Out)





a page entry is a record that contains 
 - a key
 - the value stored for this key
 - a reference to a page that contains references left of key but right of its predecessor in
   the current page.
   
 each page has an artificial maximum entry which is right of all other entries. Its only purpose is 
 to maintain a reference to the right most page.

*/

page_get_entry(Page,Key,Entry)