:- module(call_site_annotator,[]).




file_annotation_hook(_,_,Terms,InAnos,[references_files(Refs)|InAnos]):-
    member.
