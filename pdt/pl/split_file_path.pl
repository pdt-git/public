:- module( split_file_path,
      [ split_file_path/5      % (+FullPath, ?Directory, ?FileName,?BaseName,?Extension)
      ]).
      
      
%% split_file_path(+FullPath,?Directory,?FileName,?BaseName,?Extension) is det
%
split_file_path(FullPath, Directory, FileName,BaseName,Extension):-
    file_directory_name(FullPath, Directory0),           % SWI-Prolog
    atom_concat(Directory0,'/',Directory),               % SWI-Prolog
    file_base_name(FullPath,FileName),                   % SWI-Prolog
    file_name_extension(BaseName,Extension,FileName).    % SWI-Prolog
            
