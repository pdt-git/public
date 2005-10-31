:- [
    %javaFactbase, --> st.java/astSpec    % New Java syntax description
    %javaSyntax,   --> st.java/astSpec    % Old Java syntax description 
    fq_api,                               % FQN patch for Java AST
    fq_api_tests
   ].
   
:- ['javaAstOperations/main.pl']. % Operations on Java AST
:- ['src_file_handling'].         % Create Java Classes in file system
:- ['src_file_handling_test'].    
