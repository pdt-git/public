% Author:
% Date: 28.09.2003

%------------------------------------

% For every class XXXimplement the Link interface as follows:
%
% class XXX implements org.cs3.tailor.Link {
%
%   Link Link$_link
%   Link Link$next() { return Link$_link; }
%   void Link$setNext(Link newLink) { Link$_link = newLink; }
%
% }
ct(ilink_low, (
    % The interface 'org.cs3.tailor.Link' ...
    packageT(_LPID, 'org.cs3.tailor'),
    classDefT(_LCID, _LPID, 'Link', _),
    interfaceT(_LCID),

    % ... has a setNext(Link) method signature
    methodDefT(_isetID, _LCID, 'Link$setNext', [_param], type(basic, void, 0), [], null),
    varDefT(_param, _setID,_setID, type(class, _LCID, 0), 'newLink', null),

    % ... and a next() method signature
    methodDefT(_inextID, _LCID, 'Link$next', [], type(basic, void, 0), [], null),

    % ... and the current class does not implement the Link interface
    classDefT(_CID, _, _, _),
    not(implementsT(_CID, _LCID)),

    new_id(_LFID),   % id of "Link Link$_link;" field to be added to current class
    new_id(_setID),  % id of "void Link$setNext(Link)" method implem. to be added...
    new_id(_nextID), % id of "Link Link$next()" method implem. to be added...
    new_id(LHS),     % id of identifier on LHS of assignment "Link$_link = newLink;"
    new_id(RHS),     % ...                 RHS ...  within setNext
    new_id(_ret),
    new_id(_nextBody),
    new_id(_getF),
    new_id(_setBody),
    new_id(_setF)
),(
    add(varDefT(_LFID, _CID, _CID, type(class, _LCID, 0), 'Link$_link', null)),

    add(methodDefT(_nextID, _CID, 'Link$next', [], type(class, _LCID, 0), [], _nextBody)),
      add(blockT(_nextBody,_nextID,_nextID,[_ret])),
        add(returnT(_ret, _nextBody, _nextID, _getF)),
          add(getFieldT(_getF, _ret, _nextID, null,'Link$_link', _LFID)),

    add(methodDefT(_setID, _CID, 'Link$setNext', [_param], type(basic, void, 0), [], _setBody)),
      add(varDefT(_param, _setID, _setID, type(class, _LCID, 0), null)),
      add(blockT(_setBody, _setID, _setID, [_setF])),
        add(assignT(_setF, _setBody, _setID, LHS, RHS)),
           add(identT(LHS,_setF,_setID,'Link$_link',_LFID)),
           add(identT(RHS,_setF,_setID,'newLink',_param)),

    add(implementsT(_CID,_LCID))
)).

% Backlink interface transformation: ct(ibacklink_low, Cond, Trans)
%
% For every class XXX that implements the 'org.cs3.tailor.Link' interface
% implement also the 'org.cs3.tailor.BackLink' interface as follows:
%
% class XXX implements org.cs3.tailor.Link org.cs3.tailor.BackLink {
%
%   Link BackLink$_link;
%   Link BackLink$prior() { return BackLink$_link; }
%   void BackLink$setPrior(Link newLink) { BackLink$_link = newLink; }
%
% }
ct(ibacklink_low, (
    % In the package 'org.cs3.tailor'
    packageT(_LPID, 'org.cs3.tailor'),

    % ... the interface 'org.cs3.tailor.Link' exists
    classDefT(_LCID2, _LPID, 'Link', _),
    interfaceT(_LCID2),

    % ... the interface 'org.cs3.tailor.BackLink' exists
    classDefT(_LCID, _LPID, 'BackLink', _),
    interfaceT(_LCID),

    % ... and the current class implements Link but not BackLink
    classDefT(_CID, _, _, _),
    implementsT(_CID, _LCID2),
    not(implementsT(_CID, _LCID)),

    new_id(_LFID),    % id of "Link BackLink$_link" field to be added
    new_id(_nextID),  % id of "Link BackLink$prior()" method implem. to be added...
    new_id(_nextBody),% id of method body
    new_id(_ret),     % id of return statement in body
    new_id(_getF),    % id of field access in return statement
    new_id(_setID),    % id of "void BackLink$setPrior(Link)" method implem. to be added...
    new_id(_param),   % id of method parameter
    new_id(_setBody), % id of method body
    new_id(_setF),    % id of assignment statement in body
    new_id(LHS),      % id of identifier on LHS of assignment
    new_id(RHS)       % ...                 RHS ...
),( 
    add(varDefT(_LFID, _CID, _CID, type(class, _LCID, 0), 'BackLink$_link', null)),

    add(methodDefT(_nextID, _CID, 'BackLink$prior', [], type(class, _LCID, 0), [], _nextBody)),
      add(blockT(_nextBody,_nextID,_nextID,[_ret])),
        add(returnT(_ret, _nextBody, _nextID, _getF)),
          add(identT(_getF, _ret, _nextID, 'BackLink$_link', _LFID)),

    add(methodDefT(_setID, _CID, 'BackLink$setPrior', [_param], type(basic, void, 0), [], _setBody)),
      add(varDefT(_param, _setID, _setID, type(class, _LCID, 0), null)),
      add(blockT(_setBody, _setID, _setID, [_setF])),
          add(assignT(_setF, _setBody, _setID, LHS, RHS)),    
          add(identT(LHS,_setF,_setID,'BackLink$_link',_LFID)), 
          add(identT(RHS,_setF,_setID,'newLink',_param)),

    add(implementsT(_CID,_LCID))
)).


            
% Backlink code transformation: ct(cbacklink_low, Cond, Trans)
%
% For every class XXX that implements the 'org.cs3.tailor.Link' interface
% insert after the assignment in its Link$setLink method an if-Statement:
%
% class XXX implements org.cs3.tailor.Link ... {
%
%   Link$setLink(BackLink newLink) {
%     // ...
%     Link$_link = newLink;  // <-- this assignment must be followed by this -->
%     if(Link$_link != newLink) then newLink.BackLink$setPrior(Link$_link);
%     // ...
%   }
%
% }
ct(cbacklink_low, (
    % If there the BackLink interface exists
    packageT(_LPID, 'org.cs3.tailor'),
    classDefT(_LCID, _LPID, 'BackLink', _),
    interfaceT(_LCID),

    % ... and has a setPrior(BackLink???) method signature
    methodDefT(_setID, _LCID, 'BackLink$setPrior', [_param], type(basic, void, 0), [], null),
    varDefT(_param, _setID,_setID, type(class, _LCID, 0), 'newLink', null),

    % ... and there exists a class, that implements the BackLink interface
    classDefT(_CID, _, _, _),
    implementsT(_CID, _LCID),

    % ... and this class has the generated link field
    varDefT(_LFID, _CID, _CID, type(class, _LCID, 0), 'Link$_link', null),

    % ... and this class has the generated implementation of the setNext method
    methodDefT(_setIDimpl, _CID, 'Link$setNext', _, _, _, _body),

    % ... that contains an assignment to the link field
    assignT(_setF, _setp, _setIDimpl, _iLFID, _iparam),
       identT(_iLFID,_setF,_setIDimpl,'Link$_link',_LFID),

    new_id(_if),
    new_id(_noteq),
    new_id(LHS),
    new_id(RHS),
    new_id(_call)
%    new_id(MI)
),(
    % ... then call the setPrior method of the parameter during invocations
    % of setNext (if necessary for setting the back link)
    add(ifT(_if, _setp, _setIDimpl, _noteq, _call, 'null')),
        add(operationT(_noteq, _if, _setIDimpl, [LHS,RHS], '!=',0)),
           add(ident(LHS, _noteq, _setIDimpl, _LFID)),
           add(ident(RHS,_noteq, _setIDimpl,_param)),
        add(applyT(_call,_setp,_setIDimpl,null, 'BackLink$setPrior' ,[_iparam],_setID)),
%            add(identT(MI,_call,_setIDimpl,'BackLink$setPrior',_setID)),

    % ... after the assignment
    add(after(_setF, _if))
)).

