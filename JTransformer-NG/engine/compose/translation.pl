/*
Anbei eine Gegenüberstellung dessen was in der properties-Datei
von Contract steht (rechte Spalte) und was es in JTransformer als
Bedingungen, PEFs und Transformationen gibt (rechte Spalte).

- Was ist mit den Doppeleintragungen (equals, subtype)? 
  --> evtl. löschen
- Kommt es eigentlich im XML file auf Gross- und Kleinschreibung an?
- Warum werden manche PEFs nicht in Contract abgebildet (toplevelT, importT)?
  --> evtl. ergänzen 
- Bitte überprüfen, was in den Fällen genutzt wird, wo es sowohl im High-level
  als auch im low-level API eine Entsprechung gibt.  
  
*/
/*
In Contract genutzes Low-Level API (PEFs):
-----------------------------------------------------------------------------
*/                                       
%export_conditionName_(toplevelT,                           ). % <--- fehlt in ConTraCT !!!
%export_conditionName_(classDefT,       Class               ). % <--- nicht genutzt (--> high)
%export_conditionName_(extendsT,        Extends             ). % <--- nicht genutzt (--> high)
export_conditionName_(externT,         External            ). % <--- Tippfehler
%export_conditionName_(fieldDefT,       Field               ). % <--- nicht genutzt (--> high)
%export_conditionName_(implementsT,     Implements          ). % <--- nicht genutzt (--> high)
%export_conditionName_(importT,                             ). % <--- fehlt in ConTraCT !!!
%export_conditionName_(interfaceT,      Interface           ). % <--- nicht genutzt (--> high)
%export_conditionName_(methodDefT,      Method              ). % <--- nicht genutzt (--> high)
export_conditionName_(modifierT,       Modifier            ).
export_conditionName_(packageT,        Package             ). % <--- Low Level   /2
export_conditionName_(paramDefT,       Parameter           ). % <--- oder ist das der param des
                                                              %      High-Level-APIs?
                                                                                                                         
export_conditionName_( applyT,         Apply               ).            
export_conditionName_( assertT,        Assert              ).            
export_conditionName_( assignT,        Assign              ).            
export_conditionName_( assignopT,      AssignOp            ).            
export_conditionName_( blockT,         Block               ).            
export_conditionName_( breakT,         Break               ).            
export_conditionName_( caseT,          Case                ).            
export_conditionName_( catchT,         Catch               ).            
export_conditionName_( conditionalT,   Conditional         ).            
export_conditionName_( continueT,      Continue            ).            
export_conditionName_( doLoopT,        DoLoop              ).                         
%export_conditionName_( execT,          Execution           ).  % <--- nicht genutzt (--> high)    
export_conditionName_( forLoopT,       FopLoop             ).            
%export_conditionName_( getFieldT,      GetField            ).  % <--- nicht genutzt (--> high)            
export_conditionName_( identT,         Ident               ).            
export_conditionName_( ifT,            If                  ).            
export_conditionName_( indexedT,       Indexed             ).            
export_conditionName_( labelT,         Label               ).            
export_conditionName_( literalT,       Literal             ).            
export_conditionName_( localDefT,      LocalDef            ).            
export_conditionName_( newArrayT,      NewArray            ).            
export_conditionName_( newClassT,      NewClass            ).            
export_conditionName_( nopT,           NoOperation         ).  % <--- Tippfehler                
export_conditionName_( operationT,     Operation           ).            
export_conditionName_( precedenceT,    Precedence          ).            
export_conditionName_( returnT,        Return              ).            
export_conditionName_( selectT,        Select              ).            
export_conditionName_( switchT,        Switch              ).                         
export_conditionName_( synchronizedT,  SynchronizedizedPE  ).  % <--- Tippfehler    
export_conditionName_( throwT,         Throw               ).            
export_conditionName_( tryT,           Try                 ).            
export_conditionName_( typeCastT,      TypeCast            ).            
export_conditionName_( typeTestT,      TypeTest            ).            
export_conditionName_( whileLoopT,     WhileLoop           ).            
                                                           
export_transformationName_( add,       add                 ).                                                      
export_transformationName_( delete,    delete              ).  
export_transformationName_( replace,   replace             ).   
                                                      
/*
In ConTraCT genutztes High-Level API (siehe JTransformer/api/high_level_api.pl)
===============================================================================
*/
export_conditionName_( class,          Class               ).  % <--- High Level API
export_conditionName_( constructor,    Constructor         ).  % <--- High Level API
export_conditionName_( extends,        Extends             ).  % <--- High Level API
export_conditionName_( field,          Field               ).  % <--- High Level API
export_conditionName_( implements,     Implements          ).  % <--- High Level API
export_conditionName_( interface,      Interface           ).  % <--- High Level API
export_conditionName_( packageT,       Package             ).  % <--- High Level API 
export_conditionName_( getField,       GetField            ).  % <--- High Level API                         
export_conditionName_( execution,      Execution           ).  % <--- High Level API  
export_conditionName_( method,         Method              ).  % <--- High Level API

/*
In COnTraCT bisher ungenutztes High-Level API (siehe JTransformer/api/high_level_api.pl):
=========================================================================================
export_conditionName_( inner,                              ).  % <--- High Level API
export_conditionName_( local,                              ).  % <--- High Level API
export_conditionName_( anonymous,                          ).  % <--- High Level API
                                                           
export_conditionName_( fullQualifiedName,                  ).  % <--- High Level API
                                                           
export_conditionName_( param,                              ).  % <--- High Level API
export_conditionName_( localVar,                           ).  % <--- High Level API
                                                           
export_conditionName_( setField,                           ).  % <--- High Level API
export_conditionName_( methodCall,                         ).  % <--- High Level API
export_conditionName_( newCall,                            ).  % <--- High Level API
*/                                   

/*
Sonstiges. Teilweise Dinge für die mir die Entsprechung nicht klar ist
=========================================================================================
                        JTransformer   ConTRaCT
                        ---------------------------
                                      #Containings
                        ,              AndCondition
                        not            NotCondition
                        ;              OrCondition

                                     # Atomics
                        true           True
                        false          False
                                       exists
                                       subtype
                        =              equals
                                       Equals                 % <--- Doppelt eingetragen
                                       Subtype                % <--- Doppelt eingetragen                  
                                       IsInProject            % <--- Was ist die Entsprechung?
                                
                                     # ProgramElements
                                       Access                 % <--- Was ist die Entsprechung?
*/
