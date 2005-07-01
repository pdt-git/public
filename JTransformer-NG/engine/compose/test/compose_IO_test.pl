:- module(ct_compose_input_syntax_test, [ct_io_test/1] ).

legal_CT_sequence_syntax( input(1), 
andSequence(0,
            addClassCtSequence(_className_i,_className_v,_packageName_i,_packageName_v),
%                                                                                      ^
            [
              'ct'(1, 'addClass'(_className_i,_className_v,_packageName_i,_packageName_v),
%             ^  ^    ^        ^  
              and( noId, [
%             ^^^^^^^^^^^^
                 and(2,[
                       not(3,[
                             exists(4,class(_className_i,_packageName_i,_className_v))
                       ]),
                       exists(5,packageT(_packageName_i,_packageName_v))
                ]),
                new_id(_className_i)
              ]),
%             ^^
              transformations(6,
                [
                add( 666, modifierT(_className_i,'public')),
%                    ^^^^
                add(7,class(_className_i,_packageName_i,_className_v))
                ]
               ) % end of transformation
               ) % end of ct
               ])%.end of list with or-sequence body.
).


legal_CT_sequence_syntax( output(1), 
'<?xml version="1.0" ?><CTSDocument><CTSequence type="ConditionalTransformationOrSequence" treeId="0" ><CT treeId="1" ><Condition type="AndCondition" treeId="2" ><Condition type="NotCondition" treeId="3" ><Condition type="existsClass" treeId="4" ><Parameter>className</Parameter><Parameter>packageName</Parameter></Condition></Condition><Condition type="existsPackage" treeId="5" ><Parameter>packageName</Parameter></Condition></Condition><Transformation type="addClass" treeId="7" ><Parameter>className</Parameter><Parameter>packageName</Parameter></Transformation></CT></CTSequence><ParameterList><Parameter external="true"><name>className</name><value>null</value><type>org.joint.contract.model.parameterDomain.ClassDomain</type></Parameter><Parameter external="true"><name>packageName</name><value>null</value><type>org.joint.contract.model.parameterDomain.PackageDomain</type></Parameter></ParameterList></CTSDocument>'
). 

ct_io_test( addClassCtSequence ) :- 
    legal_CT_sequence_syntax( input(1), Seq ),
    legal_CT_sequence_syntax( output(1), ExpectedXML ),
    export_to_XML_document( Seq, XML ),
    assert_true('Wrong XML representation of CT AND sequence', 
                 ( nonvar(XML), nonvar(ExpectedXML), XML = ExpectedXML) 
    ).
    
    