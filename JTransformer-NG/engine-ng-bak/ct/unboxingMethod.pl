ct(unboxingMethod(_targetClass,_primitiveType),
(
      packageT(_java_lang, 'java.lang'),
%      packageT(_targetPackage, _targetPackage),
%      classDefT(_targetClass,_targetPackage,_targetClass,_),
      classDefT(_object,_java_lang,'Object',_),
      classDefT(_number,_java_lang,'Number',_),
      boxing_class(_primitiveType, _boxingClass),
      (
        (
          subtype(_boxingClass,_number),
          !,
          equals(_castTo,_number)
        );
        equals(_castTo,_boxingClass)
      ),
      classDefT(_targetClass8,_java_lang,'ClassCastException',_),
      classDefT(_targetClass9,_java_lang,'String',_),
      methodDefT(_method_27,_targetClass8,'<init>',_method_27_params,type('basic',void,0),_method_27_exs,_method_27_body),
      matchParams(_method_27_params, [type(class, _targetClass9,0)]),
      classDefT(_type_37,_java_lang,'Class',_),
      methodDefT(_method_36,_type_37,'getName',[],type('class',_targetClass9,0),_method_36_exs,_method_36_body),
      methodDefT(_method_40,_object,'getClass',[],type('class',_type_37,0),_method_40_exs,_method_40_body),

      atom_concat(_primitiveType, 'Value', _primitiveTypeValue),
      atom_concat(' can not be converted to ', _primitiveType, _msg),
      methodDefT(_method_unboxing,_castTo,_primitiveTypeValue,[],type('basic',_primitiveType,0),_,_),
      not(methodDefT(_, _targetClass,_primitiveTypeValue,[_],type(basic, _primitiveType, 0),[],_)),
      new_ids([_method,_node_0, ObjParam, _node_6,_node_7,_node_8,_node_9,_node_10,_node_11,_node_12,_node_13,_node_14,_node_15,_node_16,_node_17,_node_19,_node_20,_node_21,_node_22,_node_24,_node_25,_node_26,_node_30,_node_31,_node_32,_node_33,_node_34,_node_35,_node_38,_node_39])
),(
      add(methodDefT(_method, _targetClass,_primitiveTypeValue,[ObjParam],type(basic, _primitiveType, 0),[],_node_6)),
      add(modifierT(_method, 'public')),
      add(modifierT(_method, 'static')),
      add(paramDefT(ObjParam, _method, type(class, _object, 0), 'o')),
      add(blockT(_node_6, _method, _method, [_node_0])),
      add(ifT(_node_0, _node_6, _method, _node_8, _node_9, _node_7)),
      add(operationT(_node_8, _node_0, _method, [_node_10, _node_11], '==', 0)),
      add(identT(_node_10, _node_8, _method, 'o', ObjParam)),
      add(identT(_node_11, _node_8, _method, 'null', 'null')),
      add(returnT(_node_9, _node_0, _method, _node_13)),
      add(typeCastT(_node_13, _node_9, _method, type(basic, _primitiveType, 0), _node_14)),
      add(literalT(_node_14, _node_13, _method, type(basic, 'int', 0), '0')),
      add(ifT(_node_7, _node_0, _method, _node_16, _node_17, _node_15)),
      add(typeTestT(_node_16, _node_7, _method, type(class, _castTo, 0), _node_19)),
      add(identT(_node_19, _node_16, _method, 'o', ObjParam)),
      add(returnT(_node_17, _node_7, _method, _node_20)),
      add(applyT(_node_20, _node_17, _method, _node_22, _primitiveTypeValue, [], _method_23)),
      add(typeCastT(_node_22, _node_20, _method, type(class, _castTo, 0), _node_24)),
      add(identT(_node_24, _node_22, _method, 'o', ObjParam)),
      add(blockT(_node_15, _node_7, _method, [_node_25])),
      add(throwT(_node_25, _node_15, _method, _node_26)),
      add(newClassT(_node_26, _node_25, _method, _method_27, [_node_30], _node_31, 'null', null)),
      add(identT(_node_31, _node_26, _method, 'ClassCastException', _targetClass8)),
      add(operationT(_node_30, _node_26, _method, [_node_32, _node_33], '+', 0)),
      add(applyT(_node_32, _node_30, _method,  _node_35,'getName', [], _method_36)),
      add(applyT(_node_35, _node_34, _method, _node_39, 'getClass', [], _method_40)),
      add(identT(_node_39, _node_35, _method, 'o', ObjParam)),
      add(literalT(_node_33, _node_30, _method, type(class, _targetClass9, 0), _msg)),
      add_to_class(_targetClass, _method)
)).
consulted('unboxingMethod/3').



