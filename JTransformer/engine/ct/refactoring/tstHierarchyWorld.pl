consulted('org/joint/exintexp/tstHierarchy/ILeft.pl').
packageT(100002, 'org.joint.exintexp.tstHierarchy').
toplevelT(100001, 100002, 'org/joint/exintexp/tstHierarchy/ILeft.java', [100003]).

methodDefT(100004, 100003,'l',[100005],type(basic, 'void', 0),[],'null').
      varDefT(100005, 100004, 100004, type(basic, 'int', 0), 'i', 'null').
methodDefT(100006, 100003,'lt',[100007],type(basic, 'void', 0),[],'null').
      varDefT(100007, 100006, 100006, type(basic, 'int', 0), 'i', 'null').
methodDefT(100008, 100003,'lm',[100009],type(basic, 'void', 0),[],'null').
      varDefT(100009, 100008, 100008, type(basic, 'int', 0), 'i', 'null').
methodDefT(100010, 100003,'lb',[100011],type(basic, 'void', 0),[],'null').
      varDefT(100011, 100010, 100010, type(basic, 'int', 0), 'i', 'null').
methodDefT(100012, 100003,'ltmb',[100013],type(basic, 'void', 0),[],'null').
      varDefT(100013, 100012, 100012, type(basic, 'int', 0), 'i', 'null').
classDefT(100003, 100002, 'ILeft', [100004, 100006, 100008, 100010, 100012]).
interfaceT(100003).
   modifierT(100003, 'public').
   extendsT(100003, 100014).

consulted('org/joint/exintexp/tstHierarchy/ITop.pl').
toplevelT(100015, 100002, 'org/joint/exintexp/tstHierarchy/ITop.java', [100016]).

methodDefT(100017, 100016,'t',[100018],type(basic, 'void', 0),[],'null').
      varDefT(100018, 100017, 100017, type(basic, 'int', 0), 'i', 'null').
methodDefT(100019, 100016,'lt',[100020],type(basic, 'void', 0),[],'null').
      varDefT(100020, 100019, 100019, type(basic, 'int', 0), 'i', 'null').
methodDefT(100021, 100016,'tb',[100022],type(basic, 'void', 0),[],'null').
      varDefT(100022, 100021, 100021, type(basic, 'int', 0), 'i', 'null').
methodDefT(100023, 100016,'ltmb',[100024],type(basic, 'void', 0),[],'null').
      varDefT(100024, 100023, 100023, type(basic, 'int', 0), 'i', 'null').
classDefT(100016, 100002, 'ITop', [100017, 100019, 100021, 100023]).
interfaceT(100016).
   modifierT(100016, 'public').
   extendsT(100016, 100014).

consulted('org/joint/exintexp/tstHierarchy/IMiddle.pl').
toplevelT(100025, 100002, 'org/joint/exintexp/tstHierarchy/IMiddle.java', [100026]).

methodDefT(100027, 100026,'m',[100028],type(basic, 'void', 0),[],'null').
      varDefT(100028, 100027, 100027, type(basic, 'int', 0), 'i', 'null').
methodDefT(100029, 100026,'lm',[100030],type(basic, 'void', 0),[],'null').
      varDefT(100030, 100029, 100029, type(basic, 'int', 0), 'i', 'null').
methodDefT(100031, 100026,'ltmb',[100032],type(basic, 'void', 0),[],'null').
      varDefT(100032, 100031, 100031, type(basic, 'int', 0), 'i', 'null').
methodDefT(100033, 100026,'l',[100034, 100035],type(basic, 'void', 0),[],'null').
      varDefT(100034, 100033, 100033, type(basic, 'int', 0), 'i', 'null').
      varDefT(100035, 100033, 100033, type(basic, 'int', 0), 'j', 'null').
methodDefT(100036, 100026,'tb',[100037, 100038],type(basic, 'void', 0),[],'null').
      varDefT(100037, 100036, 100036, type(basic, 'int', 0), 'i', 'null').
      varDefT(100038, 100036, 100036, type(basic, 'int', 0), 'j', 'null').
classDefT(100026, 100002, 'IMiddle', [100027, 100029, 100031, 100033, 100036]).
interfaceT(100026).
   modifierT(100026, 'public').
   extendsT(100026, 100014).
   implementsT(100026, 100016).

consulted('org/joint/exintexp/tstHierarchy/IBottom.pl').
toplevelT(100039, 100002, 'org/joint/exintexp/tstHierarchy/IBottom.java', [100040]).

methodDefT(100041, 100040,'b',[100042],type(basic, 'void', 0),[],'null').
      varDefT(100042, 100041, 100041, type(basic, 'int', 0), 'i', 'null').
methodDefT(100043, 100040,'lb',[100044],type(basic, 'void', 0),[],'null').
      varDefT(100044, 100043, 100043, type(basic, 'int', 0), 'i', 'null').
methodDefT(100045, 100040,'tb',[100046],type(basic, 'void', 0),[],'null').
      varDefT(100046, 100045, 100045, type(basic, 'int', 0), 'i', 'null').
methodDefT(100047, 100040,'ltmb',[100048],type(basic, 'void', 0),[],'null').
      varDefT(100048, 100047, 100047, type(basic, 'int', 0), 'i', 'null').
classDefT(100040, 100002, 'IBottom', [100041, 100043, 100045, 100047]).
interfaceT(100040).
   modifierT(100040, 'public').
   extendsT(100040, 100014).
   implementsT(100040, 100026).
   implementsT(100040, 100003).

consulted('org/joint/exintexp/tstHierarchy/TheClass.pl').
toplevelT(100049, 100002, 'org/joint/exintexp/tstHierarchy/TheClass.java', [100050]).

methodDefT(100051, 100050,'<init>',[],type(basic, 'void', 0),[],100052).
   modifierT(100051, 'public').
   blockT(100052, 100051, 100051, [100053]).
      execT(100053, 100052, 100051, 100054).
         applyT(100054, 100053, 100051, 100055, []).
            identT(100055, 100054, 100051, 'super', 100014).
methodDefT(100056, 100050,'c',[100057],type(basic, 'void', 0),[],100058).
   modifierT(100056, 'public').
      varDefT(100057, 100056, 100056, type(basic, 'int', 0), 'i', 'null').
   blockT(100058, 100056, 100056, []).
methodDefT(100059, 100050,'l',[100060],type(basic, 'void', 0),[],100061).
   modifierT(100059, 'public').
      varDefT(100060, 100059, 100059, type(basic, 'int', 0), 'i', 'null').
   blockT(100061, 100059, 100059, []).
methodDefT(100062, 100050,'b',[100063],type(basic, 'void', 0),[],100064).
   modifierT(100062, 'public').
      varDefT(100063, 100062, 100062, type(basic, 'int', 0), 'i', 'null').
   blockT(100064, 100062, 100062, []).
methodDefT(100065, 100050,'lt',[100066],type(basic, 'void', 0),[],100067).
   modifierT(100065, 'public').
      varDefT(100066, 100065, 100065, type(basic, 'int', 0), 'i', 'null').
   blockT(100067, 100065, 100065, []).
methodDefT(100068, 100050,'lm',[100069],type(basic, 'void', 0),[],100070).
   modifierT(100068, 'public').
      varDefT(100069, 100068, 100068, type(basic, 'int', 0), 'i', 'null').
   blockT(100070, 100068, 100068, []).
methodDefT(100071, 100050,'lb',[100072],type(basic, 'void', 0),[],100073).
   modifierT(100071, 'public').
      varDefT(100072, 100071, 100071, type(basic, 'int', 0), 'i', 'null').
   blockT(100073, 100071, 100071, []).
methodDefT(100074, 100050,'ltmb',[100075],type(basic, 'void', 0),[],100076).
   modifierT(100074, 'public').
      varDefT(100075, 100074, 100074, type(basic, 'int', 0), 'i', 'null').
   blockT(100076, 100074, 100074, []).
methodDefT(100077, 100050,'t',[100078],type(basic, 'void', 0),[],100079).
   modifierT(100077, 'public').
      varDefT(100078, 100077, 100077, type(basic, 'int', 0), 'i', 'null').
   blockT(100079, 100077, 100077, []).
methodDefT(100080, 100050,'tb',[100081],type(basic, 'void', 0),[],100082).
   modifierT(100080, 'public').
      varDefT(100081, 100080, 100080, type(basic, 'int', 0), 'i', 'null').
   blockT(100082, 100080, 100080, []).
methodDefT(100083, 100050,'m',[100084],type(basic, 'void', 0),[],100085).
   modifierT(100083, 'public').
      varDefT(100084, 100083, 100083, type(basic, 'int', 0), 'i', 'null').
   blockT(100085, 100083, 100083, []).
methodDefT(100086, 100050,'tb',[100087, 100088],type(basic, 'void', 0),[],100089).
   modifierT(100086, 'public').
      varDefT(100087, 100086, 100086, type(basic, 'int', 0), 'i', 'null').
      varDefT(100088, 100086, 100086, type(basic, 'int', 0), 'j', 'null').
   blockT(100089, 100086, 100086, []).
methodDefT(100090, 100050,'l',[100091, 100092],type(basic, 'void', 0),[],100093).
   modifierT(100090, 'public').
      varDefT(100091, 100090, 100090, type(basic, 'int', 0), 'i', 'null').
      varDefT(100092, 100090, 100090, type(basic, 'int', 0), 'j', 'null').
   blockT(100093, 100090, 100090, []).
methodDefT(100094, 100050,'<clinit>',[],type(basic, 'void', 0),[],100095).
   modifierT(100094, 'static').
   blockT(100095, 100094, 100094, []).
methodDefT(100096, 100050,'<clinit>',[],type(basic, 'void', 0),[],100097).
   blockT(100097, 100096, 100096, []).
classDefT(100050, 100002, 'TheClass', [100051, 100056, 100059, 100062, 100065, 100068, 100071, 100074, 100077, 100080, 100083, 100086, 100090, 100094, 100096]).
   modifierT(100050, 'public').
   extendsT(100050, 100014).
   implementsT(100050, 100040).
   implementsT(100050, 100003).

consulted('org/joint/exintexp/tstHierarchy/SomeClient.pl').
toplevelT(100098, 100002, 'org/joint/exintexp/tstHierarchy/SomeClient.java', [100099]).

methodDefT(100100, 100099,'<init>',[],type(basic, 'void', 0),[],100101).
   modifierT(100100, 'public').
   blockT(100101, 100100, 100100, [100102]).
      execT(100102, 100101, 100100, 100103).
         applyT(100103, 100102, 100100, 100104, []).
            identT(100104, 100103, 100100, 'super', 100014).
methodDefT(100105, 100099,'main',[100106],type(basic, 'void', 0),[],100107).
   modifierT(100105, 'public').
   modifierT(100105, 'static').
      varDefT(100106, 100105, 100105, type(class, 100108, 1), 'argv', 'null').
   blockT(100107, 100105, 100105, [100109, 100110, 100111, 100112, 100113, 100114, 100115, 100116, 100117, 100118, 100119, 100120, 100121]).
         newClassT(100123, 100109, 100105, 100051, [], 100122, 'null', null).
            identT(100122, 100123, 100105, 'TheClass', 100050).
         varDefT(100109, 100107, 100105, type(class, 100050, 0), 'myClass', 100123).
      execT(100110, 100107, 100105, 100124).
         applyT(100124, 100110, 100105, 100125, [100126]).
            selectT(100125, 100124, 100105, 'c', 100127, 100056).
               identT(100127, 100125, 100105, 'myClass', 100109).
            literalT(100126, 100124, 100105, type(basic, 'int', 0), '1').
      execT(100111, 100107, 100105, 100128).
         applyT(100128, 100111, 100105, 100129, [100130]).
            selectT(100129, 100128, 100105, 'l', 100131, 100059).
               identT(100131, 100129, 100105, 'myClass', 100109).
            literalT(100130, 100128, 100105, type(basic, 'int', 0), '2').
      execT(100112, 100107, 100105, 100132).
         applyT(100132, 100112, 100105, 100133, [100134]).
            selectT(100133, 100132, 100105, 't', 100135, 100077).
               identT(100135, 100133, 100105, 'myClass', 100109).
            literalT(100134, 100132, 100105, type(basic, 'int', 0), '3').
      execT(100113, 100107, 100105, 100136).
         applyT(100136, 100113, 100105, 100137, [100138]).
            selectT(100137, 100136, 100105, 'm', 100139, 100083).
               identT(100139, 100137, 100105, 'myClass', 100109).
            literalT(100138, 100136, 100105, type(basic, 'int', 0), '4').
      execT(100114, 100107, 100105, 100140).
         applyT(100140, 100114, 100105, 100141, [100142]).
            selectT(100141, 100140, 100105, 'b', 100143, 100062).
               identT(100143, 100141, 100105, 'myClass', 100109).
            literalT(100142, 100140, 100105, type(basic, 'int', 0), '5').
      execT(100115, 100107, 100105, 100144).
         applyT(100144, 100115, 100105, 100145, [100146]).
            selectT(100145, 100144, 100105, 'lt', 100147, 100065).
               identT(100147, 100145, 100105, 'myClass', 100109).
            literalT(100146, 100144, 100105, type(basic, 'int', 0), '6').
      execT(100116, 100107, 100105, 100148).
         applyT(100148, 100116, 100105, 100149, [100150]).
            selectT(100149, 100148, 100105, 'lm', 100151, 100068).
               identT(100151, 100149, 100105, 'myClass', 100109).
            literalT(100150, 100148, 100105, type(basic, 'int', 0), '7').
      execT(100117, 100107, 100105, 100152).
         applyT(100152, 100117, 100105, 100153, [100154]).
            selectT(100153, 100152, 100105, 'lb', 100155, 100071).
               identT(100155, 100153, 100105, 'myClass', 100109).
            literalT(100154, 100152, 100105, type(basic, 'int', 0), '8').
      execT(100118, 100107, 100105, 100156).
         applyT(100156, 100118, 100105, 100157, [100158]).
            selectT(100157, 100156, 100105, 'tb', 100159, 100080).
               identT(100159, 100157, 100105, 'myClass', 100109).
            literalT(100158, 100156, 100105, type(basic, 'int', 0), '9').
      execT(100119, 100107, 100105, 100160).
         applyT(100160, 100119, 100105, 100161, [100162]).
            selectT(100161, 100160, 100105, 'ltmb', 100163, 100074).
               identT(100163, 100161, 100105, 'myClass', 100109).
            literalT(100162, 100160, 100105, type(basic, 'int', 0), '10').
      execT(100120, 100107, 100105, 100164).
         applyT(100164, 100120, 100105, 100165, [100166, 100167]).
            selectT(100165, 100164, 100105, 'l', 100168, 100090).
               identT(100168, 100165, 100105, 'myClass', 100109).
            literalT(100166, 100164, 100105, type(basic, 'int', 0), '11').
            literalT(100167, 100164, 100105, type(basic, 'int', 0), '12').
      execT(100121, 100107, 100105, 100169).
         applyT(100169, 100121, 100105, 100170, [100171, 100172]).
            selectT(100170, 100169, 100105, 'tb', 100173, 100086).
               identT(100173, 100170, 100105, 'myClass', 100109).
            literalT(100171, 100169, 100105, type(basic, 'int', 0), '13').
            literalT(100172, 100169, 100105, type(basic, 'int', 0), '14').
methodDefT(100174, 100099,'<clinit>',[],type(basic, 'void', 0),[],100175).
   modifierT(100174, 'static').
   blockT(100175, 100174, 100174, []).
methodDefT(100176, 100099,'<clinit>',[],type(basic, 'void', 0),[],100177).
   blockT(100177, 100176, 100176, []).
classDefT(100099, 100002, 'SomeClient', [100100, 100105, 100174, 100176]).
   modifierT(100099, 'public').
   extendsT(100099, 100014).
