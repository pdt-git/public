/* 
 * This is a first dummy-metric counting only the number of classes in a package
 *
 * call it 
 *   ->  metric(+Packagename,-Result)
 *
 */

metric(Packagename, Result) :-
    countClass('org.cs3.timetracker',Result).
    %countClass(Packagename,Result).
    
classInPackage(PN,CID) :-
    packageT(PID,PN),
    classDefT(CID,PID,_,_).    

countClass(PN,Result) :-
    findall(CID,classInPackage(PN,CID),ClassList),
    length(ClassList,Result).
    
    