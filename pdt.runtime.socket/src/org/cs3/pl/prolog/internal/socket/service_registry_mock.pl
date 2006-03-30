:- module(service_registry,
     [nearest_service_registry/3,
      service_available/3]).
     
nearest_service_registry(_IP, RegistryIP, Port):-
    RegistryIP = '127.0.0.1',
    Port = 9944.
    %use 131.220.155.88 on gorbag!
    
service_available(Servicename, Package, Url):-
	Servicename = 'Indexing',
	Package = 'org.cs3.csi.adaption.IIndexingAdaption',
	Url = 'http://www.informatik.uni-bonn.de/~speicher/pimpro/BundleCSIIndexing-1.0.3.jar'.
%	Url = 'file:///home/pierre/workspace/BundleCSIIndexing/out/BundleCSIIndexing-1.0.3.jar'.
%	Url = 'file:///C:/Documents and Settings/Daniel Speicher/workspace_j9/BundleCSIIndexing/out/BundleCSIIndexing-1.0.3.jar'.	
%	Url = 'file:///C:/eclipse/workpace_ercp/BundleCSIIndexing/out/BundleCSIIndexing-1.0.3.jar'.	
%	Url = 'file:///d:/javaproj/csi/BundleCSIIndexing/out/BundleCSIIndexing-1.0.3.jar'.
	
service_available(Servicename, Package, Url):-
	Servicename = 'gradual distance Categorizer',
	Package = 'org.cs3.csi.adaption.CategorizationAdaption',
	Url = 'http://www.informatik.uni-bonn.de/~speicher/pimpro/BundleCSICategorization-1.0.4.jar'.
%	Url = 'file:///home/pierre/workspace/BundleCSICategorization/out/BundleCSICategorization-1.0.4.jar'.
%	Url = 'file:///d:/javaproj/csi/BundleCSICategorization/out/BundleCSICategorization-1.0.3.jar'.
%	Url = 'file:///C:/Documents and Settings/Daniel Speicher/workspace_j9/BundleCSICategorization/out/BundleCSICategorization-1.0.3.jar'.	
%	Url = 'file:///C:/eclipse/workpace_ercp/BundleCSICategorization/out/BundleCSICategorization-1.0.3.jar'.	

service_available(Servicename, Package, Url):-
	Servicename = 'vicinity distance Categorizer',
	Package = 'org.cs3.csi.adaption.CategorizationAdaption',
	Url = 'http://www.informatik.uni-bonn.de/~speicher/pimpro/BundleCSIVDCategorization-1.0.0.jar'.
	
service_available(Servicename, Package, Url):-
	Servicename = 'closeup distance Categorizer',
	Package = 'org.cs3.csi.adaption.CategorizationAdaption',
	Url = 'http://www.informatik.uni-bonn.de/~speicher/pimpro/BundleCSICDCategorization-1.0.0.jar'.
	
service_available(Servicename, Package, Url):-
	Servicename = 'NearCompanies',
	Package = 'org.cs3.csi.nearcompanies',
	Url = 'http://www.informatik.uni-bonn.de/~speicher/pimpro/BundleCSINearCompanies-1.0.0.jar'.
