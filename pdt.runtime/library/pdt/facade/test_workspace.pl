:- ensure_loaded(pdt_facade).
:- ensure_loaded(library('pef/pef_base')).
:- ensure_loaded(library('pef/pef_api')).
:- tspy(pdt_builder:spyme).
:- debug(builder(build(_))).
%:- debug(builder(transition(problems('/home/lukas/workspace/pdt.runtime/library/pdt/util/pdt_render_term.pl')))).
create_test_project(Path):-
    pef_reserve_id(pef_project,Project),
    pef_reserve_id(pef_source_path,SourcePath),
    pef_project_assert([id=Project,name=test_project]),
    pef_source_path_assert([id=SourcePath,path=Path,include_pattern='.*\.pl', exclude_pattern='']).