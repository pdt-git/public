:- use_module(pdt_runtime_builder_analyzer('metafile_referencer.pl')).
:- use_module(pdt_runtime_builder_analyzer(pdt_xref_experimental)).
:- use_module(pdt_runtime_builder_analyzer(properties)).

:- use_module(org_cs3_lp_utils(utils4modules)).

:- initialization((
    logtalk_load(library(types_loader)),
    logtalk_load(library(metapredicates_loader)),
    logtalk_load(logtalk_adapter),
    logtalk_load(utils4entities)
%    logtalk_load(utils4entities_facade)
)).