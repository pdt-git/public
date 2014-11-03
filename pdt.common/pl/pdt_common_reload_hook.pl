:- module(pdt_common_reload_hook, []).

:- multifile(pdt_reload:reload_message/2).

pdt_reload:reload_message(automatic_reconsult(all), 'INFO Reconsulting previously loaded files\nINFO For turning reconsulting off or for reconsulting just entry point files\nINFO use another option from the drop-down menu of the Restart button\nINFO or change the corresponding preference').
pdt_reload:reload_message(automatic_reconsult(entry_points), 'INFO Reconsulting PDT entry point files\nINFO For turning reconsulting off or for reconsulting all consulted files\nINFO use another option from the drop-down menu of the Restart button\nINFO or change the corresponding preference').