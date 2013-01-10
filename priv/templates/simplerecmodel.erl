-module ({{modname}}).

-behaviour(erlymod_gen_model).

-extend(erlymod).

-include("{{modname}}.hrl").

-export([
	construct/0,
	fields/0,
	fields_values/1,
	field_info/2,
	to_proplists/1,
	from_proplists/1,
	key/1,
	bucket/1
	]).

-spec construct() -> erlymod:model_instance().
construct() ->
	#{{modname}}{}.

-spec fields() -> [erlymod:model_field_name()].
fields() ->
	record_info(fields, {{modname}}).

-spec field_values(erlymod:model_instance()) -> [{erlymod:model_field_name(), erlymod:model_field_value()}].
fields_values(Instance) ->
	[{Name, Value} || Value <- values(Instance), Name <- fields()].

-spec field_info(erlymod_gen_model:field_info_request(), erlymod:model_field_name()) -> erlymod_gen_model:field_info_response().
field_info(validators, _FieldName) ->
	throw(not_implemented).

-spec to_proplists(erlymod:model_instance()) -> [{elrymod:model_field_name(), erlymod:model_field_value()}].
to_proplists(Instance) ->
	[{Name, field_to_proplist(Name, Value)} || {Name, Value} <- field_values(Instance)].

-spec from_proplists([{elrymod:model_field_name(), erlymod:model_field_value()}]) -> erlymod:model_instance().
from_proplists(Properties) ->
	Empty = construct(),
	Fields = fields(),
	NamesAndIndex = lists:zip(Fields, lists:seq(2, length(Fields) + 1)),
	Defaults = to_proplists(Empty),
	lists:foldl(fun({Name, Index}, Rec) ->
				setelement(Index, Rec, field_from_proplist(Name, proplists:get_value(Name, Properties, proplists:get_value(Name, Defaults)))) end,
				Empty, NamesAndIndex).

-spec key(erlymod:model_instance()) -> erlymod:model_field_value().
key(_Instance) ->
	throw(not_implemented).

-spec bucket(erlymod:model_instance()) -> atom().
bucket(_Instance) ->
	{{modname}}.


field_to_proplist(_FildName, FieldValue) ->
	FieldValue.

field_from_proplist(_FieldName, FieldValue) ->
	FieldValue.

values(Instance) ->
	tl(tuple_to_list(Instance)).

