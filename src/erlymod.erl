-module (erlymod).


-type model_name() :: atom().
-type model_instance() :: tuple().
-type model_field_name() :: atom().
-type model_field_value() :: term().

-export_type([
		model_name/0,
		model_instance/0,
		model_field_name/0,
		model_field_value/0
		]).

-export([
	new/1,
	validate_fields/1
	]).

-spec new(model_name()) -> model_instance().
new(ModelName) ->
	ModelName:construct().

-spec validate_fields(model_instance()) -> ok | {validate_error, [{model_field_name(), [erlymod_validators:error_description()]}]}.
validate_fields(Instance) ->
	ModelName = model_name(Instance),
	Values = fields_values(Instance),
	Results = lists:foldl(fun({Name, Value}, Result) ->
				case validate_field(ModelName, Name, Value) of
					ok -> Result;
					{error, Desc} -> [{Name, Desc} | Result]
				end
		end, [], Values),
	if length(Results) == 0 -> ok;
		true -> {validate_error, Results}
	end.



validate_field(ModelName, FieldName, Value) ->
	Validators = field_info(ModelName, validators, FieldName),
	erlymod_validators:validate_value(Value, Validators).

%% private
model_name(Instance) when is_tuple(Instance) ->
	element(1, Instance).

fields(ModelName) when is_atom(ModelName)->
	ModelName:fields();
fields(Instance) when is_tuple(Instance) ->
	fields(model_name(Instance)).

fields_values(Instance) when is_tuple(Instance) ->
	ModelName = model_name(Instance),
	ModelName:fields_values(Instance).

field_info(ModelName, Request, FieldName) when is_atom(ModelName), is_atom(FieldName) ->
	ModelName:field_info(Request, FieldName);
field_info(Instance, Request, FieldName) when is_tuple(Instance), is_atom(FieldName) ->
	field_info(model_name(Instance), Request, FieldName).

to_proplists(Instance) when is_tuple(Instance) ->
	ModelName = model_name(Instance),
	ModelName:to_proplists(Instance).

from_proplists(ModelName, PropLists) when is_atom(ModelName) ->
	ModelName:from_proplists(PropLists).

key(Instance) when is_tuple(Instance) ->
	ModelName = model_name(Instance),
	ModelName:key(Instance).

bucket(Instance) when is_tuple(Instance) ->
	ModelName = model_name(Instance),
	ModelName:bucket(Instance).

