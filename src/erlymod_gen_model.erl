-module (erlymod_gen_model).


-type field_info_request() :: validators.
-type field_info_validators() :: [erlymod_validators:validator()].
-type field_info_response() :: field_info_validators().

-export_type([
		field_info_request/0,
		field_info_validators/0,
		field_info_response/0
		]).

-callback construct() -> erlymod:model_instance().
-callback fields() -> [erlymod:model_field_name()].
-callback fields_values(erlymod:model_instance()) -> [{erlymod:model_field_name(), erlymod:model_field_value()}].
-callback field_info(field_info_request(), erlymod:model_field_name()) -> field_info_response().
-callback to_proplists(erlymod:model_instance()) -> [{elrymod:model_field_name(), erlymod:model_field_value()}].
-callback from_proplists([{elrymod:model_field_name(), erlymod:model_field_value()}]) -> erlymod:model_instance().
-callback key(erlymod:model_instance()) -> erlymod:model_field_value().
-callback bucket(erlymod:model_instance()) -> atom().
