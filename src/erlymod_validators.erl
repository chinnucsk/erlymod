-module (erlymod_validators).

-export([
	validate_value/2,
	type/1,
	chars_count/1,
	presents/0,
	choice/1,
	regex/1,
	with_func/3,
	with_func/4,
	positive/0,
	non_positive/0,
	negative/0,
	non_negative/0,
	to_proplists/1,
	is_presents_validator/1
]).

-type type() :: type | chars_count | presents | choice | regex | positive | non_negative | non_positive | negative.
-type arg_type() :: models_fields:field_type().
-type arg_chars_count() :: {eq, pos_integer()} |
								{max, pos_integer()} |
								{min, pos_integer()} |
								{in, [pos_integer()]}.
-type arg_presents() :: undefined.
-type arg_choice() :: [models_fields:field_value()].
-type arg_regex() :: iodata() | {iodata(), [re:compile_options()]}.
-type arg_positive() :: undefined.
-type arg_non_negative() :: undefined.
-type arg_negative() :: undefined.
-type arg_non_positive() :: undefined.

-type arg() :: arg_type() |
			   arg_chars_count() |
			   arg_presents() |
			   arg_choice() |
			   arg_regex() |
			   arg_positive() |
			   arg_non_negative() |
			   arg_negative() |
			   arg_non_positive() |
			   term().

-define(CHARS_COUNT_ERROR(Arg), [{chars_count_error, [Arg]}]).
-define(TYPE_ERROR(Arg), [{type_error, Arg}]).
-define(PRESENTS_ERROR, presents_error).
-define(POSITIVE_ERROR, positive_error).
-define(NON_NEGATIVE_ERROR, non_negative_error).
-define(NEGATIVE_ERROR, negative_error).
-define(NON_POSITIVE_ERROR, non_positive_error).
-define(CHOICE_ERROR(Arg), [{choice_error, Arg}]).
-define(REGEX_ERROR(Arg), [{regex_error, Arg}]).

-type chars_count_error() :: {chars_count_error, arg_chars_count()}.
-type type_error() :: {type_error, arg_type()}.
-type presents_error() :: presents_error.
-type positive_error() :: positive_error.
-type non_negative_error() :: non_negative_error.
-type non_positive_error() :: non_positive_error.
-type negative_error() :: negative_error.
-type choice_error() :: {choice_error, arg_choice()}.
-type regex_error() :: {regex_error, arg_regex()}.

-type error_description() :: chars_count_error() |
							 type_error() |
							 presents_error() |
							 choice_error() |
							 regex_error() |
							 positive_error() |
							 non_positive_error() |
							 non_negative_error() |
							 negative_error() |
							 term().

-type checker_func() :: fun((models_fields:field_value(), arg()) -> boolean()).

-record(validator, {
		validator_type :: type(),
		arguments :: arg(),
		error_description :: undefined | error_description(),
		checker_func :: undefined | checker_func()
		}).

-type validator() :: #validator{}.

-export_type([
		type/0,
		arg_type/0,
		arg_chars_count/0,
		arg_presents/0,
		arg_choice/0,
		arg_regex/0,
		arg_positive/0,
		arg/0,
		chars_count_error/0,
		type_error/0,
		presents_error/0,
		choice_error/0,
		regex_error/0,
		error_description/0,
		validator/0,
		checker_func/0
	]).

is_presents_validator(#validator{validator_type = presents}) -> true;
is_presents_validator(_Validator) -> false.

to_proplists(Validators) ->
	to_proplists(Validators, []).

to_proplists([], Json) -> lists:reverse(Json);
to_proplists([#validator{validator_type = Type, arguments = Args} | T], Json) ->
	NewJson = [value_to_proplists(simplify_validator_for_json(Type, Args)) | Json],
	to_proplists(T, NewJson).

-spec simplify_validator_for_json(type(), arg()) -> _.
simplify_validator_for_json(type, FieldType) ->
	{type, FieldType};
simplify_validator_for_json(choice, Options) ->
	[{type, choice}, {options, Options}];
simplify_validator_for_json(regex, Regex) ->
	[{type, regex}, {regex, Regex}];
simplify_validator_for_json(chars_count, {eq, N}) ->
	[{type, length}, {value, N}];
simplify_validator_for_json(chars_count, {max, N}) ->
	[{type, max_length}, {value, N}];
simplify_validator_for_json(chars_count, {min, N}) ->
	[{type, min_length}, {value, N}];
simplify_validator_for_json(chars_count, {in, [Min, Max]}) ->
	[{type, length_between}, {min, Min}, {max, Max}];
simplify_validator_for_json(ValidationType, undefined) ->
	{type, ValidationType}.

value_to_proplists(V) when is_tuple(V) -> [V];
value_to_proplists(V) -> V.

-spec validate_value(Value :: models_fields:value(), Validators :: [validator()]) -> ok | {error, [error_description()]}.
validate_value(Value, Validators) ->
	Results = [validate_value_with_validator(Value, Validator) || Validator <- Validators],
	Errors = lists:foldl(fun
							(ok, Acc) -> Acc;
							({error, Err}, Acc) -> [value_to_proplists(Err) | Acc]
						 end,
						 [], Results),
	case Errors of
		[] -> ok;
		_ -> {error, lists:reverse(Errors)}
	end.

validate_value_with_validator(Value, #validator{validator_type = type, arguments = Type}) ->
	validate_value_type(Value, Type);
validate_value_with_validator(Value, #validator{validator_type = chars_count, arguments = LengthAssert}) ->
	validate_value_chars_count(Value, LengthAssert);
validate_value_with_validator(Value, #validator{validator_type = presents}) ->
	validate_value_presence(Value);
validate_value_with_validator(Value, #validator{validator_type = positive}) ->
	validate_value_positive(Value);
validate_value_with_validator(Value, #validator{validator_type = non_negative}) ->
	validate_value_non_negative(Value);
validate_value_with_validator(Value, #validator{validator_type = negative}) ->
	validate_value_negative(Value);
validate_value_with_validator(Value, #validator{validator_type = non_positive}) ->
	validate_value_non_positive(Value);
validate_value_with_validator(Value, #validator{validator_type = choice, arguments = Choices}) ->
	validate_value_choices(Value, Choices);
validate_value_with_validator(Value, #validator{validator_type = regex, arguments = RegEx}) ->
	validate_value_with_regex(Value, RegEx);
validate_value_with_validator(Value, #validator{arguments = Args, error_description = Descr, checker_func = Checker}) ->
	try
		case Checker(Value, Args) of
			true -> ok;
			false -> {error, Descr}
		end
	catch
		_:_ -> {error, Descr}
	end.

validate_value_with_regex(Value, RegEx) ->
	validate_value_ignore_undefined(Value, RegEx, fun regex_match/1, ?REGEX_ERROR(RegEx)).

validate_value_choices(Value, Choices) ->
	validate_value_ignore_undefined(Value, Choices, fun(C) -> fun(V) -> lists:member(V, C) end end, ?CHOICE_ERROR(Choices)).

validate_value_presence(undefined) -> {error, ?PRESENTS_ERROR};
validate_value_presence(_) -> ok.

validate_value_positive(Value) ->
	validate_value_ignore_undefined(Value, fun(V) -> V > 0 end, ?POSITIVE_ERROR).

validate_value_non_negative(Value) ->
	validate_value_ignore_undefined(Value, fun(V) -> V >= 0 end, ?NON_NEGATIVE_ERROR).

validate_value_negative(Value) ->
	validate_value_ignore_undefined(Value, fun(V) -> V < 0 end, ?NEGATIVE_ERROR).

validate_value_non_positive(Value) ->
	validate_value_ignore_undefined(Value, fun(V) -> V =< 0 end, ?NON_POSITIVE_ERROR).

validate_value_ignore_undefined(Value, Checker, ErrorDescr) ->
	case is_undefined(Value) orelse exception_to_false(Checker, Value) of
		true -> ok;
		false -> {error, ErrorDescr}
	end.

validate_value_type(Value, Type) ->
	validate_value_ignore_undefined(Value, Type, fun type_checker/1, ?TYPE_ERROR(Type)).

validate_value_chars_count(Value, LengthAssert) ->
	validate_value_ignore_undefined(Value, LengthAssert, fun chars_count_checker/1, ?CHARS_COUNT_ERROR(LengthAssert)).

validate_value_ignore_undefined(Value, Arg, CheckerSelectFunc, ErrorDescr) ->
	case is_undefined(Value) orelse exception_to_false(CheckerSelectFunc(Arg), Value) of
		true -> ok;
		false -> {error, ErrorDescr}
	end.

-spec positive() -> validator().
positive() ->
	#validator{validator_type = positive, error_description = ?POSITIVE_ERROR}.

-spec non_positive() -> validator().
non_positive() ->
	#validator{validator_type = non_positive, error_description = ?NON_POSITIVE_ERROR}.

-spec negative() -> validator().
negative() ->
	#validator{validator_type = negative, error_description = ?NEGATIVE_ERROR}.

-spec non_negative() -> validator().
non_negative() ->
	#validator{validator_type = non_negative, error_description = ?NON_NEGATIVE_ERROR}.

-spec choice(Choices :: arg_choice()) -> validator().
choice(Choices) ->
	#validator{validator_type = choice, arguments = Choices, error_description = ?CHOICE_ERROR(Choices)}.

-spec type(Type :: arg_type()) -> validator().
type(Type) ->
	#validator{validator_type = type, arguments = Type, error_description = ?TYPE_ERROR(Type)}.

-spec presents() -> validator().
presents() ->
	#validator{validator_type = presents, error_description = ?PRESENTS_ERROR}.

-spec chars_count(LengthAssert :: arg_chars_count()) -> validator().
chars_count(LengthAssert) ->
	#validator{validator_type = chars_count, arguments = LengthAssert, error_description = ?CHARS_COUNT_ERROR(LengthAssert)}.

-spec regex(RegEx :: arg_regex()) -> validator().
regex(RegEx) ->
	#validator{validator_type = regex, arguments = RegEx, error_description = ?REGEX_ERROR(RegEx)}.

-spec with_func(Type :: atom(), Func :: checker_func(), ErrorDesc :: error_description(), Arg :: arg()) -> validator().
with_func(Type, Func, ErrorDesc, Arg) ->
	#validator{validator_type = Type, arguments = Arg, error_description = ErrorDesc, checker_func = Func}.

-spec with_func(Type :: atom(), Func :: checker_func(), ErrorDesc :: error_description()) -> validator().
with_func(Type, Func, ErrorDesc) ->
	with_func(Type, Func, ErrorDesc, undefined).


exception_to_false(Func, Arg) ->
	try
		Func(Arg)
	catch
		_:_ -> false
	end.

type_checker(string) ->
	fun is_string/1;
type_checker(text) ->
	fun is_string/1;
type_checker(integer) ->
	fun is_integer/1;
type_checker(number) ->
	fun is_number/1;
type_checker(boolean) ->
	fun is_boolean/1;
type_checker(binary) ->
	fun is_binary/1;
type_checker(date) ->
	fun is_date/1;
type_checker(time) ->
	fun is_time/1;
type_checker(datetime) ->
	fun is_datetime/1.

is_time({H, M, S}) ->
			is_integer(H) andalso
				 is_integer(M) andalso
				 is_integer(S) andalso
				 0 =< H andalso H =< 23 andalso
				 0 =< M andalso M =< 59 andalso
				 0 =< S andalso S =< 59;
is_time(_) -> false.

is_date(D) ->
	calendar:valid_date(D).

is_datetime({D, T}) ->
	is_date(D) andalso is_time(T);
is_datetime(_) -> false.

is_string(Value) ->
	is_list(Value) andalso core_string:is_string(Value).

is_undefined(undefined) -> true;
is_undefined(_) -> false.

string_chars_count(V) ->
	length(utils:utf8_bytes_to_char(V)).

chars_count_checker({eq, L}) -> chars_count_eq(L);
chars_count_checker({max, L}) -> chars_count_max(L);
chars_count_checker({min, L}) -> chars_count_min(L);
chars_count_checker({in, [Min, Max]}) -> chars_count_in(Min, Max).

chars_count_eq(L) ->
	fun(V) -> string_chars_count(V) =:= L end.

chars_count_max(L) ->
	fun(V) -> string_chars_count(V) =< L end.

chars_count_min(L) ->
	fun(V) -> string_chars_count(V) >= L end.

chars_count_in(Min, Max) ->
	fun(V) -> L = string_chars_count(V), Min =< L andalso L =< Max end.

-ifndef(TEST).
get_compiled_regex(Pattern, Options) ->
	regex_cache:get_compiled_regex(Pattern, Options).
-else.
get_compiled_regex(Pattern, Options) ->
	re:compile(Pattern, Options).
-endif.

regex_match({Pattern, Options}) ->
	{ok, MP} = get_compiled_regex(Pattern, Options),
	fun(V) -> case re:run(V, MP) of nomatch -> false; _ -> true end end;
regex_match(Pattern) -> regex_match({Pattern, []}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

validate_type_test_() -> [
		?_assertEqual(ok, validate_value("", [type(string)])),
		?_assertEqual(ok, validate_value("", [type(text)])),
		?_assertEqual(ok, validate_value(10, [type(integer)])),
		?_assertEqual(ok, validate_value(10, [type(number)])),
		?_assertEqual(ok, validate_value(10.1, [type(number)])),
		?_assertEqual(ok, validate_value(true, [type(boolean)])),
		?_assertEqual(ok, validate_value(false, [type(boolean)])),
		?_assertEqual(ok, validate_value(<<>>, [type(binary)])),
		?_assertEqual(ok, validate_value({2012, 2, 29}, [type(date)])),
		?_assertEqual(ok, validate_value({23, 59, 59}, [type(time)])),
		?_assertEqual(ok, validate_value({{2012, 2, 29}, {23, 59, 59}}, [type(datetime)])),
		?_assertEqual({error, [[{type_error, string}]]}, validate_value(10, [type(string)])),
		?_assertEqual({error, [[{type_error, text}]]}, validate_value(10.1, [type(text)])),
		?_assertEqual({error, [[{type_error, integer}]]}, validate_value("", [type(integer)])),
		?_assertEqual({error, [[{type_error, integer}]]}, validate_value(10.1, [type(integer)])),
		?_assertEqual({error, [[{type_error, number}]]}, validate_value(true, [type(number)])),
		?_assertEqual({error, [[{type_error, boolean}]]}, validate_value(1, [type(boolean)])),
		?_assertEqual({error, [[{type_error, binary}]]}, validate_value("<<>>", [type(binary)])),
		?_assertEqual({error, [[{type_error, date}]]}, validate_value("<<>>", [type(date)])),
		?_assertEqual({error, [[{type_error, date}]]}, validate_value({2011, 2, 29}, [type(date)])),
		?_assertEqual({error, [[{type_error, time}]]}, validate_value({2011, 2, 29}, [type(time)])),
		?_assertEqual({error, [[{type_error, datetime}]]}, validate_value({2011, 2, 29}, [type(datetime)])),
		?_assertEqual({error, [[{type_error, datetime}]]}, validate_value({{2012, 2, 29}, {24, 60, 60}}, [type(datetime)])),
		?_assertEqual(ok, validate_value(undefined, [type(type_doesnot_matter)]))
	].

validate_chars_count_test_() -> [
		?_assertEqual(ok, validate_value(lists:duplicate(10, "Ф"), [chars_count({eq, 10})])),
		?_assertEqual(ok, validate_value(lists:duplicate(10, "L"), [chars_count({max, 10})])),
		?_assertEqual(ok, validate_value(lists:duplicate(10, "L"), [chars_count({min, 10})])),
		?_assertEqual(ok, validate_value(lists:duplicate(10, "L"), [chars_count({in, [10, 10]})])),
		?_assertEqual({error, [[{chars_count_error, [{eq, 10}]}]]}, validate_value(lists:duplicate(11, "L"), [chars_count({eq, 10})])),
		?_assertEqual({error, [[{chars_count_error, [{max, 10}]}]]}, validate_value(lists:duplicate(11, "L"), [chars_count({max, 10})])),
		?_assertEqual({error, [[{chars_count_error, [{min, 10}]}]]}, validate_value(lists:duplicate(9, "L"), [chars_count({min, 10})])),
		?_assertEqual({error, [[{chars_count_error, [{in, [10, 20]}]}]]}, validate_value(lists:duplicate(9, "L"), [chars_count({in, [10, 20]})])),
		?_assertEqual({error, [[{chars_count_error, [{in, [10, 20]}]}]]}, validate_value(lists:duplicate(21, "L"), [chars_count({in, [10, 20]})]))
	].

validate_presents_test_() -> [
		?_assertEqual(ok, validate_value(any_value, [presents()])),
		?_assertEqual({error, [presents_error]}, validate_value(undefined, [presents()]))
	].

validate_choice_test_() -> [
		?_assertEqual(ok, validate_value(undefined, [choice([1, 2, 3, 4])])),
		?_assertEqual(ok, validate_value(1, [choice([1, 2, 3, 4])])),
		?_assertEqual({error, [[{choice_error, [1, 2, 3, 4]}]]}, validate_value(-1, [choice([1, 2, 3, 4])]))
	].

validate_regex_test_() -> [
		?_assertEqual(ok, validate_value("mama", [regex("mama")])),
		?_assertEqual({error, [[{regex_error, "mama"}]]}, validate_value("papa", [regex("mama")])),
		?_assertEqual({error, [[{regex_error, "mama"}]]}, validate_value(mama, [regex("mama")]))
	].

validate_with_func_test_() -> [
		?_assertEqual(ok, validate_value("test", [with_func(custom, fun(V, undefined) -> "test" =:= V end, {custom, "test"})])),
		?_assertEqual({error, [[{custom, "test"}]]}, validate_value("no_test", [with_func(custom, fun(V, undefined) -> "test" =:= V end, {custom, "test"})]))
	].

-endif.
