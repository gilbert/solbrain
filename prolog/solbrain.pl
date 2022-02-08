:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes,codes).


test(A, Leftover) :-
  phrase_from_file(program_from_file(A, Leftover), '../test/Foo.sol').

test_t(Tokens, Leftover) :-
  phrase_from_file(tokens_from_file(Tokens, Leftover), '../test/Foo.sol').

tokens_from_file(Tokens, Leftover) -->
  tokens(Tokens),
  remainder(Leftover0),
  { append(Leftover0, [], Leftover) }. % Force out lazy stream

program_from_file(A, Leftover) -->
  tokenize,
  program,
  [A],
  remainder(Leftover0),
  { append(Leftover0, [], Leftover) }. % Force out lazy stream


%%
%% State helpers
%%
newstate, [S] --> { init_state(S) }.

init_state(s(A)) :-
  empty_assoc(A0)
  , put_assoc(pragma, A0, [], A)
  .

state(A), [s(A)] --> [s(A)].
state(K, V), [s(A)] --> [s(A)], { get_assoc(K, A, V) }.

update(A), [s(A)] --> [s(_)].
update(K, V), [s(A)] --> [s(A0)], { put_assoc(K, A0, V, A) }.

push(K, X) --> state(K, Xs), update(K, [X | Xs]).

%%
%% Tokenizer
%%
op_chars(`=+-/^<>`).


tokenize, [T|Ts] --> tokens([T|Ts]).

tokens(Ts)     --> ws, !, tokens(Ts).
tokens([T|Ts]) --> token(T), !, tokens(Ts).
tokens([])     --> [].

token(string(S)) --> `"`, match_string(S, `"`).
token(string(S)) --> `'`, match_string(S, `'`).
token(punc(T))   --> punc(T0), { atom_codes(T, [T0]) }.
token(op(T))     --> operator(T0), { atom_codes(T, T0) }.
token(word(T))   --> word(T0), { atom_codes(T, T0) }.


match_string([], [End]) --> [End].
match_string([92,End|Cs], [End]) --> [92, End], match_string(Cs, [End]). % Escaped quote (92 = backslash)
match_string([C|Cs], End) --> [C], match_string(Cs, End).

operator([C]), [Sep] --> opr(C), [Sep], { op_chars(Ops), \+ member(Sep, Ops) }.
operator([C|Cs])     --> opr(C), operator(Cs).

word([C]), [Sep] --> [C], (non_word_char(Sep); call(eos_)).
word([C|Cs])     --> [C], word(Cs).

non_word_char(C) --> (space(C); punc(C); opr(C)).

punc(C) --> [C], { member(C, `()[]{};.,`) }.

opr(C) --> [C], { op_chars(Ops), member(C, Ops) }.

ws --> spaces(X), { length(X, N), N > 0 }.
ws --> `//`, comment.

spaces([C|Cs]) --> space(C), !, spaces(Cs).
spaces([]) --> [].

space(C) --> [C], { code_type(C, space) }.

comment --> call(eos_), !.
comment --> `\r\n`, !.
comment --> `\n`, !.
comment --> [_], comment.

%% From https://www.metalevel.at/prolog/dcg
eos_([], []).


%%
%% Parser
%%
program --> newstate, source_unit.

source_unit --> pragma, !, source_unit.
source_unit --> contract_def, !, source_unit.
source_unit --> [].

contract_def -->
  (match(word(abstract)) -> {Abstract = true}; {Abstract = false}),
  match(word(contract)),
  identifier(Contract),
  update([contract, Contract], []),
  %% TODO: Check for duplicate identifiers
  %% TODO: Inheritance
  match(punc('{')),
  star(contract_body_elem(Contract)),
  match(punc('}')).

contract_body_elem(Contract) --> state_var_def(Contract).
contract_body_elem(Contract) --> function_def(Contract).

state_var_def(Contract) -->
  type_name(Type),
  visibility(Vis),
  identifier(Id),
  (match(op('=')) ->
    expression(Init);
    { Init = noop }
  ),
  push([contract, Contract], [state_var, Type, Vis, Id, Init]),
  match(punc(';')).

visibility(public)    --> match(word(public)), !.
visibility(private)   --> match(word(private)), !.
visibility(internal)  --> match(word(internal)), !.
visibility(constant)  --> match(word(constant)), !.
visibility(immutable) --> match(word(immutable)), !.
visibility(default)   --> [], !.
% TODO: override-specifier

%% Expressions
%%
expression(E) --> literal(E).

% LAST TIME: PARSE SET GREETING
% LAST TIME: PARSE SET GREETING
% LAST TIME: PARSE SET GREETING
% LAST TIME: PARSE SET GREETING
% LAST TIME: PARSE SET GREETING
% LAST TIME: PARSE SET GREETING

%% TODO: identifier
%% TODO: square brackets
%% TODO: square brackets colon
%% TODO: dot
%% TODO: curly brackets
%% TODO: call-argument-list
%% TODO: type
%% TODO: prefix operators
%% TODO: postfix operators
%% TODO: math operators
%% TODO: binary operators
%% TODO: boolean operators
expression(['=', X, Y]) --> expression(X), match(punc('=')), expression(Y).
%% TODO: combo assignment operators
%% TODO: new
%% TODO: tuple
%% TODO: inline array
%% TODO: elementary-type-name


%% Functions
%%
function_def(Contract) -->
  match(word(function)),
  identifier(Fn),
  { member(Fn, [fallback, receive]) -> throw(legacy_err(fn_name, Fn)); true },
  match(punc('(')),
  parameter_list(Params),
  log(params(Params)),
  match(punc(')')),

  update([contract, Contract, fn, Fn, body],   []),
  update([contract, Contract, fn, Fn, params], Params),

  %% TODO: Visibility
  %% TODO: Pure/view/payable
  %% TODO: Virtual
  %% TODO: Overrides
  %% TODO: Returns

  %% TODO: Semicolon without block
  block(Contract, Fn).

parameter_list([P|Ps]) --> parameter(P), !, (match(punc(',')) -> parameter_list(Ps); { Ps = [] }).
parameter_list([]) --> [].

parameter([Type, DataLoc, Id]) -->
  type_name(Type),
  data_location(DataLoc),
  identifier(Id).

block(Contract, Fn) -->
  match(punc('{')),
  star(statement(Contract, Fn)),
  match(punc('}')).

statement(Contract, Fn) -->
  expression(E),
  match(punc(';')),
  push([contract, Contract, fn, Fn, body], E).
%% TODO: block
%% TODO: variable-declaration-statement
%% TODO: if-statement
%% TODO: for-statement
%% TODO: while-statement
%% TODO: do-while-statement
%% TODO: continue-statement
%% TODO: break-statement
%% TODO: try-statement
%% TODO: return-statement
%% TODO: emit-statement
%% TODO: revert-statement
%% TODO: assembly-statement


%% Literals
%%
literal(L) --> string_literal(L).

string_literal(L) --> match(string(L)).


%% Types
%%
type_name(Type) --> elementary_type_name(Type).

elementary_type_name(bool)            --> match(word(bool)).
elementary_type_name(bytes)           --> match(word(bytes)).
elementary_type_name(fixed)           --> match(word(fixed)).
elementary_type_name(string)          --> match(word(string)).
elementary_type_name(ufixed)          --> match(word(ufixed)).
elementary_type_name(address)         --> match(word(address)).
elementary_type_name(address_payable) --> match(word(address_payable)).
elementary_type_name(Type)            --> fixed_bytes(Type).
elementary_type_name(Type)            --> signed_integer_type(Type).
elementary_type_name(Type)            --> unsigned_integer_type(Type).

signed_integer_type(int256) --> match(word(int)).
signed_integer_type(Word) --> integer_type(`int`, Word).

unsigned_integer_type(uint256) --> match(word(uint)).
unsigned_integer_type(Word) --> integer_type(`uint`, Word).

integer_type(Prefix, Word) --> match(word(Word)), {
  atom_codes(Word, Codes),
  append(Prefix, Bits, Codes),
  number_codes(N, Bits),
  N >= 8, N =< 256,
  N mod 8 =:= 0
}.

fixed_bytes(Word) --> match(word(Word)), {
  atom_codes(Word, Codes),
  append(`bytes`, Bits, Codes),
  number_codes(N, Bits),
  N >= 1, N =< 32
}.

pragma -->
  match(word(pragma)),
  pragma_content(Text),
  state(pragma, P),
  update(pragma, [Text | P]).

pragma_content([]) --> match(punc(';')).
pragma_content([X|Xs]) --> match(X), pragma_content(Xs).

data_location(memory) --> match(word(memory)), !.
data_location(storage) --> match(word(storage)), !.
data_location(calldata) --> match(word(calldata)), !.
data_location(default) --> [], !.

%%
%% Elemental Matchers
%%
identifier(Id) --> match(word(Id)), { atom_codes(Id, Codes), identifier_text(Codes) }.

identifier_text([X|Xs]) :-
  (code_type(X, alpha); member(X, `$_`)),
  identifier_text_rest(Xs).

identifier_text_rest([]).
identifier_text_rest([X|Xs]) :-
  (code_type(X, alnum); member(X, `$_`)),
  identifier_text_rest(Xs).

%%
%% Match helpers
%%
match(Term), [s(A)] --> { is_list(Term), !, [X|XS] = Term }, [s(A), X | XS].
match(Term), [s(A)] --> [s(A), Term].

match_req(Term, _)   --> match(Term).
match_req(_, ErrMsg) --> { throw(parse_err(ErrMsg)) }.

star(Goal) --> call(Goal), star(Goal).
star(_)    --> [].

%% Debug
%%
log(Term) --> { write(Term), nl }.
