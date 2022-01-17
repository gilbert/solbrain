:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes,codes).


test(A, Leftover) :-
  phrase_from_file(program_from_file(A, Leftover), '../test/Foo.sol').

%% test_t(Tokens, Leftover) :-
%%   phrase_from_file(tokens_from_file(Tokens, Leftover), 'test/Foo.sol').

%% tokens_from_file(Tokens, Leftover) -->
%%   tokens(Tokens),
%%   remainder(Leftover0),
%%   { append(Leftover0, [], Leftover) }. % Force out lazy stream

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
%% Match helpers
%%
match(Term), [s(A)] --> { is_list(Term), !, [X|XS] = Term }, [s(A), X | XS].
match(Term), [s(A)] --> [s(A), Term].

match_req(Term, _)   --> match(Term).
match_req(_, ErrMsg) --> { throw(parse_err(ErrMsg)) }.

%%
%% Tokenizer
%%
op_chars(`=+-/^<>`).


tokenize, [T|Ts] --> tokens([T|Ts]).

tokens(Ts)     --> ws, !, tokens(Ts).
tokens([T|Ts]) --> token(T), !, tokens(Ts).
tokens([])     --> [].

token(punc(T)) --> punc(T0), { atom_codes(T, [T0]) }.
token(op(T))   --> operator(T0), { atom_codes(T, T0) }.
token(word(T)) --> word(T0), { atom_codes(T, T0) }.

operator([C]), [Sep] --> opr(C), [Sep], { op_chars(Ops), \+ member(Sep, Ops) }.
operator([C|Cs])     --> opr(C), operator(Cs).

word([C]), [Sep] --> [C], non_word_char(Sep).
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
  contract_body_elem(Contract),
  match(punc('}')).

contract_body_elem(Contract) --> state_var_def(Contract).

state_var_def(Contract) -->
  type_name(Type),
  %% TODO: Visibility
  identifier(Id),
  {write(id2(Type, Id)),nl},
  %% TODO: Assignment expression
  push([contract, Contract], [state_var, Type, Id]),
  {write(yeh),nl},
  match(punc(';')).


type_name(Type) --> elementary_type_name(Type).

elementary_type_name(address) --> match(word(address)).
elementary_type_name(address_payable) --> match(word(address_payable)).
elementary_type_name(bool) --> match(word(bool)).
elementary_type_name(string) --> match(word(string)).
elementary_type_name(bytes) --> match(word(bytes)).
elementary_type_name(fixed) --> match(word(fixed)).
elementary_type_name(ufixed) --> match(word(ufixed)).
elementary_type_name(Type) --> signed_integer_type(Type).
elementary_type_name(Type) --> unsigned_integer_type(Type).


signed_integer_type(int256) --> match(word(int)).
%% TODO

unsigned_integer_type(uint256) --> match(word(uint)).
%% TODO

pragma -->
  match(word(pragma)),
  pragma_text(Text),
  state(pragma, P),
  update(pragma, [Text | P]).

pragma_text([]) --> match(punc(';')).
pragma_text([X|Xs]) --> match(X), pragma_text(Xs).


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
