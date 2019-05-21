:- module(alexa_mod, [
  alexa/1,
  addDotsToFacts/2
]).

:- use_module(library(race/ape)).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).

% wrapper predicate to store the knowledge (not persistently)
:- dynamic knowledge/1.

% Add debug; all debug messages of debug/3 with 'alexa' on the first argument will be printed
% :- debug(alexa).

% ----- Prolog Webserver Part II -----

% Generate the JSON output in the variable DictOut
alexa(Request) :-
  % Extract the information of the received JSON input and store it in the variable DictIn
  http_read_json_dict(Request, DictIn),
  % Compute DictOut depending on the given DictIn
  handle_dict(DictIn, DictOut),
  % Generate the JSON reply
  reply_json(DictOut).


% ----- Prolog Model -----

% Compute DictOut depending on the given DictIn
handle_dict(DictIn, DictOut) :-
  % Extract the IntentName for the following execution
  get_intent(DictIn, IntentName),
  % Compute the DictOut for the inserted DictIn and the corresponding IntentName
  intent_dictOut(IntentName, DictIn, DictOut).

% Extract the IntentName by using get_dict/3, which return the information that is associated with the first argument
get_intent(DictIn, IntentName) :-
  get_dict(request, DictIn, RequestObject),
  get_dict(intent, RequestObject, IntentObject),
  get_dict(name, IntentObject, IntentName).


% Execute the remember Intent of the skill
intent_dictOut("remember", DictIn, DictOut) :-
  % Extract important infomation of the DictIn by using the predefined predicate get_dict/3
  get_dict(request, DictIn, RequestObject),
  get_dict(intent, RequestObject, IntentObject),
  get_dict(slots, IntentObject, SlotsObject),
  get_dict(myslot, SlotsObject, MySlotObject),
  get_dict(value, MySlotObject, ValueMaybeDot),
  % Add a dot to a sentence, if there is no dot available in the inserted sentence; this is required for RACE
  maybe_dot(ValueMaybeDot, Value),
  % debug message
  debug(alexa, 'remember intent with value: ~w~n', [Value]),
  % Convert the Value of the slot to a String
  atom_string(ValueAtom, Value),
  % Check consistency of the inserted sentence by using the predicate check_consistency of the library(race)
  check_consistency(ValueAtom, Inconsistencies, Variant),
  % check, if RACE returned insonsistencies
  ( Inconsistencies \= [] ->
    % if there are Inconsistencies, store them
    maplist(remove_fact, Inconsistencies, InconsistenciesFact),
    % Convert them into a String
    atomics_to_string(InconsistenciesFact, ' ', InconsistenciesFactReason),
    debug(alexa, 'Inconsistent fact because: ~w~n', InconsistenciesFactReason),
    % Concat the answer
    atom_concat('Inconsistent fact because: ', InconsistenciesFactReason, InconsistenciesFactOutput),
    % Generate DictOut
    my_json_answer(InconsistenciesFactOutput, DictOut)
  ; otherwise ->
    % if there are no insonsistencies
    % Get facts from the database and combine it with the inserted sentence
    combine_sentences(Variant, CombinedSentences),
    !,
    % Check consistency of the database together with the inserted sentence by using the predicate check_consistency of the library(race)
    check_consistency(CombinedSentences, InconsistenciesDatabase),
    % check if RACE returned insonsistencies
    ( InconsistenciesDatabase \= [] ->
      % if there are inconsistencies, store them
      maplist(remove_fact, InconsistenciesDatabase, InconsistenciesDatabaseWithoutFact),
      % Convert them into a String
      atomics_to_string(InconsistenciesDatabaseWithoutFact,' ', InconsistenciesDatabaseReason),
      debug(alexa, 'Inconsistent with database because: ~w~n', InconsistenciesDatabaseReason),
      % Concat the answer
      atom_concat('Inconsistent database because: ', InconsistenciesDatabaseReason, InconsistenciesDatabaseOutput),
      % Generate DictOut
      my_json_answer(InconsistenciesDatabaseOutput, DictOut)
    ; otherwise ->
      % if there are no inconsistencies
      % Add the new fact to the database
      assert(knowledge(Variant)),
      % Concat the answer
      atom_concat('This fact was saved in the database: ', Value, SuccessfulAnswer),
      debug(alexa, 'This fact was saved in the database: ~w~n', Value),
      % Generate DictOut
      my_json_answer(SuccessfulAnswer, DictOut)
    )
  ).

% Execute the question Intent of the skill
intent_dictOut("question", DictIn, DictOut) :-
  % Extract important infomation of the DictIn by using the predefined predicate get_dict/3
  get_dict(request, DictIn, RequestObject),
  get_dict(intent, RequestObject, IntentObject),
  get_dict(slots, IntentObject, SlotsObject),
  get_dict(questionslot, SlotsObject, MySlotObject),
  get_dict(value, MySlotObject, ValueMaybeQuestionmark),
  % Add a questionmark to a sentence, if there is no questionmark available in the inserted sentence; this is required for RACE
  maybe_questionmark(ValueMaybeQuestionmark,Value),
  % debug message
  debug(alexa, 'question intent with value: ~w~n', [Value]),
  % Get the whole knowledge which is stored in the database
  get_whole_database(WholeDatabase),
  % Solve if the inserted question can be answered with the available knowledge by using ask_with_answers/3 of the library(race)
  ask_with_answers(WholeDatabase, Value, Result),
  % Check if the received result contains the term 'results'
  ( Result = results([ResultsString|_]) ->
    % if there are results print the first derivation
    debug(alexa, 'Yes, the question could be answered with: ~w~n', [ResultsString]),
    % Concat the answer
    string_concat('Yes, the question could be answered with: ', ResultsString, AnswerQuery)
  ; otherwise ->
    % if the question could not be answered
    AnswerQuery = 'The question could not be answered because the requested facts are not available in the database. Try again.'
  ),
  % Generate DictOut
  my_json_answer(AnswerQuery, DictOut).


% Execute the prove Intent of the skill
intent_dictOut("prove", DictIn, DictOut) :-
  % Extract important infomation of the DictIn by using the predefined predicate get_dict/3
  get_dict(request, DictIn, RequestObject),
  get_dict(intent, RequestObject, IntentObject),
  get_dict(slots, IntentObject, SlotsObject),
  get_dict(proveslot, SlotsObject, MySlotObject),
  get_dict(value, MySlotObject, ValueMaybeDot),
  % Add a dot to a sentence, if there is no dot available in the inserted sentence; this is required for RACE
  maybe_dot(ValueMaybeDot,Value),
  % debug message
  debug(alexa, 'prove intent with value: ~w~n', [Value]),
  % Get the whole knowledge which is stored in the database
  get_whole_database(WholeDatabase),
  % Solve if the inserted theorem can be proven with the available knowledge by using prove_with_answers/3 of the library(race)
  prove_with_answers(WholeDatabase, Value, Result),
  % Check if the received result contains the term 'results'
  ( Result = results([ResultsString|_]) ->
    % if there are results print the first derivation
    debug(alexa, 'Yes, the theorem is right and could be proven with: ~w~n', [ResultsString]),
    % Concat the answer
    string_concat('Yes, the theorem is right and could be proven with: ', ResultsString, AnswerProof)
  ; otherwise ->
    % if the theorem could not be proven
    % Concat the answer
    AnswerProof = 'The theorem could not be proven. Try again.'
  ),
  % Generate DictOut
  my_json_answer(AnswerProof, DictOut).

% If the IntentName does not match with one of the defined below, generate a error message for the reply
intent_dictOut(_,_,DictOut):-
my_json_answer('Error parsing',DictOut).


%%%%%%
% auxiliary predicates
%%%%%%

% Generates a JSON answer with the required structure
my_json_answer(Message, JSON) :-
  JSON = _{
    response: _{
      shouldEndSession: false,
      outputSpeech: _{
        type: "PlainText",
        text: Message
      }
    },
    version:"1.0"
  }.

% Checks if MaybeDot has a dot and add one, if there is no dot. Return a sentence with dot
maybe_dot(MaybeDot, SureDot) :-
  ( atom_concat(_, '.', MaybeDot) ->
    MaybeDot = SureDot
  ; atom_concat(MaybeDot, '.', SureDot)
  ).

% Combine the facts from the database with new knowledge stored in the variable 'Value'
combine_sentences(Value, CombinedSentences) :-
  findall(Z, knowledge(Z), Sentences),
  flatten(Sentences, SentenceList),
  atomics_to_string(SentenceList, ' ', SentencesString),
  atom_concat(SentencesString, Value, CombinedSentences).

% Remove the term 'fact' from the String
remove_fact(WithFact, WithoutFact) :-
  WithFact = fact(WithoutFact).

% Checks if MaybeWith has a questionmark and add one, if there is no one. Return a sentence with a questionmark
maybe_questionmark(MaybeWith, SureWith) :-
  ( string_concat(_, '?', MaybeWith) ->
    MaybeWith = SureWith
  ; string_concat(MaybeWith, '?', SureWith)
  ).

% Extract the whole database
get_whole_database(WholeDatabase) :-
  findall(Z, knowledge(Z), Sentences),
  flatten(Sentences, SentenceList),
  atomics_to_string(SentenceList, ' ', WholeDatabase).

% Return all the stored knowledge
answersAll(X) :-
  findall(Z, knowledge(Z), Sentences),
  maplist(addDotsToFacts, Sentences, SentencesWithDot),
  flatten(SentencesWithDot, Xy),
  atomics_to_string(Xy, ' ', X).

addDotsToFacts(Sentence, SentenceWithDot):-
  append(Sentence, [.], SentenceWithDot).
