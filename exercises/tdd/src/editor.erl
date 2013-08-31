-module(editor).
-export([edit/2, edits/2]).

-type command() :: delete | skip | {add, char()} | {replace, char()} | {insert,char()}.

%% Applies a sequence of commands on the given string and returns the transformed string
-spec edit(From :: string(), [command()]) -> string().

edit([], _Commands) ->
  "";

edit(From, []) ->
  From;

edit([_DeletedChar|RestOfString], [delete|OtherCommands]) ->
  edit(RestOfString, OtherCommands);

edit([Skipped|RestOfString], [skip|OtherCommands]) ->
  [Skipped|edit(RestOfString, OtherCommands)];

edit([FirstChar|RestOfString], [{add,NewChar}|OtherCommands]) ->
  edit([FirstChar|[NewChar|RestOfString]], OtherCommands);

edit([_CharToReplace|RestOfString], [{replace,NewChar}|OtherCommands]) ->
  edit([NewChar|RestOfString], OtherCommands);

edit(From, [{insert,NewChar}|OtherCommands]) ->
  edit([NewChar|From], OtherCommands).

%% Returns the sequence of commands required to transform one string into another
-spec edits(From :: string(), To :: string()) -> [command()].

edits(From, []) ->
  [delete || _ <- From];

edits([], CharsLeftToInsert) ->
  [{insert, NewChar} || NewChar <- CharsLeftToInsert];

edits([SameChar|RestFrom], [SameChar|RestTo]) ->
  [skip|edits(RestFrom, RestTo)];

edits([_FromChar|RestFrom], [ToChar|RestTo]) ->
  [{replace, ToChar}|edits(RestFrom, RestTo)].