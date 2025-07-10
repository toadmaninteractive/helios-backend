-module(web_types).

%% Exported functions

-export([
    id/1,
    datetime_to_text/1,
    datetime_from_text/1
]).

%% API

id(T) -> T.

datetime_to_text(DateTime) ->
    iso8601:format_datetime_zone(DateTime, utc).

datetime_from_text(Json) when is_binary(Json) ->
    iso8601:parse_datetimems(Json).

%% Local functions
