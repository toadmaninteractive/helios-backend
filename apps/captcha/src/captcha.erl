-module(captcha).

%% Exported functions

-export([
    generate/1
]).

%% API

-spec generate(Dir :: file:filename_all()) ->
    {'ok', Answer :: binary(), FileName :: binary()} | {'error', Reason :: atom()}.

generate(Dir) ->
    % Create answer
    <<Bytes:128>> = uuid:get_v4(strong),
    BinStr = iolist_to_binary(io_lib:format("~.36.0B", [Bytes])),
    Answer = binary:part(BinStr, 0, 6),

    % Generate unique filename prefix
    <<I1:128>> = uuid:get_v4(strong),
    <<I2:128>> = uuid:get_v4(strong),
    Prefix = iolist_to_binary(io_lib:format("~.36.0B~.36.0B", [I1, I2])),

    % Define extension, file name and related paths
    Ext = <<".png">>,
    FileName = <<Prefix/binary, Ext/binary>>,
    FilePathBG = filename:join([Dir, <<Prefix/binary, "_bg", Ext/binary>>]),
    FilePathText = filename:join([Dir, <<Prefix/binary, "_text", Ext/binary>>]),
    FilePathResult = filename:join([Dir, FileName]),

    % Define geometry
    Width = 220,
    Height = 60,

    try
        % Generate CAPTCHA background
        ok = gm:convert("", FilePathBG, [], [
            {raise, 40 + rand:uniform(50), 3},      % {40,90}x3
            {'+raise', 40 + rand:uniform(50), 3},   % {40,90}x3
            {size, Width, Height},                  % 220x60
            {pattern, random_pattern()},
            {wave, 5, 25 + rand:uniform(35)},       % 5x{25,60}
            {swirl, rand:uniform(200) - 100}        % {-100,100}
        ]),

        % Generate CAPTCHA text
        ok = gm:convert("", FilePathText, [], [
            {size, Width, Height},                  % 220x60
            {fill, "#777777"},                      % was: #888888
            {implode, 0.1},
            {median, 1},
            {spread, 2},
            {sharpen, 2},
            {gravity, "Center"},
            {wave, 4, 30 + rand:uniform(50)},       % 5x{30,80}
            {swirl, rand:uniform(20) - 10},         % {-10,10}
            {font, captcha_config:font()},
            {pointsize, 72},
            {label, binary_to_list(Answer)}
        ]),

        % Compose CAPTCHA
        ok = gm:composite(FilePathText, FilePathBG, FilePathResult, [
            {size, Width, Height},                  % 220x60
            {compose, "multiply"}
        ]),

        % Delete intermediate files
        file:delete(FilePathBG),
        file:delete(FilePathText),

        {ok, Answer, FileName}
    catch Type:What:StackTrace ->
        logger:error("CAPTCHA generation failed (reason: ~p:~p)", [Type, What], #{caption => ?MODULE, stacktrace => StackTrace}),
        file:delete(FilePathBG),
        file:delete(FilePathText),
        file:delete(FilePathResult),
        {error, failure}
    end.

%% Local functions

patterns() ->
    [
        "BRICKS",
        "CIRCLES",
        "FISHSCALES",
        "HEXAGONS",
        "HORIZONTALSAW",
        "HS_BDIAGONAL",
        "HS_CROSS",
        "LEFTSHINGLE",
        "VERTICALBRICKS",
        "VERTICALRIGHTSHINGLE",
        "VERTICALSAW"
    ].

random_pattern() ->
    AllPatterns = patterns(),
    lists:nth(rand:uniform(length(AllPatterns)), AllPatterns).
