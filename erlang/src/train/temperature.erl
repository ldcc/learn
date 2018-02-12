-module(temperature).
-export([format_temps/1]).

format_temps([]) -> ok;
format_temps([City | Other]) -> 
    print_temp(convert_temp(City)),
    format_temps(Other).

convert_temp({Name, {f, Temp}}) -> 
    {Name, {c, 5 / 9 * (Temp - 32)}};
convert_temp({Name, {c, Temp}}) -> 
    {Name, {f, Temp}};
convert_temp(_) -> "no such temperature".

print_temp({Name, {_, Temp}}) ->
    io:format("~-15w~w c~n", [Name, Temp]).