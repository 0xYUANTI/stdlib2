-ifndef(__S2_TIME_HRL).
-define(__S2_TIME_HRL, true).

%% Express time units in seconds
-define(MINUTES(X), (trunc(X * 60))).
-define(HOURS(X),   (trunc(X * 60 * 60))).
-define(DAYS(X),    (trunc(X * 60 * 60 * 24))).
-define(WEEKS(X),   (trunc(X * 60 * 60 * 24 * 7))).
-define(YEARS(X),   (trunc(X * 60 * 60 * 24 * 365.25))). % Julian year

%% Daynums
-define(MONDAY,    1).
-define(TUESDAY,   2).
-define(WEDNESDAY, 3).
-define(THURSDAY,  4).
-define(FRIDAY,    5).
-define(SATURDAY,  6).
-define(SUNDAY,    7).

-endif.
