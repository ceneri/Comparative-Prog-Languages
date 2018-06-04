% $Id: functions.pl,v 1.3 2016-11-08 15:04:13-08 - - $
%  Cesar Neri   
%  ceneri@ucsc.edu
%  1513805
%  functions.pl


% Not defined
not( X ) :- X, !, fail.
not( _ ).

% Predefined functions

mathfns( X, List ) :-
   S is sin( X ),
   C is cos( X ),
   Q is sqrt( X ),
   List = [S, C, Q].

constants( List ) :-
   Pi is pi,
   E is e,
   Epsilon is epsilon,
   List = [Pi, E, Epsilon].

sincos( X, Y ) :-
   Y is sin( X ) ** 2 + cos( X ) ** 2.

haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

% *******************New functions**************************

% Printing functions

write_time(Hour, Mins) :-
   Mins < 10,
   write(Hour), write(':0'), write(Mins).

write_time(Hour, Mins) :-
   Mins >= 10,
   write(Hour), write(':'), write(Mins).


% Code used to print airport code in uppercase 
char_up( [] ) :-
   write(' ').

char_up( [CAR | CDR] ) :-
  char_code(Char, CAR),
  lower_upper(Char, Upp),
  write(Upp), char_up(CDR).

write_uppercase(String) :-
  atom_codes(String, AsList),
  char_up(AsList).

% Given position in degrees and minutes, retuns only degrees
% (with decimal)
degmin_to_deg(Deg, Min, Degs) :-
   Degs is Deg + (Min / 60).

% Given position in degrees returns it in radians
degs_to_radian(Degs, Radians) :-
   Radians is Degs * pi / 180.

% Given position in degrees returns it in radians
get_radian_pos(City, Lat, Long) :- 
   airport( _, City, degmin(Deg1, Min1), degmin(Deg2, Min2) ),
   degmin_to_deg(Deg1, Min1, LatDegs),
   degmin_to_deg(Deg2, Min2, LonDegs),
   degs_to_radian(LatDegs, Lat),
   degs_to_radian(LonDegs, Long).

% Given two airports calculates the travcel distance between them
flight_distance(Airport1, Airport2, Distance) :-
   airport(Airport1, City1, _, _ ),
   airport(Airport2, City2, _, _ ),
   get_radian_pos(City1, Lat1, Long1),
   get_radian_pos(City2, Lat2, Long2),
   haversine_radians( Lat1, Long1, Lat2, Long2, Distance ).

% Given a distance, time needed to travel is calculated in hours
flight_time(Distance, Time) :-
   Time is Distance / 500.

% Given a time in MINUTES and HOURS, calculates new time when added
% HOURSDEC hours
new_time(HOld, MOld, HoursDec, HNew, MNew) :-
   TotalMins is round(HoursDec * 60),
   ExtraMins is TotalMins mod 60,
   ExtraHours is TotalMins // 60,
   MNew is (MOld + ExtraMins) mod 60,
   HNew is HOld + ExtraHours + ((MOld + ExtraMins) // 60 ).

% Handles test case where destination and origin are the same
fly(Air1, Air2) :-
   not(airport(Air1, _, _, _ )),  
   not(airport(Air2, _, _, _ )),  
   nl, write('Invalid origin and destination airport.').

% Handles test case where destination and origin are the same
fly(Air1, Air2) :-
   not(airport(Air1, _, _, _ )),  
   airport(Air2, _, _, _ ),  
   nl, write('Invalid origin airport.').

% Handles test case where destination and origin are the same
fly(Air1, Air2) :-  
   airport(Air1, _, _, _ ),
   not(airport(Air2, _, _, _ )),  
   nl, write('Invalid destination airport.').

% Flight exists directly
fly(Air1, Air2) :-
   flight(Air1, Air2, time(Hour, Mins) ),
   airport(Air1, City1, _, _ ),
   airport(Air2, City2, _, _ ),
   flight_distance(Air1, Air2, Dis),
   flight_time(Dis, Tim),
   new_time(Hour, Mins, Tim, HourNew, MinNew),
   HourNew < 24,
   nl, write('depart '),  write_uppercase(Air1), 
   write(City1), write_time(Hour, Mins), nl,
   write('arrive '),  write_uppercase(Air2), 
   write(City2), write_time(HourNew, MinNew). %This time needs to change

% Handles test case where destination and origin are the same
fly(Air1, Air1) :-
   airport(Air1, City1, _, _ ),
   nl, write('Already in '), write(City1).

% Flight does not exist directly (Otherwise traverse graph)**********
fly(Air1, Air2) :-
   not( flight(Air1, Air2, time(_, _) ) ),
   flight(Air1, SomeAir, time(Hour, Mins) ),
   airport(Air1, City1, _, _ ),
   airport(SomeAir, SomeCity, _, _ ),
   flight_distance(Air1, SomeAir, Dis),
   flight_time(Dis, Tim),
   new_time(Hour, Mins, Tim, HourNew, MinNew),
   new_time(HourNew, MinNew, 0.5, HourTrans, MinsTrans),
   flight(SomeAir, Air2, time(Hour2, Mins2) ),
   ( (Mins2 >= MinsTrans, Hour2 = HourTrans); (Hour2 > HourTrans) ),
   airport(Air2, City2, _, _ ),
   flight_distance(SomeAir, Air2, Dis2),
   flight_time(Dis2, Tim2),   
   new_time(Hour2, Mins2, Tim2, HNew2, MNew2),
   HNew2 < 24,
   nl, write('depart '),  write_uppercase(Air1), 
   write(City1), write_time(Hour, Mins), nl,
   write('arrive '),  write_uppercase(SomeAir), 
   write(SomeCity), write_time(HourNew, MinNew), nl,
   write('depart '),  write_uppercase(SomeAir), 
   write(SomeCity), write_time(Hour2, Mins2), nl,
   write('arrive '),  write_uppercase(Air2), 
   write(City2), write_time(HNew2, MNew2). %This time needs to change
