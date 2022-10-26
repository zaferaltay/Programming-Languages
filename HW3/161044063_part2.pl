% istanbul-x
flight(istanbul,izmir,2).
flight(istanbul,ankara,1).
flight(istanbul,rize,4).

%ankara-x
flight(ankara,izmir,6).
flight(ankara,van,4).
flight(ankara,rize,5).
flight(ankara,diyarbakýr,8).
flight(ankara,istanbul,1).

%rize-x
flight(rize,ankara,5).
flight(rize,istanbul,4).

%van-x
flight(van,gaziantep,3).
flight(van,ankara,4).

%gaziantep-x
flight(gaziantep,van,3).

%diyarbakýr-x
flight(diyarbakýr,antalya,4).
flight(diyarbakýr,ankara,8).

%izmir-x
flight(izmir,antalya,2).
flight(izmir,ankara,6).
flight(izmir,istanbul,2).

%antalya-x
flight(antalya,erzincan,3).
flight(antalya,izmir,2).
flight(antalya,diyarbakýr,4).

%erzincan -x
flight(erzincan,canakkale,6).
flight(erzincan,antalya,3).

%canakkale-x
flight(canakkale,erzincan,6).



% a predicate indicating there exist a route between
% X and Y if there is flight between X and Y.

route(Start,Destination,X) :- flight(Start, Destination,X).
route(Start,Destination,X) :- flight(Start, Y, A),flight(Y,Destination, B), X is A+B.
