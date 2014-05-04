%   bctree, concurrent b-tree:ish search, bucket test module
% 
%    Copyright (C) 2014 Peter Bartha <peitur@gmail.com>
%
%    This program is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module( bukcet_test ).

-export([
		add/0,
		search/0,
		clear/0
	]).


add() -> 
	ok.

search( ) -> 
	ok-

clear() -> 
	ok.	

load_file( Filenmae ) ->
	case file:open( Filenmae, [read] ) of
		{ ok, Handle } -> read_file( Handle, [] );
		{error, Reason} -> {error, Reason}
	end.

read_file( Handle, Lines ) ->
	case file:read_line( Handle ) of
		eof -> lists:reverse( Lines );
		{ok, Line0} -> 
			Line = string:strip( Line0, both, $\n ), 
			read_file( Handle, [Line|Lines] );
		{error, Reason} -> {error, Reason}
	end.