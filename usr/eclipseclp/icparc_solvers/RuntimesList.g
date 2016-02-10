
### Ian Gent March 30, 2004

## RuntimesList() returns a five element list
    
# In Gap4.4 the four elements are the times as returned by Runtimes
#       and the fifth element is 1 to indicate that the runtimes are real
# In Gap4,3, the first element is Runtime, second to fourth are 0,
#       and the fifth element is 0 indicate the zero times 
# The first element (user time) is the same in each case

#
# Copyright (C) 2004  The SBDS Group
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#


RuntimesAvailable := true;

if not(IsBound(Runtimes)) then Runtimes := false; 
                               RuntimesAvailable := false; fi;

RuntimesList := function() 
       local times; 
       if RuntimesAvailable
       then     times := Runtimes();
                return [times.user_time,times.system_time, 
                        times.user_time_children, times.system_time_children,1];
       else
                return [Runtime(),0,0,0,0];
       fi;
end;

