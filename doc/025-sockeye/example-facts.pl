node_overlay(["RAMOUT", [1, 10], "RAM", "root"], ["LOCAL", "root"]).
node_overlay(["RAMOUT", [1, 11], "RAM", "root"], ["LOCAL", "root"]).
node_overlay(["RAMOUT", [2, 10], "RAM", "root"], ["LOCAL", "root"]).
node_overlay(["RAMOUT", [2, 11], "RAM", "root"], ["LOCAL", "root"]).
node_overlay(["RAMOUT", [10, 10], "RAM", "root"], ["LOCAL", "root"]).
node_overlay(["RAMOUT", [10, 11], "RAM", "root"], ["LOCAL", "root"]).
node_overlay(["RAMOUT", [11, 10], "RAM", "root"], ["LOCAL", "root"]).
node_overlay(["RAMOUT", [11, 11], "RAM", "root"], ["LOCAL", "root"]).

node_accept(["LOCAL", "root"], [block(0, 4276092927, _142)]).
node_accept(["GDDR0", [1, 10], "RAM", "root"], [block(0, 4276092927, _152)]).
node_accept(["GDDR0", [1, 11], "RAM", "root"], [block(0, 4276092927, _152)]).
node_accept(["GDDR0", [2, 10], "RAM", "root"], [block(0, 4276092927, _152)]).
node_accept(["GDDR0", [2, 11], "RAM", "root"], [block(0, 4276092927, _152)]).
node_accept(["GDDR0", [10, 10], "RAM", "root"], [block(0, 4276092927, _152)]).
node_accept(["GDDR0", [10, 11], "RAM", "root"], [block(0, 4276092927, _152)]).
node_accept(["GDDR0", [11, 10], "RAM", "root"], [block(0, 4276092927, _152)]).
node_accept(["GDDR0", [11, 11], "RAM", "root"], [block(0, 4276092927, _151)]).

node_translate(["DRAMMAP", [1, 10], "RAM", "root"], [block(0, 4276092927, _182)], ["RAMOUT", [1, 10], "RAM", "root"], [block(0, 4276092927, _192)]).
node_translate(["LOCAL_SRC", "root"], [block(4096, 8192, _172)], ["GDDR0", [1, 10], "RAM", "root"], [block(4096, 8192, _192)]).
node_translate(["DRAMMAP", [1, 11], "RAM", "root"], [block(0, 4276092927, _182)], ["RAMOUT", [1, 11], "RAM", "root"], [block(0, 4276092927, _192)]).
node_translate(["LOCAL_SRC", "root"], [block(4096, 8192, _172)], ["GDDR0", [1, 11], "RAM", "root"], [block(4096, 8192, _192)]).
node_translate(["DRAMMAP", [2, 10], "RAM", "root"], [block(0, 4276092927, _182)], ["RAMOUT", [2, 10], "RAM", "root"], [block(0, 4276092927, _192)]).
node_translate(["LOCAL_SRC", "root"], [block(4096, 8192, _172)], ["GDDR0", [2, 10], "RAM", "root"], [block(4096, 8192, _192)]).
node_translate(["DRAMMAP", [2, 11], "RAM", "root"], [block(0, 4276092927, _182)], ["RAMOUT", [2, 11], "RAM", "root"], [block(0, 4276092927, _192)]).
node_translate(["LOCAL_SRC", "root"], [block(4096, 8192, _172)], ["GDDR0", [2, 11], "RAM", "root"], [block(4096, 8192, _192)]).
node_translate(["DRAMMAP", [10, 10], "RAM", "root"], [block(0, 4276092927, _182)], ["RAMOUT", [10, 10], "RAM", "root"], [block(0, 4276092927, _192)]).
node_translate(["LOCAL_SRC", "root"], [block(4096, 8192, _172)], ["GDDR0", [10, 10], "RAM", "root"], [block(4096, 8192, _192)]).
node_translate(["DRAMMAP", [10, 11], "RAM", "root"], [block(0, 4276092927, _182)], ["RAMOUT", [10, 11], "RAM", "root"], [block(0, 4276092927, _192)]).
node_translate(["LOCAL_SRC", "root"], [block(4096, 8192, _172)], ["GDDR0", [10, 11], "RAM", "root"], [block(4096, 8192, _192)]).
node_translate(["DRAMMAP", [11, 10], "RAM", "root"], [block(0, 4276092927, _182)], ["RAMOUT", [11, 10], "RAM", "root"], [block(0, 4276092927, _192)]).
node_translate(["LOCAL_SRC", "root"], [block(4096, 8192, _172)], ["GDDR0", [11, 10], "RAM", "root"], [block(4096, 8192, _192)]).
node_translate(["DRAMMAP", [11, 11], "RAM", "root"], [block(0, 4276092927, _182)], ["RAMOUT", [11, 11], "RAM", "root"], [block(0, 4276092927, _192)]).
node_translate(["LOCAL_SRC", "root"], [block(4096, 8192, _171)], ["GDDR0", [11, 11], "RAM", "root"], [block(4096, 8192, _191)]).
