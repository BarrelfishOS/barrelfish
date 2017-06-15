net('device1',node(device,[block(16'0,16'fff)],[],'@none')).
net('device2',node(device,[block(16'0,16'fff)],[],'@none')).
net('RAM',node(memory,[block(16'0,16'ffff)],[],'@none')).
net('Interconnect',node(other,[],[map(block(16'0,16'fff),'device1',16'0),map(block(16'2000,16'2fff),'device2',16'0),map(block(16'10000,16'1ffff),'RAM',16'0)],'@none')).
net('CPU',node(other,[],[],'Interconnect')).
