#!/usr/bin/env python

import sys
import os
import re

TRACE_SUBSYS_NET = 0x6000
valid_cores = ["2 ","3 "]
core_map = {2:"NIC", 3:"APP"}
event_map_actual = {
0X0001: "TRACE_EVENT_NET_START",
0X0002: "TRACE_EVENT_NET_STOP",
0X0003: "TRACE_EVENT_NET_NI_A",
0X0004: "TRACE_EVENT_NET_NI_P",
0X0005: "TRACE_EVENT_NET_NI_S",
0X0006: "TRACE_EVENT_NET_AI_A",
0X0007: "TRACE_EVENT_NET_AI_P",
0X0008: "TRACE_EVENT_NET_AO_C",
0X0009: "TRACE_EVENT_NET_AO_Q",
0X000A: "TRACE_EVENT_NET_AO_S",
0X000B: "TRACE_EVENT_NET_NO_A",
0X000C: "TRACE_EVENT_NET_NO_S",
0x000D: "TRACE_EVENT_NET_AOR_S",
0x000E: "TRACE_EVENT_NET_AIR_R" 
}

event_map = {
0X0001: "NET_START",
0X0002: "NET_STOP",
0X0003: "New packet came",
0X0004: "pkt processed",
0X0005: "pkt uploaded",
0X0006: "pkt entered app",
0X0007: "pkt processed app",
0X0008: "reply prepared app",
0X0009: "reply queued app",
0X000A: "reply sent app",
0X000B: "reply into NIC",
0X000C: "reply sent by NIC",
0x000D: "app done with pbuf",
0x000E: "app recved TX_done",
0x0010: "interrupt came", 
0x0011: "ARP packet incoming",
0x0012: "physical interrupt came" 
}
packet_boundries = [0X0012]

MAX_EVENT_NOREPLY = 8

def cycles_to_time (cycles):
	return ( cycles / 2800.0) / 1000 # miliseconds

def print_event(event):
	print "%-15f %-12f %-25s %-4s %-15x" % (cycles_to_time(event['TS']), 
		cycles_to_time(event['DIFF']), event_map[event['EVENT']],
	 	core_map[event['CID']], event['INFO'])
	#print event['DIFF_S'], event_map[event['EVENT']],\
	#core_map[event['CID']], event['INFO']

def show_event_list(event_list):
	for e in event_list:
		print_event(e)

def extract_events(in_f):
	splitter = re.compile(r'[\ ]')
	event_list = []
	line_count = 0
	for line in in_f:
		if line[0:2] not in valid_cores :
			continue
		if line[0] == '#' :
			continue
		tokens = splitter.split(line.strip())
		if len(tokens) != 3 :
			print "Error: Cant process line " + line
			continue
		c_event = {}
	#	print "Processing line " + str(tokens)
		c_event['CID'] = int(tokens[0])
		c_event['TS'] = int(tokens[1])
		c_event['SYS'] = int(tokens[2][0:4],16)
		c_event['EVENT'] = int(tokens[2][4:8],16)
		c_event['INFO'] = int(tokens[2][8:],16)


		# Lets ignore non-networking events 
		if  c_event['SYS'] != TRACE_SUBSYS_NET :
			print "Non network event, ignoring " + line
			continue

		# Lets ignore start and stop events
		if c_event['EVENT'] in [0x1, 0x2]:
			continue

		if(len(event_list) == 0):
			c_event['DIFF_S'] = 0
		else:
			prev_c_event = event_list[-1]
			c_event['DIFF_S'] = c_event['TS'] - prev_c_event['TS']
		# For time being making DIFF = DIFF_S to show interesting prints
		c_event['DIFF'] = c_event['DIFF_S']
		c_event['DIFF'] = 0
		event_list.append(c_event)
	return event_list


def diff_events(event_list):
	# now sort the event list based on timestamp
	sorted_events = sorted(event_list, key=lambda dd: dd['TS'])

	packet_list = []
	i = 0
	for e in sorted_events:
		if i == 0:
			e['DIFF'] = 0
		else:
			e['DIFF'] = e['TS'] - sorted_events[i-1]['TS']
		i = i + 1
	return sorted_events

def print_packet(pkt):
	print ""
	print "######################"
	print "Time after last packet %f" % (
		cycles_to_time(pkt['PDIFF']))
	for e in pkt['EL']:
		print_event(e)
	print "Packet Lifetime = %f, no. of events = %d" % (
	cycles_to_time(pkt['STOP'] - pkt['START']), len(pkt['EL']))



def show_packet_list(plist, only_answered=False):
	for pkt in plist:
		if (only_answered):
			if(len(pkt['EL'])<= MAX_EVENT_NOREPLY):
				continue
		print_packet(pkt)

def create_empty_packet(starting_event, last_stop):
	starting_event['DIFF'] = 0
	pkt = {'EL':		[starting_event],
		'START':	starting_event['TS'],
		}
	if last_stop == 0:
		pkt['PDIFF'] = 0
	else:
		pkt['PDIFF'] = pkt['START'] - last_stop

	return pkt

def group_events(event_list):
	packet_list = []
	i = 0
	for e in event_list:
		is_appended = False
		if i == 0:
			packet_list.append(create_empty_packet(e, 0))
			is_appended = True
		else:
			if e['EVENT'] in packet_boundries :
				packet_list.append(create_empty_packet(e, 
				packet_list[-1]['STOP']))
				is_appended = True

		packet_list[-1]['STOP'] = e['TS']
		e['PDIFF'] = e['TS'] - packet_list[-1]['EL'][-1]['TS']
		if not is_appended :
			packet_list[-1]['EL'].append(e)
		i = i + 1

	return packet_list 

def process_trace(in_f):
	elist = diff_events(extract_events(in_f))
#	show_event_list(elist)
	plist = group_events(elist)
#	show_packet_list(plist)
	show_packet_list(plist, True)

def show_usage():
	print "Usage: " + sys.argv[0] + " <traceFile> <outputFile>"

	
def main():
	if len(sys.argv) != 3:
		show_usage()
		sys.exit(1)
	inputFile = open(sys.argv[1], 'r')
	#outputFile = open(sys.argv[2], 'w')
	process_trace(inputFile)

if __name__ == '__main__' :
	main()
