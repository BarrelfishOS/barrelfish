##########################################################################
# Copyright (c) 2012, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

import os, signal, tempfile, subprocess, shutil, time
import debug, machines
from machines import Machine

GEM5_CACHES_ENABLE = '--caches --l2cache'.split()

class Gem5MachineBase(Machine):
	def __init__(self, options):
		super(Gem5MachineBase, self).__init__(options)
		self.child = None
		self.tftp_dir = None
		self.options = options
        
	def get_coreids(self):
		return range(0, self.get_ncores())
        
	def get_tickrate(self):
		return None
        
	def get_boot_timeout(self):
		# we set this to 10 mins since gem5 is very slow
		return 600
        
	def get_tftp_dir(self):
		if self.tftp_dir is None:
			debug.verbose('creating temporary directory for Gem5 files')
			self.tftp_dir = tempfile.mkdtemp(prefix='harness_gem5_')
			debug.verbose('Gem5 install directory is %s' % self.tftp_dir)
		return self.tftp_dir
		
	def _write_menu_lst(self, data, path):
		debug.verbose('writing %s' % path)
		debug.debug(data)
		f = open(path, 'w')
		f.write(data)
		f.write("mmap map 0x0 0x20000000 1\n")
		f.close()
		
	def set_bootmodules(self, modules):
		pass
	
	def lock(self):
		pass

	def unlock(self):
		pass

	def setup(self):
		pass

	def _get_cmdline(self):
		raise NotImplementedError

	def _kill_child(self):
		# terminate child if running
		if self.child:
			os.kill(self.child.pid, signal.SIGTERM)
			self.child.wait()
			self.child = None
			
	def reboot(self):
		self._kill_child()
		cmd = self._get_cmdline()
		debug.verbose('starting "%s"' % ' '.join(cmd))
		devnull = open(os.devnull, 'r')
		self.child = subprocess.Popen(cmd, stdin=devnull, stderr=devnull)
		devnull.close()

	def shutdown(self):
		self._kill_child()
		# try to cleanup tftp tree if needed
		if self.tftp_dir and os.path.isdir(self.tftp_dir):
			shutil.rmtree(self.tftp_dir, ignore_errors=True)
		self.tftp_dir = None

	def get_output(self):
		# wait a bit to give gem5 time to listen for a telnet connection
		time.sleep(3)
		telnet_proc = subprocess.Popen(['telnet','127.0.0.1','3456'],stdout=subprocess.PIPE)
		return telnet_proc.stdout
	
class Gem5MachineARM(Gem5MachineBase):
	def get_bootarch(self):
		return 'arm_gem5'
	
	def set_bootmodules(self, modules):
		# store path to kernel for _get_cmdline to use
		tftp_dir = self.get_tftp_dir()
		self.kernel_img = os.path.join(tftp_dir, 'arm_gem5_harness_kernel')
		
		#write menu.lst
		path = os.path.join(self.get_tftp_dir(), 'menu.lst')
		self._write_menu_lst(modules.get_menu_data('/'), path)

@machines.add_machine
class Gem5MachineARMSingleCore(Gem5MachineARM):
	name = 'gem5_arm_1'
	
	def get_ncores(self):
		return 1
	
	def _get_cmdline(self):
		script_path = os.path.join(self.options.sourcedir, 'tools/arm_gem5', 'gem5script.py')
		return (['gem5.fast', script_path, '--kernel=%s'%self.kernel_img, '--n=%s'%self.get_ncores()]
				+ GEM5_CACHES_ENABLE)

@machines.add_machine
class Gem5MachineARMMultiCore(Gem5MachineARM):
	name = 'gem5_arm_2'
	
	def get_ncores(self):
		return 2
	
	def _get_cmdline(self):
		script_path = os.path.join(self.options.sourcedir, 'tools/arm_gem5', 'gem5script.py')
		return (['gem5.fast', script_path, '--kernel=%s'%self.kernel_img, '--n=%s'%self.get_ncores()]
				+ GEM5_CACHES_ENABLE)		
@machines.add_machine
class Gem5MachineARMMultiCore(Gem5MachineARM):
	name = 'gem5_arm_4'
	
	def get_ncores(self):
		return 4
	
	def _get_cmdline(self):
		script_path = os.path.join(self.options.sourcedir, 'tools/arm_gem5', 'gem5script.py')
		return (['gem5.fast', script_path, '--kernel=%s'%self.kernel_img, '--n=%s'%self.get_ncores()]
				+ GEM5_CACHES_ENABLE)
	
	
	
	
	
	
	
	
	
	
	