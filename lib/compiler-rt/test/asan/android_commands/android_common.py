import os, subprocess, tempfile
import time

ANDROID_TMPDIR = '/data/local/tmp/Output'
ADB = os.environ.get('ADB', 'adb')

verbose = False
if os.environ.get('ANDROID_RUN_VERBOSE') == '1':
    verbose = True

def adb(args):
    if verbose:
        print args
    devnull = open(os.devnull, 'w')
    return subprocess.call([ADB] + args, stdout=devnull, stderr=subprocess.STDOUT)

def pull_from_device(path):
    tmp = tempfile.mktemp()
    adb(['pull', path, tmp])
    text = open(tmp, 'r').read()
    os.unlink(tmp)
    return text

def push_to_device(path):
    # Workaround for https://code.google.com/p/android/issues/detail?id=65857
    dst_path = os.path.join(ANDROID_TMPDIR, os.path.basename(path))
    tmp_path = dst_path + '.push'
    adb(['push', path, tmp_path])
    adb(['shell', 'cp "%s" "%s" 2>&1' % (tmp_path, dst_path)])
