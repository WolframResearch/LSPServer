
import os
import sys
import re
import subprocess
import argparse

def main():
	"""Wrap around a WolframKernel process and convert LSP traffic to a format that WolframKernel can handle.
	
	WolframKernel is used as a LSP Server, but WolframKernel cannot read arbitrary data from stdin.
	bug 11114
	so this thin Python script is used to marshall stdio data between an LSP client and the WolframKernel,
	which is an LSP server.

	The LSP client starts this Python script with the appropriate arguments.

	The process tree looks like this:
	LSP Client (Sublime Text, VS Code, Atom, etc.)
	  |
	  Python interpreter running this script
	    |
	    WolframKernel

	NOTE: Currently 0 or 1 lines are expected from the kernel for each loop. More than 1 line are not handled currently.
	For example, sending more than 1 notification at a time is not currently supported.

	All data is assumed to be UTF-8.
	"""

	parser = argparse.ArgumentParser()
	parser.add_argument("wolframkernel", help="path to WolframKernel")
	parser.add_argument("--debug", help="turn on debugging", action="store_true")
	parser.add_argument("--logDir", help="directory for log files", type=str)
	args = parser.parse_args()

	wolframkernel = args.wolframkernel
	debug = args.debug

	if debug:
		logDir = args.logDir
		if not logDir:
			raise FileNotFoundError
		if not os.path.isdir(logDir):
			raise FileNotFoundError
		logFileName = os.path.join(logDir, "logFile.txt")

		logFile = open(logFileName, 'w')

		kernelLogFile = os.path.join(logDir, "kernelLogFile.txt")
	else:
		kernelLogFile = ""


	kernelProc = subprocess.Popen(
	    [wolframkernel,
	    		# -noinit to speed-up starting
	    		'-noinit',
	    		# -noprompt to prevent the standard banner and
	    		# In[] / Out[] prompts interfering with protocol
	    		'-noprompt',
	    		# -rawterm is needed to enable $PreRead
	    		# bug 337831
	    		'-rawterm',
	    		# Assume LSPServer paclet is already installed
	    		'-run', f'(Needs["LSPServer`"];LSPServer`StartServer[{debug}, "{kernelLogFile}"])'],
	    stdin=subprocess.PIPE,
	    stdout=subprocess.PIPE,
	    stderr=subprocess.PIPE,
	)

	# Start the stdio loop with parent client and child kernel
	while True:

		header = sys.stdin.readline()
		m = re.search('Content-Length: (\d+)', header);
		if not m:
			break
		contentLength = int(m.group(1))

		# read the \r\n
		sys.stdin.read(2)

		contentString = sys.stdin.read(contentLength)
		contentString += '\n'

		if debug:
			logFile.write("P-->  " + contentString + "\n")

		contentBytes = contentString.encode('utf-8')
		kernelProc.stdin.write(contentBytes)
		# always remember to flush!
		kernelProc.stdin.flush()
		contentBytes = kernelProc.stdout.readline()

		# Did the kernel die?
		if kernelProc.poll():
			break

		#
		# contentBytes is b'' or b'\x0a'
		#
		# Null response from kernel
		#
		# Something like a notification was sent to the kernel, and we do not need to send a response back to the client
		#
		if len(contentBytes) <= 1:
			if debug:
				logFile.write("P<--  null\n")
			continue
		
		contentString = contentBytes.decode('utf-8')

		if debug:
			logFile.write("P<--  " + contentString + "\n")

		sys.stdout.write("Content-Length: " + str(len(contentString)) + "\r\n")
		sys.stdout.write("\r\n")
		sys.stdout.write(contentString)
		# always remember to flush!
		sys.stdout.flush()

	# Make sure that child kernel is killed before exiting
	kernelProc.kill()
	kernelProc.wait()
	if debug:
		logFile.write('kernel exit code: ' + str(kernelProc.returncode) + '\n')



if __name__== "__main__":
	main()

