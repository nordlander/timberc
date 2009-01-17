module BeforeConstraint where

import POSIX

root env = class
	a = action
		env.stdout.write "Hello\n"
		after (sec 1) a
	result action
		after (sec 2) before (sec 1) a
