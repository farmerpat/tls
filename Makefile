all: tls.scm
	#csc tls.scm
	csc -d3 -c -j tls tls.scm
