all:
	gcc -shared -fPIC TkDND_XDND.c \
        -DTKDND_PACKAGE="\"tkdnd\"" -DTKDND_VERSION="\"2.0\"" \
        -DUSE_TCL_STUBS -DUSE_TK_STUBS -DHAVE_UNISTD_H -DHAVE_LIMITS_H \
        -I /usr/include -I /usr/include/tk-private/generic \
        -I /usr/include/tcl-private/generic \
        -L /usr/lib -ltclstub8.4 -ltkstub8.4 -o ../libtkdnd20.so

swig:
	/usr/bin/swig -tcl -module tkdnd TkDND_X.h
	gcc -shared -fPIC TkDND_X_wrap.c -D USE_TCL_STUBS -D USE_TK_STUBS -I \
        /usr/include -L/usr/lib -ltclstub8.4 -ltkstub8.4 -o libtkdnd.so

