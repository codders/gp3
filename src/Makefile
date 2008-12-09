LDFLAGS = -lgnomeui-2 -lcairo -lglade-2.0
CFLAGS = -I/usr/include/libgnomeui-2.0 -I/usr/include/gtk-2.0/ -I/usr/include/cairo/ -I/usr/include/glib-2.0/ -I/usr/lib/glib-2.0/include/ -I/usr/include/pango-1.0/ -I/usr/lib/gtk-2.0/include/ -I/usr/include/atk-1.0/ -I/usr/include/libgnome-2.0/ -I/usr/include/libbonobo-2.0/ -I/usr/include/libgnomecanvas-2.0/ -I/usr/include/libart-2.0/ -I/usr/include/libbonoboui-2.0/ -I/usr/include/gnome-vfs-2.0/ -Werror -Wall

GHCC=ghc

default: gp3

gp3: gtk_docker.o GP3Main.hs GP3GUI.hs
	$(GHCC) --make $(LDFLAGS) $^

clean:
	rm -f *.o *.hi GP3Main
