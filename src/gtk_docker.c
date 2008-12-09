#include <gnome.h>

void do_gnome_init()
{
  static char **argv = NULL;
  if (argv == NULL)
  {
    argv = malloc(2);
    argv[0] = "gtk_docker";
    argv[1] = '\0';
  }
  gnome_init("my-app", "my-version", 1, argv);
}

