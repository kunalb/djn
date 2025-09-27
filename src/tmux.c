#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>


void open_popup() {
  int res = execlp("tmux", "display-popup", "-x5%", "-y5%", "-w90%", "-h40%", (char*) NULL);
  if (res != 0) {
    printf("Failed to launch tmux!\n%s\n", strerror(errno));
  }
}


int main(int argc, char **argv) {
  open_popup();
  return EXIT_SUCCESS;
}
