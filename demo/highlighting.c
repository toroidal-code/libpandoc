#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <pandoc.h>

static FILE* in_file;

int reader(char *buf, int buffer_len, void* user_data) {
  // We're alread at the end of a file
  if (feof(in_file)) {
    // perform file cleanup here
    buf[0] = '\0'; // Set the null term so that it doesn't re-read the buffer.
    return 0;
  }

  // Otherwise, we can do things:  
  char c;
  int chars_left = buffer_len;
  while (--chars_left != 0 && !feof(in_file)
         && (c = fgetc(in_file))) {
    *buf++ = c;
  }
  *buf = '\0'; // Always ensure null-termination
  return buffer_len - chars_left;
}

/* A fake out buffer. This could be a FILE* or whatever */
static FILE* out_buffer = NULL;

void writer(const char *buf, int buffer_len, void* user_data){
  if (out_buffer == NULL) {  /* Do your opening */
    out_buffer = stdout;
  }
  if (buf == NULL || buffer_len == 0) {  /* Close the File Descriptor */
    out_buffer = NULL;
  } else {
    fprintf(out_buffer,"%.*s",buffer_len,buf);  /* Be careful about the string length! */
  }
}


int main(int argc, char *argv[]) {
  in_file = stdin;
  pandoc_init();
  char lang[21]; // There aren't any programming language names over 20 chars long
  while(true) {
    printf ("Enter a language: ");
    scanf ("%20s", lang);
    if (valid_language(lang)) {
      break;
    } else {
      printf("Error! %s is not available for highlighting. Please try a different language\n", lang);
    }
  }
  printf("Enter some code. Press Ctrl-D to exit.\n");
  highlight(4, lang, "html", true, reader, writer, NULL);
  pandoc_exit();
  return 0;
}

