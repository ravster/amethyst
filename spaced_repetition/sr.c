/*
gcc -o sr sr.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

struct sr {
  char question[200];
  char answer[200];
  struct tm tm;
  int days;
};
int s_to_sr(struct sr* out, char* in) {
  char* line = in;
  line = strtok(line, "\t");
  strcpy(out->question, line);

  line = strtok(NULL, "\t");
  strcpy(out->answer, line);

  line = strtok(NULL, "\t");
  // Have to clear this memory to get rid of random bytes in here.  If you don't, those
  // random bytes will be interpreted, and will make your results weird.
  memset(&(out->tm), 0, sizeof(struct tm));
  strptime(line, "%Y-%m-%d", &(out->tm));

  line = strtok(NULL, "\t");
  out->days = atoi(line);

  return 1; // OK
}
int sr_to_s(char* out, struct sr* in) {
  char buf[255];
  strftime(buf, sizeof(buf), "%Y-%m-%d", &(in->tm));

  snprintf(out, 400, "%s\t%s\t%s\t%d",
	   in->question,
	   in->answer,
	   buf,
	   in->days);

  return 1; // OK
}

void question_user(struct sr* entry) {
  printf("\nQuestion: %s\n", entry->question);
  printf("\n\nPress ENTER when ready for an answer:");
  int a = getchar();
  printf("Answer: %s\n", entry->answer);

  printf("\nDid you get the right answer? (y/n):");
  a = getchar();
  getchar(); // To swallow up the ENTER that comes after the input above.  If this
	     // program grows, then quickly switch to using something proper, like fgets.
  if (a == 'y') {
    entry->days++;
  } else {
    if(entry->days > 1) {
      entry->days--;
    }
  }

  memset(&(entry->tm), 0, sizeof(struct tm));
  time_t now = time(NULL);
  entry->tm = *localtime(&now);
}

int main(int argc, char* argv[]) {
  if (argc != 3) {
    printf("Usage: ./sr INPUT.tsv OUTPUT.tsv\nINPUT and OUTPUT are allowed to be the same file.\n");
    exit(1);
  }
  char* filename = argv[1];

  FILE* f = fopen(filename, "r");
  char line[300];
  char line2[300];
  int is_first = 1;
  struct sr all_entries[100];
  int entry_count = 0;
  while(fgets(line, 299, f)) {
    if (is_first == 1) {
      is_first = 0;
      continue;
    }
    strcpy(line2, line);
    struct sr entry;
    int ok = 0;
    ok = s_to_sr(&entry, line2);
    time_t now = time(NULL);
    time_t entry_time = mktime(&(entry.tm));

    int should_show = (now - entry_time) > (entry.days * 24 * 60 * 60);
    if (should_show) {
      question_user(&entry);
    }

    // Append the entry to the output array.
    all_entries[entry_count] = entry;
    entry_count++;
  }
  fclose(f);

  // Output of the new "state" file.  This can't be sent out to STDOUT and redirected
  // using the shell, because our interface is through STDOUT too.  This is an
  // interactive program, and we can't batch this and use the shell builtins.
  char* outfile = argv[2];

  FILE* f2 = fopen(outfile, "w");
  int ok = 0;
  char print[400];
  fprintf(f2, "Question\tAnswer\tDate\tNumber of days\n");
  for(int i = 0; i < entry_count; i++) {
    struct sr entry = all_entries[i];
    ok = sr_to_s(print, &entry);
    fprintf(f2, "%s\n", print);
  }
  fclose(f2);
}
