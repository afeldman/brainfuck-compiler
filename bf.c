#include <stdio.h>
#include <string.h>
#include <stdlib.h>

unsigned char *p;

void interpreter(char *);

int main(int argc, char *argv[])
{
  if (argc < 2)
  {
    printf("missing brainfuck command\n");
    exit(1);
  }

  p = malloc(sizeof(unsigned char) * 30000);
  char *code_ = malloc(sizeof(char) * strlen(argv[1]));
  strcpy(code_, argv[1]);

  interpreter(code_);

  return 0;
}

void interpreter(char *code)
{
  char current_token;
  unsigned int i = 0;
  unsigned int j = 0; //loop counter

  for (i = 0; code[i] != 0; ++i)
  {
    current_token = code[i];
    if (current_token == '>')
    { // pointer + 1
      ++p;
    }
    else if (current_token == '<')
    { //pointer - 1
      --p;
    }
    else if (current_token == '+')
    { //pointer value + 1
      ++*p;
    }
    else if (current_token == '-')
    { //pointer value - 1
      --*p;
    }
    else if (current_token == '.')
    { //print the pointer value
      putchar(*p);
    }
    else if (current_token == ',')
    { //read the char value into pointer
      *p = getchar();
    }
    else if (current_token == ']' && *p)
    { //loop end. Read the code until got empty
      j = 1;
      while (j > 0)
      {
        current_token = code[--i];
        if (current_token == '[')
        {
          --j;
        }
        else if (current_token == ']')
        {
          ++j;
        }
      }
    }
    else
    {
      continue;
    }
  }
}
