#include <stdio.h>

// Variable declaration:
extern int a, b;
extern int c, d;
extern float f;

// function declaration
int foo();

int main ()
{
  /* variable definition: */
  int a, b;
  int c, d;
  float f;
 
  /* actual initialization */
  a = 10;
  b = 20;
  
  c = a + b;
  printf("value of c : %d \n", c);
  
  d = foo();
  printf("value of g : %d \n", d);

  f = 70.0/3.0;
  printf("value of f : %f \n", f);
  
  return 0;
}

int foo()
{
  return 0;
}
