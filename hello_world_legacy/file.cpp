#include <stdio.h>

int some_function(int input)
{
  return input * 2;
}

int main()
{

  int x = 4;
  auto fun  = [=](int a){return a+x;};
  int answer = some_function(21);
  int d=fun(145);
  printf("Answer is %d\n", answer);
  return 0;
}