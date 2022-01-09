// gcc -o area area.c -lm

#include <stdio.h>
#include <math.h>

float rectangle_area(float l, float w) {
  return l * w;
}

float CircleArea(float radius){
  const float PI = 3.14159;
  return PI * radius * radius;
}

float TriangleArea( float side1, float side2, float side3){
  float s, area;
  s = (side1 + side2 + side3)/2.0;
  area = sqrtf(s * (s - side1)*(s-side2)*(s-side3));
  return area;
}

int main() {
  float l, w, r, a, b, c, area = 0;
  l = 5.4;
  w = 4.7;
  area = rectangle_area(l, w);
  printf("Area of Rectangle 5.4 x 4.7 is: %.3f\n", area);

  r = 7.0;
  area= CircleArea(r);
  printf("Area of Circle with radius 7.0 is: %.3f\n", area);

  a = 3.0;
  b= 4.0;
  c= 5.0;

  area= TriangleArea(a, b, c);
  printf("Area of Triangle 3.0 by 4.0 by 5.0 is: %.3f\n", area);

  return 0;
}
