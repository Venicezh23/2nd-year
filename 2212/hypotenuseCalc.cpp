#include <iostream>
#include <cmath>

using namespace std;

double hypotenuse(double a, double b){
	double c = sqrt((a*a)+(b*b));
	return c;
}

/*
int main(){
	double a, b, c;
	cout << "Enter first angle: " << endl;
	cin >> a;
	cout << "Enter second angle: " << endl;
	cin >> b;
	c = hypotenuse(a, b);
	cout << "Hypotenuse of " << a << " and " << b << " = " << c << endl;
	return 0;
}*/