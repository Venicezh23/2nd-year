#include <iostream>

using namespace std;

int gcd(int a, int b) {
    if (b == 0)
        return a;
    return gcd(b, a % b);
}

/*
int main(){
	int a = 98, b = 56;
	cout << "GCD of " << a << " and " << b << " = " << gcd(a,b);
	return 0;
}*/