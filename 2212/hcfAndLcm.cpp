#include <iostream>

using namespace std;

int findHCF (int a, int b){
	if (b == 0)
        return a;
    return findHCF(b, a % b);
}

int findLCM (int a, int b){
	return (a / findHCF(a,b)*b);
}
/*
int main(){
	int num1, num2;
	cout << "Enter first number: ";
	cin >> num1;
	cout << "Enter second number: ";
	cin >> num2;
	cout << "HCF = " << findHCF (num1, num2) << "; LCM = " << findLCM (num1, num2) << endl;
	return 0;
}*/