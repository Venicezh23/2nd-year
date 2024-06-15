#include <iostream>
using namespace std;

namespace arrayOperations {
	//a. Initialize the 10 elements of integer array counts to zeros.
	void initZeroes(int (&arr)[10]){
		for (int i = 0; i < 10; ++i){
			arr[i] = 0;
		}
	}
	//b) Add 1 to each of the 15 elements of integer array bonus.
	void addOnesPlus(int (&arr)[15]){
		for(int i = 0; i < 15; ++i){
			++arr[i]; //or arr[i] += 1
		}
	}
	//c) Read the 12 values of floating-point array monthlyTemperatures from the keyboard.
	void getTempValues(float (&arr)[12]){
		for (int i = 0; i < 12; ++i){
			cout << "Enter month " << i+1 << ": " << endl;
			cin >> arr[i];
		}
	}
	//d) Print the five values of integer array bestScores in column format.
	void printColFormat(int (&arr)[5]){
		for (int i = 0; i < 5; ++i){
			cout << arr[i] << endl;
		}
	}
}

/*
int main(){
	int arr[10] = {0};
	int bonus[15] = {0};
	float monthlyTemp[12] = {0};
	int bestScores[5] = {3,5,12,50,23};
	
	//arrayOperations::initZeroes(arr);
	
	arrayOperations::getTempValues(monthlyTemp);
	
	//arrayOperations::printColFormat(bestScores);
	return 0;
}*/