#include <iostream>
using namespace std;

namespace distanceConvert {
	float kmToMeter(float km){
		float result = km * 1000;
		return result;
	}
	float kmToFeet(float km){
		float result = km * 3280.84;
		return result;
	}
	float kmToInch(float km){
		float result = km * 39370.1;
		return result;
	}
	float kmToCm(float km){
		float result = km * 100000;
		return result;
	}
	float kmToMm(float km){
		float result = km * 1000000;
		return result;
	}
}