#include <iostream>

using namespace std;

// Function to convert Fahrenheit to Celsius
namespace convertTempSpace {
    float convertToFah(float celsius) {
        float resultFahrenheit;
        resultFahrenheit = celsius * 1.8 + 32;
        return resultFahrenheit;
    }
    
    float convertToCel(float fahrenheit){
    	float resultCelsius;
    	resultCelsius = (fahrenheit - 32) / 1.8;
    	return resultCelsius;
	}
}

/*
int main() {
    //write a loop to show F to C equivalent from 0 ~ 100 C
    for (int i = 0; i <= 100; i++){
    	cout << "Celsius " << i << " --> Fahrenheit : " << convertTempSpace::convertToFah(i) << endl;
	}
    //write a loop to show C to F equivalent from 32 ~ 212 F
    for (int j = 32; j <= 212; j++){
    	cout << "Fahrenheit " << j << " --> Celsius : " << convertTempSpace::convertToCel(j) << endl;
	}
    return 0;
}*/