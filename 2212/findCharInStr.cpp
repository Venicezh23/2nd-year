#include <iostream>
#include <bits/stdc++.h>
#include <string>

using namespace std;

//takes in string input and its length + char to find
int findNumOfChar(string strInput, char findChar){
	int counter = 0;
	for (int i = 0; i < strInput.length(); i++){
		if (strInput[i] == findChar){
			counter++;
		}
	}
	return counter;
}

/*
int main(){
	string str;
	char findCharVal;
	cout << "Enter the string: ";
	cin >> str;
	cout << "Enter a character to find: ";
	cin >> findCharVal;
	cout << "String: " << str << "; character to find: " << findCharVal << endl;
	
	int numberOfOccurrences = findNumOfChar(str, findCharVal);
	cout << "Number of occurrences of " << findCharVal << " in " << str << " = " << numberOfOccurrences << endl;
	return 0;
}*/