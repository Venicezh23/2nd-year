/*
11.In a company an employee is paid as under: 
If his basic salary is less than Rs. 1500, then HRA = 10% of basic salary 
and DA = 90% of basic salary.
If his salary is either equal to or above Rs. 1500, 
then HRA = Rs. 500 and DA = 98% of basic salary. 
If the employee's salary is input by the user 
write a program to find his gross salary.
*/
#include <iostream>
#include <bits/stdc++.h>

using namespace std;
/*
int main(){
	float basicSalary, grossSalary, hra, da;
	
	//basic salary
	cout << "Enter basic salary (Rs): ";
	cin >> basicSalary;
	
	//if else statement
	if(basicSalary < 1500){
		hra = 0.1 * basicSalary;
		da = 0.9 * basicSalary;
	} else {
		hra = 500;
		da = 0.98 * basicSalary;
	}
	
	grossSalary = basicSalary + hra + da;
	cout << "Gross salary : Rs. " << grossSalary << endl;
	return 0;
}*/