/*//#define _USE_MATH_DEFINES
#include <iostream>
//#include <iomanip>

using namespace std;

int main()
{

	double value = 0;
	bool flag1 = true, flag2 = true, flag3 = true, flag4 = true, flag5 = true;
	int i = 0, j = 1;
	do
	{

		i++ % 2 == 0 ? value += (double) 4.0 / j : value -= (double) 4.0 / j;
//		cout << " " << i << "\t " << setprecision(16) << value << "\t " << setprecision(16) << value - M_PI << endl;
		j += 2;
		if (3.1 <= value && value < 3.2 && flag1) // 3.1
		{
			cout << "First one fraction digit is the same in " << i << "th term of series." << endl << endl;
			flag1 = false;
		}
		if (3.14 <= value && value < 3.15 && flag2) // 3.14
		{
			cout << "First two fraction digit is the same in " << i << "th term of series." << endl << endl;
			flag2 = false;
		}
		if (3.141 <= value && value < 3.142 && flag3) // 3.141
		{
			cout << "First three fraction digit is the same in " << i << "th term of series." << endl << endl;
			flag3 = false;
		}
		if (3.1415 <= value && value < 3.1416 && flag4) // 3.1415
		{
			cout << "First four fraction digit is the same in " << i << "th term of series." << endl << endl;
			flag4 = false;
		}
		if (3.14159 <= value && value < 3.1416 && flag5) // 3.14159
		{
			cout << "First five fraction digit is the same in " << i << "th term of series." << endl << endl;
			flag5 = false;
		}
	} while (flag5);
}*/

#include <iostream>
#include <iomanip>
using namespace std;

int main() {
    int terms;
    double pi = 0;
    
    cout << "Enter the number of terms to approximate p: ";
    cin >> terms;
    
    cout << "Number of Terms   Approximated Value of p" << endl;
    cout << "----------------------------------------" << endl;
    
    for (int i = 1; i <= terms; i++) {
        if (i % 2 == 0) {
            pi -= 4.0 / (2 * i - 1);
        } else {
            pi += 4.0 / (2 * i - 1);
        }
        
        cout << setw(10) << i << setw(20) << fixed << setprecision(10) << pi << endl;
    }
    
    // Check if we've reached desired precision
    for (int i = 1; i <= terms; i++) {
        if (pi >= 3.14 && pi < 3.141) {
            cout << "p first approximates 3.14 after " << i << " terms." << endl;
            break;
        }
        if (pi >= 3.141 && pi < 3.1415) {
            cout << "p first approximates 3.141 after " << i << " terms." << endl;
            break;
        }
        if (pi >= 3.1415 && pi < 3.14159) {
            cout << "p first approximates 3.1415 after " << i << " terms." << endl;
            break;
        }
        if (pi >= 3.14159) {
            cout << "p first approximates 3.14159 after " << i << " terms." << endl;
            break;
        }
    }
    
    return 0;
}

