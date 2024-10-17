#include "MyForm.h"
#include <cmath>
#include <cstdlib>
#include <ctime>


using namespace System;
using namespace System::Windows::Forms;

[STAThreadAttribute] //Атрибут для main
int main(array<String^>^ args) //^ - вместо delete
{
	Application::SetCompatibleTextRenderingDefault(false); //Обработа тескта
	Application::EnableVisualStyles(); //Обработка визуальных стилей 
	Approx::MyForm form; //Обращение к классу и создание элемента form
	Application::Run(% form); // Обращение к методу Run, передача по ссылке form

}

double F(double x) //Функция
{
	return x * x * sin(x);
	//return 2 * exp(-x) + x;
}
 
double Approx::MyForm::P(double x,int n) //Полином 
{
	double p = 0;
	double* coefficients = new double[n + 1]; // массив для коэффициентов полинома

	// Рассчитываем коэффициенты полинома с помощью метода Гаусса
	Gauss(n, m, a, b); // Вызываем метод Гаусса

	// Заполняем массив coefficients
	for (int i = 0; i <= n; i++) {
		coefficients[i] = C[i];
	}

	// Вычисляем значение полинома
	for (int i = 0; i <= n; i++) {
		p += coefficients[i] * pow(x, i);
	}

	delete[] coefficients;
	return p;
	
}

double Approx::MyForm::Gauss(int n, int m, double a, double b)
{
	x = new double[m + 1];
	z = new double[m + 1];
	f = new double[m + 1];
	D = new double[2 * n + 1]; //Вспомогательный массив для A
	B = new double[n + 1]; //Столбец свободных членов
	C = new double[n + 1];
	A = new double* [n + 1]; 
	for (int i = 0; i <= n; i++)
	{
		A[i] = new double[n + 1];
	}

	for (int i = 0; i <= m; i++) //Подсчет нужных точек 
	{ 
		x[i] = a + i * (b - a) / m;
		z[i] = static_cast<double>(rand()) / RAND_MAX;
		if (z[i] < 0.5) k = -1;
		else k = 1;
		f[i] = F(x[i]) * (1 + k * z[i] / c);
	}

	
	for (int k = 0; k <= 2 * n; k++) //Заполенение D
	{
		D[k] = 0;
		for (int i = 0; i <= m; i++)
		{
			D[k] += pow(x[i], k);
		}
	}

	for (int k = 0; k <= n; k++) //Заполнение B
	{
		B[k] = 0;
		for (int i = 0; i <= m; i++)
		{
			B[k] += f[i] * pow(x[i], k);
		}
	}

	for (int i = 0; i <= n; i++) //Заполение A через D
	{
		for (int j = 0; j <= n; j++)
		{
			A[i][j] = D[i + j]; 
		}
	}

	for (int i = 0; i <= n; i++) {
		int maxRow = i;
		for (int k = i + 1; k <= n; k++) {
			if (abs(A[k][i]) > abs(A[maxRow][i])) {
				maxRow = k;
			}
		}

		for (int k = 0; k <= n; k++) {
			double temp = A[i][k];
			A[i][k] = A[maxRow][k];
			A[maxRow][k] = temp;
		}

		double tempB = B[i];
		B[i] = B[maxRow];
		B[maxRow] = tempB;
	}

	// Примените метод Гаусса для преобразования матрицы

	for (int i = 0; i <= n - 1; i++) {
		for (int k = i + 1; k <= n; k++) {
			double coeff = A[k][i] / A[i][i];
			for (int j = i; j <= n; j++) {
				A[k][j] -= coeff * A[i][j];
			}
			B[k] -= coeff * B[i];
		}
	}

	// Обратный ход метода Гаусса
	for (int i = n; i >= 0; i--) {
		for (int k = i + 1; k <= n; k++) {
			B[i] -= A[i][k] * C[k];
		}
		C[i] = B[i] / A[i][i];
	}

	for (int i = 0; i <= n ; i++)
	{
		return C[i];
	}

	// Освободить память
	delete[] x;
	delete[] z;
	delete[] f;
	delete[] D;
	delete[] B;
	delete[] C;

	for (int i = 0; i <= n; i++) {
		delete[] A[i];
	}
	delete[] A;

}

System::Void Approx::MyForm::построитьГрафикToolStripMenuItem_Click(System::Object^ sender, System::EventArgs^ e)
{
	if (textBox_a->Text == "" || textBox_b->Text == "" || textBox_m->Text == "" || textBox_n->Text == "" || textBox_c->Text == "")
	{
		MessageBox::Show("Пустые ячейки заменены по умолчанию!");
		DefaultParams();
	}
	else
	{
		a = Convert::ToDouble(textBox_a->Text);
		b = Convert::ToDouble(textBox_b->Text);
		m = Convert::ToInt32(textBox_m->Text);
		n = Convert::ToInt32(textBox_n->Text);
		c = Convert::ToDouble(textBox_c->Text);
	}

	
	x = new double[m + 1]; //Динамичесий масиив для x
	f = new double[m + 1]; //Динамичесий масиив для f для вывода точек
	z = new double[m + 1]; //Динамичесий масиив для z случайно распределенных точек от 0 до 1
	p = new double[m + 1]; //Динамичесий масиив p для вывода полинома 

	srand(time(NULL)); // Выбор начальной точки 

	this->chart->Series[0]->Points->Clear(); //В начале график нужно отчистить 
	this->chart->Series[1]->Points->Clear(); //В начале график нужно отчистить 
	this->chart->Series[2]->Points->Clear(); //В начале график нужно отчистить 
	for (int i = 0; i <= m; i++)
	{
		z[i] = static_cast<double>(rand()) / RAND_MAX; //Вывод рандомного числа, static_cast<double> - для вывода дробных чисел
		if (z[i] < 0.5) k = -1;
		else k = 1;
		x[i] = a + i * (b - a) / m; //Точки
		f[i] = F(x[i]) * (1 + k* z[i]/c); //Точки при случайном отклонении 
		p[i] = P(x[i],n); //Точки Полинома
		this->chart->Series[0]->Points->AddXY(x[i], f[i]); //Построение точек 
		this->chart->Series[1]->Points->AddXY(x[i], F(x[i])); //Построение графика
		this->chart->Series[2]->Points->AddXY(x[i], p[i]); //Построение полинома
	}

	delete[] x;
	delete[] f;
	delete[] z;
	delete[] p;
}

System::Void Approx::MyForm::очиститьГрафикToolStripMenuItem_Click(System::Object^ sender, System::EventArgs^ e)
{
	this->chart->Series[0]->Points->Clear();
	this->chart->Series[1]->Points->Clear(); 
	this->chart->Series[2]->Points->Clear();
}

System::Void Approx::MyForm::выходToolStripMenuItem_Click(System::Object^ sender, System::EventArgs^ e)
{
	if (MessageBox::Show("Выйти?", "Внимание!", MessageBoxButtons::YesNo) == Windows::Forms::DialogResult::Yes)
	{
		Application::Exit();
	}
}

void Approx::MyForm::DefaultParams()
{
	a = 0;
	b = 4;
	m = 10;
	n = 3;
	c = 5;
}
