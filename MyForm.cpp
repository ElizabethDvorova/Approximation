#include "MyForm.h"
#include <cmath>
#include <cstdlib>
#include <ctime>


using namespace System;
using namespace System::Windows::Forms;

[STAThreadAttribute] //������� ��� main
int main(array<String^>^ args) //^ - ������ delete
{
	Application::SetCompatibleTextRenderingDefault(false); //�������� ������
	Application::EnableVisualStyles(); //��������� ���������� ������ 
	Approx::MyForm form; //��������� � ������ � �������� �������� form
	Application::Run(% form); // ��������� � ������ Run, �������� �� ������ form

}

double F(double x) //�������
{
	return x * x * sin(x);
	//return 2 * exp(-x) + x;
}
 
double Approx::MyForm::P(double x,int n) //������� 
{
	double p = 0;
	double* coefficients = new double[n + 1]; // ������ ��� ������������� ��������

	// ������������ ������������ �������� � ������� ������ ������
	Gauss(n, m, a, b); // �������� ����� ������

	// ��������� ������ coefficients
	for (int i = 0; i <= n; i++) {
		coefficients[i] = C[i];
	}

	// ��������� �������� ��������
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
	D = new double[2 * n + 1]; //��������������� ������ ��� A
	B = new double[n + 1]; //������� ��������� ������
	C = new double[n + 1];
	A = new double* [n + 1]; 
	for (int i = 0; i <= n; i++)
	{
		A[i] = new double[n + 1];
	}

	for (int i = 0; i <= m; i++) //������� ������ ����� 
	{ 
		x[i] = a + i * (b - a) / m;
		z[i] = static_cast<double>(rand()) / RAND_MAX;
		if (z[i] < 0.5) k = -1;
		else k = 1;
		f[i] = F(x[i]) * (1 + k * z[i] / c);
	}

	
	for (int k = 0; k <= 2 * n; k++) //����������� D
	{
		D[k] = 0;
		for (int i = 0; i <= m; i++)
		{
			D[k] += pow(x[i], k);
		}
	}

	for (int k = 0; k <= n; k++) //���������� B
	{
		B[k] = 0;
		for (int i = 0; i <= m; i++)
		{
			B[k] += f[i] * pow(x[i], k);
		}
	}

	for (int i = 0; i <= n; i++) //��������� A ����� D
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

	// ��������� ����� ������ ��� �������������� �������

	for (int i = 0; i <= n - 1; i++) {
		for (int k = i + 1; k <= n; k++) {
			double coeff = A[k][i] / A[i][i];
			for (int j = i; j <= n; j++) {
				A[k][j] -= coeff * A[i][j];
			}
			B[k] -= coeff * B[i];
		}
	}

	// �������� ��� ������ ������
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

	// ���������� ������
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

System::Void Approx::MyForm::���������������ToolStripMenuItem_Click(System::Object^ sender, System::EventArgs^ e)
{
	if (textBox_a->Text == "" || textBox_b->Text == "" || textBox_m->Text == "" || textBox_n->Text == "" || textBox_c->Text == "")
	{
		MessageBox::Show("������ ������ �������� �� ���������!");
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

	
	x = new double[m + 1]; //����������� ������ ��� x
	f = new double[m + 1]; //����������� ������ ��� f ��� ������ �����
	z = new double[m + 1]; //����������� ������ ��� z �������� �������������� ����� �� 0 �� 1
	p = new double[m + 1]; //����������� ������ p ��� ������ �������� 

	srand(time(NULL)); // ����� ��������� ����� 

	this->chart->Series[0]->Points->Clear(); //� ������ ������ ����� ��������� 
	this->chart->Series[1]->Points->Clear(); //� ������ ������ ����� ��������� 
	this->chart->Series[2]->Points->Clear(); //� ������ ������ ����� ��������� 
	for (int i = 0; i <= m; i++)
	{
		z[i] = static_cast<double>(rand()) / RAND_MAX; //����� ���������� �����, static_cast<double> - ��� ������ ������� �����
		if (z[i] < 0.5) k = -1;
		else k = 1;
		x[i] = a + i * (b - a) / m; //�����
		f[i] = F(x[i]) * (1 + k* z[i]/c); //����� ��� ��������� ���������� 
		p[i] = P(x[i],n); //����� ��������
		this->chart->Series[0]->Points->AddXY(x[i], f[i]); //���������� ����� 
		this->chart->Series[1]->Points->AddXY(x[i], F(x[i])); //���������� �������
		this->chart->Series[2]->Points->AddXY(x[i], p[i]); //���������� ��������
	}

	delete[] x;
	delete[] f;
	delete[] z;
	delete[] p;
}

System::Void Approx::MyForm::��������������ToolStripMenuItem_Click(System::Object^ sender, System::EventArgs^ e)
{
	this->chart->Series[0]->Points->Clear();
	this->chart->Series[1]->Points->Clear(); 
	this->chart->Series[2]->Points->Clear();
}

System::Void Approx::MyForm::�����ToolStripMenuItem_Click(System::Object^ sender, System::EventArgs^ e)
{
	if (MessageBox::Show("�����?", "��������!", MessageBoxButtons::YesNo) == Windows::Forms::DialogResult::Yes)
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
