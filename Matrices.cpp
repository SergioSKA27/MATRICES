#include <iostream>
#include <vector>
#include <concepts>
#include <string>
#include <cstddef>
#include <ctype.h>

class Numero
{
private:
    long double Rvalue;         //valor real
    char tipo;                  //conjunto de numeros a utilizar
    std::string representacion; //como imprimir el numero;

    long long Qvalue_num; //valor Racional(numerador)
    long long Qvalue_den; //valor Racional(denominador)

    long long Zvalue; //valor entero

public:
    Numero(long double value);                          //constructor real
    Numero(long long numerador, long long denominador); //constructor Racional o entero

    friend std::ostream &operator<<(std::ostream &o, const Numero &num)
    {
        if (num.tipo == 'R')
        {
            o << num.Rvalue;
        }
        else
        {
            if (num.Qvalue_den == 1)
            {
                o << num.Qvalue_num;
            }
            else
            {
                o << num.Qvalue_num << "/" << num.Qvalue_den;
            }
        }
        return o;
    }

    Numero operator+(const Numero &num2);
    Numero operator-(const Numero &num2);
    Numero operator*(const Numero &num2);
    Numero operator/(const Numero &num2);
    Numero &operator=(const Numero &num2);

    Numero();
    ~Numero();
};

Numero::Numero(long double value)
{
    this->tipo = 'R';
    this->Rvalue = value;
}

Numero::Numero(long long numerador, long long denominador)
{ //Para construir un entero utiliza un uno como denominador
    this->Qvalue_num = numerador;
    this->Qvalue_den = denominador;
    if (denominador == 0)
        throw std::invalid_argument("Denominador 0");

    if (denominador == 1)
        Zvalue = numerador;

    if (denominador != 0 && numerador == 0)
    {
        Zvalue = 0;
    }

    this->tipo = 'Q';
}

Numero::Numero()
{
}

Numero::~Numero()
{
}

/*
    Clase para ejecutar todo tipo de operaciones sobre matrices 
    suma, resta , multiplicacion 

*/
template <typename type>
class Matrix
{
private:
    type **Mat;
    int filas;
    int columnas;

    inline Matrix<type> ExtractMat(type **Mat, size_t sz, size_t F, size_t C); //funcion para el Determinate
    type Det(type **Mat, size_t sz);                                           //Calcula el determinante por cofactores

    type brackethelp(type *fila, int col); //funcion de ayuda para el operador corche
    int brcketaux, bracketaux2;            //variables auxiliares para indices en corchetes

public:
    Matrix(const type init, size_t filas, size_t columnas); //Para cualquier Matrix inicializada
    Matrix(size_t filas, size_t columnas);                  //Para cualquier Matrix
    Matrix(size_t size);                                    //Para Matrices cuadradas
    Matrix();                                               //Constructor vacio

    Matrix &resize(size_t filas, size_t columnas); //Redimensionar Matrix sin perder los datos

    void print(); //Imprimir la matrix

    type Determinante(); //Devuelve el determinante de la matrix

    Matrix<type> transp(); //Devuelve la transpuesta de la matrix
    Matrix<type> adj();    //devuelve la adjunta de la matrix

    Matrix<type> inversa(); //Retorna la inversa de una matrix (mediante la ajunta de la matrix)

    Matrix<type> operator+(const Matrix<type> &Mat2);  //Suma de matrices
    Matrix<type> operator-(const Matrix<type> &Mat2);  //Resta de matrices
    Matrix<type> operator*(const Matrix<type> &Mat2);  //Multiplicacion de matrices
    Matrix<type> &operator=(const Matrix<type> &Mat2); //Operador de asignacion

    type *operator[](const int index);        //Operador corchete para filas
    type operator[](short int index2);        //Operador corchete para columnas
    Matrix<type> &operator=(const type Data); //Para asignar valor a las casillas de la matrix

    friend std::ostream &operator<<(std::ostream &o, const Matrix<type> &Mat)
    {
        for (int i = 0; i < Mat.filas; i++)
        {
            for (int j = 0; j < Mat.columnas; j++)
            {
                if (j == 0)
                    o << "| ";

                o << Mat.Mat[i][j] << "\t";
            }
            o << "\t|";
            o << "\n";
        }

        return o;
    }

    ~Matrix();
};

template <typename type>
Matrix<type>::Matrix(size_t filas, size_t columnas)
{
    this->Mat = new type *[filas];

    for (int i = 0; i < filas; i++)
    {
        this->Mat[i] = new type[columnas];
    }
    this->filas = filas;
    this->columnas = columnas;
}

template <typename type>
Matrix<type>::Matrix(size_t size)
{
    this->Mat = new type *[size];

    for (int i = 0; i < size; i++)
    {
        this->Mat[i] = new type[size];
    }
    this->filas = size;
    this->columnas = size;
}

template <typename type>
Matrix<type>::Matrix(const type init, size_t filas, size_t columnas)
{
    this->Mat = new type *[filas];

    for (int i = 0; i < filas; i++)
    {
        this->Mat[i] = new type[columnas];
    }

    for (int i = 0; i < filas; i++)
    {
        for (int j = 0; j < columnas; j++)
        {
            this->Mat[i][j] = init;
        }
    }

    this->filas = filas;
    this->columnas = columnas;
}

template <typename type>
Matrix<type>::Matrix()
{
    this->Mat = NULL;
    this->filas = 0;
    this->columnas = 0;
}

template <typename type>
Matrix<type> &Matrix<type>::resize(size_t filas, size_t columnas)
{
    type **newMat;

    newMat = new type *[filas];

    for (int i = 0; i < filas; i++)
    {
        newMat[i] = new type[columnas];
    }

    for (int i = 0; i < this->filas; i++)
    {
        for (int j = 0; j < this->columnas; j++)
        {
            if (i < this->filas && j < this->columnas)
                newMat[i][j] = this->Mat[i][j];
        }
    }

    this->Mat = newMat;
    this->filas = filas;
    this->columnas = columnas;

    return *this;
}

template <typename type>
inline Matrix<type> Matrix<type>::ExtractMat(type **Mat, size_t sz, size_t F, size_t C)
{
    Matrix<type> result(sz - 1, sz - 1);
    int k = 0, l = 0;

    for (int i = 0; i < sz; i++)
    {
        l = 0;
        for (int j = 0; j < sz; j++)
        {
            if (i != F && j != C)
            { //Si no estamos en la fila y la columna que se van a eliminar asignamos
                //el valor en esa posicion al valor k,l de la matrix resultado
                result.Mat[k][l] = Mat[i][j];
                l++; //iteramos las columnas de la matrix resultado
            }
        }
        if (i != F) //si no estamos en la fila que se va a eliminar iteramos k
            k++;    //iteramos las filas de la matrix resultado
    }

    return result;
}

template <typename type>
type Matrix<type>::Det(type **Mat, size_t sz)
{

    type detval;

    type dt;

    detval = 0;

    if (sz == 2)
    {
        detval = ((Mat[0][0] * Mat[1][1]) - (Mat[0][1] * Mat[1][0]));
        return detval;
    }
    else
    {
        Matrix<type> res(sz - 1, sz - 1);

        for (int i = 0; i < sz; i++)
        {
            res = this->ExtractMat(Mat, sz, 0, i);

            dt = this->Det(res.Mat, sz - 1);

            if (i % 2 == 0)
            {
                detval += Mat[i][0] * dt;
            }
            else
            {
                detval += (Mat[i][0] * dt) * -1;
            }
        }
        return detval;
    }
}

template <typename type>
type Matrix<type>::Determinante()
{
    if (this->filas != this->columnas)
        throw std::invalid_argument("La matriz no es cuadrada");

    return this->Det(this->Mat, this->filas);
}

template <typename type>
Matrix<type> Matrix<type>::transp()
{
    Matrix<type> T(this->columnas, this->filas);

    for (int i = 0; i < this->columnas; i++)
    {
        for (int j = 0; j < this->filas; j++)
        {
            T.Mat[i][j] = this->Mat[j][i];
        }
    }

    return T;
}

template <typename type>
Matrix<type> Matrix<type>::adj()
{
    Matrix<type> AD(this->filas, this->columnas);
    Matrix<type> aux(this->filas - 1, this->columnas - 1);

    for (int i = 0; i < this->columnas; i++)
    {

        for (int j = 0; j < this->filas; j++)
        {
            aux = this->ExtractMat(this->Mat, this->filas, i, j);
            if (((i + 1) + (j + 1)) % 2 == 0)
            {
                AD.Mat[i][j] = aux.Determinante();
            }
            else
            {
                AD.Mat[i][j] = aux.Determinante() * -1;
            }
        }
    }

    return AD;
}

template <typename type>
Matrix<type> Matrix<type>::inversa()
{ /*Calculamos la inversa sabiendo que la inversa de una matriz A es:
            trans(adj(A))
    inv(A)= -------------
                |A|
    Donde |A| representa el determiante de la matriz.
*/
    if (this->Determinante() == 0)
        throw std::invalid_argument("La matriz no tiene inversa(Determinante igual a 0\n");

    Matrix<type> Inv, aux;

    type det = this->Determinante();

    aux = this->adj();

    Inv = aux.transp();

    for (int i = 0; i < Inv.filas; i++)
    {
        for (int j = 0; j < Inv.columnas; j++)
        {
            Inv.Mat[i][j] = Inv.Mat[i][j] / det;
        }
    }

    return Inv;
}

template <typename type>
void Matrix<type>::print()
{
    if (Mat == NULL)
        throw std::invalid_argument("La Matriz es de tamano 0\n");

    for (int i = 0; i < this->filas; i++)
    {
        for (int j = 0; j < this->columnas; j++)
        {
            std::cout << this->Mat[i][j] << "\t";
        }
        std::cout << std::endl;
    }
    std::cout << std::endl;
}

template <typename type>
Matrix<type> Matrix<type>::operator+(const Matrix<type> &Mat2)
{
    if (this->filas != Mat2.filas || this->columnas != Mat2.columnas)
        throw std::invalid_argument("Las Matrices no tienen el mismo tamano\n");
    Matrix<type> Result(this->filas, this->columnas);

    for (int i = 0; i < this->filas; i++)
    {
        for (int j = 0; j < this->columnas; j++)
        {
            Result.Mat[i][j] = this->Mat[i][j] + Mat2.Mat[i][j];
        }
    }

    return Result;
}

template <typename type>
Matrix<type> Matrix<type>::operator-(const Matrix<type> &Mat2)
{
    if (this->filas != Mat2.filas || this->columnas != Mat2.columnas)
        throw std::invalid_argument("Las Matrices no tienen el mismo tamano\n");
    Matrix<type> Result(this->filas, this->columnas);

    for (int i = 0; i < this->filas; i++)
    {
        for (int j = 0; j < this->columnas; j++)
        {
            Result.Mat[i][j] = this->Mat[i][j] - Mat2.Mat[i][j];
        }
    }

    return Result;
}

template <typename type>
Matrix<type> Matrix<type>::operator*(const Matrix<type> &Mat2)
{
    if (this->filas != Mat2.columnas)
        throw std::invalid_argument("Las Matrices no se pueden multiplicar\n");
    Matrix<type> Result(this->filas, Mat2.columnas);

    type sum; //si se planea usar objetos estos tienen que tener un 0 y sobrecargar el operador = para poder asignarlo

    sum = 0;

    for (int i = 0; i < this->filas; i++)
    {
        sum = 0;
        for (int j = 0; j < Mat2.columnas; j++)
        {
            for (int k = 0; k < Mat2.columnas; k++)
            {
                sum = sum + (this->Mat[i][k] * Mat2.Mat[k][j]);
                Result.Mat[k][j] = sum;
            }
        }
    }

    return Result;
}

template <typename type>
Matrix<type> &Matrix<type>::operator=(const Matrix<type> &Mat2)
{
    if (this->filas != Mat2.filas || this->columnas != Mat2.columnas)
        this->resize(Mat2.filas, Mat2.columnas);

    for (int i = 0; i < this->filas; i++)
    {
        for (int j = 0; j < this->columnas; j++)
        {
            this->Mat[i][j] = Mat2.Mat[i][j];
        }
    }

    this->filas = Mat2.filas;
    this->columnas = Mat2.columnas;

    return *this;
}

template <typename type>
type Matrix<type>::brackethelp(type *fila, int col)
{
    return fila[col];
}

template <typename type>
type *Matrix<type>::operator[](const int index)
{
    if (index >= this->filas)
        throw std::out_of_range("Fuera del rango de la matrix\n");
    this->brcketaux = index;
    return this->Mat[index];
}

template <typename type>
type Matrix<type>::operator[](short int index2)
{
    if (index2 >= this->columnas)
        throw std::out_of_range("Fuera del rango de la matrix\n");

    this->bracketaux2 = index2;
    return (this->brackethelp((*this)[this->brcketaux], index2));
}

template <typename type>
Matrix<type> &Matrix<type>::operator=(const type Data)
{
    this->Mat[brcketaux][bracketaux2] = Data;
    return *this;
}

template <typename type>
Matrix<type>::~Matrix()
{
    for (int i = 0; i < this->filas; i++)
    {
        delete this->Mat[i];
    }
    delete this->Mat;
}

int main(int argc, char const *argv[])
{
    Matrix<char> Mat(4);

    for (int i = 0; i < 4; i++)
    {
        for (int j = 0; j < 4; j++)
        {
            int a;
            std::cin >> a;
            Mat[i][j] = a;
        }
    }

    std::cout << Mat << std::endl;

    return 0;
}
