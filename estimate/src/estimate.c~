#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>


typedef struct {
int rows;
int cols;
double** data;
}matrix;

//declaration of functions
double** allocate(matrix);
matrix transposeOf(matrix);
matrix inverseOf(matrix);
void initializeMatrixX(matrix);
void printMatrix(matrix);
matrix multiply(matrix,matrix);
double ** createIdentity(int);

//global variables
 //matrix X; //matrix for house data
 //matrix Y; //matrix for house prices
 //matrix W; //matrix for attribute weights

int main(int argc, char* argv[]){

int k; //number of columns in the matrix
int n; //number of rows in the matrix
char string [20];
//read the training data and determine weights
FILE *fp;
fp = fopen(argv[1] , "r");
if(fp==NULL){
	printf("error\n");
	return 0;
	exit(0);
}
fscanf(fp, "%s",string); //first line of the file is a string

fscanf(fp, "%d",&k); //second line of the file is k, number of attributes

fscanf(fp, "%d", &n); //third line of the file is n, number of houses

//initialize matrix X
matrix X;
X.rows = n;
X.cols = k+1;
X.data = allocate(X);
initializeMatrixX(X);

//initialize vector Y
matrix Y;
Y.rows = n;
Y.cols =1;
Y.data = allocate(Y);

//initialize vector W
matrix W;
W.rows = k+1;
W.cols = 1;
W.data = allocate(W);

//put elements in matrix
int limit  = n*(k+1);
double * array = (double*) malloc(sizeof(double) * limit);
for(int i=0;i<limit;i++){
double num;
fscanf(fp, "%lf",&num);
array[i] = num;
}

double** temp = (double**) malloc(sizeof(double*) * n);
for(int i=0;i<n;i++){
temp[i] = (double*) malloc(sizeof(double) * (k+1));
}

int arrayCounter=0;
for(int i=0;i<n;i++){
for(int j=0;j<k+1;j++){
	temp[i][j] = array[arrayCounter++];
}
}
/*
matrix te;
te.rows = n;
te.cols = k+1;
te.data = temp;
printMatrix(te);
*/
free(array);

//get values into matrix X
int tempCol=0;
for(int i=0;i<X.rows;i++){
	tempCol=0;
for(int j=1;j<X.cols;j++){
	X.data[i][j] = temp[i][tempCol++];	
}
}
//get Values into matrix Y
for(int i=0;i<Y.rows;i++){
Y.data[i][0] = temp[i][k];
}
for(int i=0;i<n;i++){
free(temp[i]);
}
free(temp);

//compute the weights

matrix XT = transposeOf(X); // tranpose of X
//printMatrix(XT);
matrix XTX = multiply(XT, X); //product of XT and X
//printMatrix(XTX);
matrix beta = inverseOf(XTX); //inverse of the square matrix
//printMatrix(beta);

matrix echo = multiply(beta, XT);
W = multiply(echo, Y);


//read the test data file and compute price
FILE *f1;
f1 = fopen(argv[2], "r");
if(f1==NULL){
printf("error\n");
return 0;
exit(0);
}

fscanf(f1, "%s", string);

fscanf(f1, "%d", &k);

fscanf(f1, "%d", &n);

X.rows = n;
X.cols = k+1;
initializeMatrixX(X);
//put house data into matrix X
limit = n * (k+1);
double * array2 = (double*) malloc(sizeof(double) * limit);
for(int i=0;i<limit;i++){
double input;
fscanf(f1, "%lf" , &input);
array2[i] = input;
}
arrayCounter=0;
for(int i=0;i<X.rows;i++){
for(int j=1;j<X.cols;j++){
	X.data[i][j] = array2[arrayCounter++];
}
}
free(array2);

//calculate price for each house
int weightCounter=0; //index of the weights matrix
for(int i=0;i<X.rows;i++){
weightCounter=0;
double price=0;
for(int j=0;j<X.cols;j++){
	double sum = W.data[weightCounter++][0] * X.data[i][j];
	price+=sum; 
}
printf("%.0f\n" , price);
}

free(X.data);
free(Y.data);
free(W.data);
return 0;
exit(0);
}

double ** allocate(matrix alpha){
matrix answer;
answer.rows = alpha.rows;
answer.cols = alpha.cols;
answer.data = (double**) malloc(sizeof(double*) * alpha.rows);
for(int i=0;i<answer.rows;i++){
answer.data[i] = (double*) malloc(sizeof(double) * alpha.cols);
}
return answer.data;
}


matrix transposeOf(matrix a){
	matrix ans;
	ans.rows = a.cols;
	ans.cols = a.rows;
	ans.data = allocate(ans);
	for(int i=0;i<a.cols;i++){
	for(int j=0;j<a.rows;j++){
	ans.data[i][j] = a.data[j][i];	
	}	
	}
	return ans;
}

matrix inverseOf(matrix M){
//check if matrix is invertible
int a = 0;
for(int i=0;i<M.cols;i++){
	if(a==M.cols){ //if there is a zero row, then matrix is not invertible
	return M;	
	}else{
		a=0;
	}
 for(int j=0;j<M.rows;j++){
	 if(M.data[j][i] == 0){
		a++; 
	 }
 }
}


matrix N;
N.rows = M.rows;
N.cols = M.cols;
N.data = createIdentity(M.rows);

double pivot;
for(int p=0;p<M.rows;p++){
	pivot = M.data[p][p];

	for(int a=0;a<M.cols;a++){
	M.data[p][a]  = M.data[p][a]/pivot;
	N.data[p][a] = N.data[p][a]/pivot;	
	}
	for(int i=p+1; i<M.rows ;i++){
	pivot = M.data[i][p];
	for(int b=0;b<M.cols;b++){
	M.data[i][b] = M.data[i][b] - ( M.data[p][b] * pivot);
	N.data[i][b] = N.data[i][b] - ( N.data[p][b] * pivot);
	}
	}//end for
}//end for 

for(int p=M.cols-1;p>=0;p--){
for(int i=p-1;i>=0;i--){
	double pivot = M.data[i][p];
	//subtract Mp and Np times piv from Mi	
	for(int a=0;a<M.cols;a++){
	M.data[i][a] = M.data[i][a] - (M.data[p][a] * pivot);
	N.data[i][a] = N.data[i][a] - (N.data[p][a] * pivot);	
	}
}//end for
}//end for

return N;
}

void initializeMatrixX(matrix xylo){
for(int i=0;i<xylo.rows;i++){
	for(int j=0;j<xylo.cols;j++){
	if(j==0){
	xylo.data[i][j] = 1;
	continue;
}
	xylo.data[i][j]=0;
	}
 }
}

matrix multiply(matrix mat1 , matrix mat2){
int row1 = mat1.rows;
int row2 = mat2.rows;
int col2 = mat2.cols;

//intialize product matrix
matrix product;
product.rows = row1;
product.cols = col2;
product.data = allocate(product);

for(int i=0;i<row1;i++){
  for(int j=0;j<col2;j++){
	product.data[i][j] =0;
	for(int k=0;k<row2;k++){
	double sum = mat1.data[i][k] * mat2.data[k][j];
	product.data[i][j]= product.data[i][j]+sum;
	}  
  }
}
return product;
}

void printMatrix(matrix ans){
for(int i=0;i<ans.rows;i++){
	for(int j=0;j<ans.cols;j++){
	printf("%lf ",ans.data[i][j]);	
	}
	printf("\n");
}
printf("\n");
}

double **createIdentity(int size){
double ** identity = (double**) malloc(sizeof(double*) * size);
for(int i=0;i<size;i++){
	identity[i] = (double*) malloc(sizeof(double) * size);
}

for(int i=0;i<size;i++){
 for(int j=0;j<size;j++){
	identity[i][j] = 0; 
 }
}
for(int i=0;i<size;i++){
	identity[i][i] = 1;
}

return identity;
}






