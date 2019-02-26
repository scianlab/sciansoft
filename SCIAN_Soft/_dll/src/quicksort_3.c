#include <stdio.h>
#include <stdlib.h>

#define MAXELT          10734460
#define INFINITY        13100070         /* numbers in list should not exceed: 32760*/
                                      /* this. change the value to suit your*/
                                      /* needs*/
#define SMALLSIZE       10            /* not less than 3*/
#define STACKSIZE       100           /* should be ceiling(lg(MAXSIZE)+1)*/

IDL_ULONG list[MAXELT+1];                   /* one extra, to hold INFINITY*/

struct {                              /* stack element.*/
        int a,b;
} stack[STACKSIZE];

int top=-1;                           /* initialise stack*/

void interchange(IDL_ULONG *x,IDL_ULONG *y)        /* swap*/
{
    IDL_ULONG temp;

    temp=*x;
    *x=*y;
    *y=temp;
}

void split(int first,int last,int *splitpoint)
{
    int x,i,j,s,g;

    /* here, atleast three elements are needed*/
    if (list[first]<list[(first+last)/2]) {  /* find median*/
        s=first;
        g=(first+last)/2;
    }
    else {
        g=first;
        s=(first+last)/2;
    }
    if (list[last]<=list[s])
        x=s;
    else if (list[last]<=list[g])
        x=last;
    else
        x=g;
    interchange(&list[x],&list[first]);      /* swap the split-point element*/
                                             /* with the first*/
    x=list[first];
    i=first+1;                               /* initialise*/
    j=last+1;
    while (i<j) {
        do {                                 /* find j */
            j--;
        } while (list[j]>x);
        do {
            i++;                             /* find i*/
        } while (list[i]<x);
        interchange(&list[i],&list[j]);      /* swap*/
    }
    interchange(&list[i],&list[j]);          /* undo the extra swap*/
    interchange(&list[first],&list[j]);      /* bring the split-point*/
                                             /* element to the first*/
    *splitpoint=j;
}

void push(int a,int b)                        /* push*/
{
    top++;
    stack[top].a=a;
    stack[top].b=b;
}

void pop(int *a,int *b)                       /* pop*/
{
    *a=stack[top].a;
    *b=stack[top].b;
    top--;
}

void insertion_sort(int first,int last)
{
    int i,c;
	IDL_ULONG j;

    for (i=first;i<=last;i++) {
        j=list[i];
        c=i;
        while ((list[c-1]>j)&&(c>first)) {
            list[c]=list[c-1];
            c--;
        }
        list[c]=j;
    }
}

void quicksort(int n)
{
    int first,last,splitpoint;

    push(0,n);
    while (top!=-1) {
        pop(&first,&last);
        for (;;) {
            if (last-first>SMALLSIZE) {
                /* find the larger sub-list*/
                split(first,last,&splitpoint);
                /* push the smaller list*/
                if (last-splitpoint<splitpoint-first) {
                    push(first,splitpoint-1);
                    first=splitpoint+1;
                }
                else {
                    push(splitpoint+1,last);
                    last=splitpoint-1;
                }
            }
            else {  /* sort the smaller sub-lists*/
                    /* through insertion sort*/
                insertion_sort(first,last);
                break;
            }
        }
    }                        /* iterate for larger list*/
}




int main() {
  int i, j;
  /*long *ejemplo;*/
  char word[20];
  int N = 10734454;
  long num;
  FILE *fp = fopen("cristi.txt", "rb");
  if (fp == NULL) {
    perror("Failed to open file \"myfile\"");
    return EXIT_FAILURE;
  }

  printf("N = %d\n", N);
 /* ejemplo = (long *)malloc(N*sizeof(long));*/


  for (i = 0; i < N; ++i) {
    for (j= 0; j <20; ++j) {
      word[j] = '\0';
	}
    if(fgets(word, 20, fp) == NULL) {
      perror("Failed to read file");
      printf("i = %d, valor anterior = %ld\n", i, list[i-1]);
      return EXIT_FAILURE;
    }
    sscanf(word, "%ld", &num);
	/*printf(".%ld.\n", num);*/
	list[i] = num;
  }

  fclose(fp);

  printf("\n");
  printf("Antes de qs\n");
  quicksort(i-1);
  printf("Despues: \n");
  i = 0;
  printf("i = %d, valor = %ld\n", i, list[i]);
  i = 1;
  printf("i = %d, valor = %ld\n", i, list[i]);
  i = N - 2;
  printf("i = %d, valor = %ld\n", i, list[i]);
  i = N - 1;
  printf("i = %d, valor = %ld\n", i, list[i]);
  /*
  for(i = 0; i< N; ++i) {
    printf("%ld ", ejemplo[i]);
  }
  */
  printf("\n");
  return 0;
}
