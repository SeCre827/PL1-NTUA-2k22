#include <stdio.h>
#include <math.h>
#include <ctype.h>

const double freqs[26] =
    {0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
     0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
     0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
     0.00978, 0.02360, 0.00150, 0.01974, 0.00074};

unsigned int counter = 0;
unsigned int instances[26] = {0};


    /*   0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
     0   a
     1   b  c
     2   c  d  e
     3   d  e  f  g
     4   e  f  g  h  i
     5   f  g  h  i  j  k
     6   g  h  i  j  k  l  m
     7   h  i  j  k  l  m  n  o
     8   i  j  k  l  m  n  o  p  q
     9   j  k  l  m  n  o  p  q  r  s
     10  k  l  m  n  o  p  q  r  s  t  u
     11  l  m  n  o  p  q  r  s  t  u  v  w
     12  m  n  o  p  q  r  s  t  u  v  w  x  y
     13  n  o  p  q  r  s  t  u  v  w  x  y  z  a
     14  o  p  q  r  s  t  u  v  w  x  y  z  a  b  c
     15  p  q  r  s  t  u  v  w  x  y  z  a  b  c  d  e
     16  q  r  s  t  u  v  w  x  y  z  a  b  c  d  e  f  g
     17  r  s  t  u  v  w  x  y  z  a  b  c  d  e  f  g  h  i
     18  s  t  u  v  w  x  y  z  a  b  c  d  e  f  g  h  i  j  k
     19  t  u  v  w  x  y  z  a  b  c  d  e  f  g  h  i  j  k  l  m
     20  u  v  w  x  y  z  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o
     21  v  w  x  y  z  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q
     22  w  x  y  z  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s
     23  x  y  z  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u
     24  y  z  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w
     25  z  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y 
    */
 
int main(int argc, char * argv[]) {

    char c;
    FILE * fp = fopen(argv[1], "r");
    if (fp == NULL) {
        perror("Error in opening file");
        return -1;
    }
    do {
        if (isalpha( c = fgetc(fp) )) {
            if (c <= 'Z') // capital. instances are rec'd for lowercase.
                c += 'a'-'A';
            instances[c - 'a'] += 1;
        }

        if (feof(fp)) { // EOF
            break;
        }
        // printf("%c\n", c);
    } while (1);
    
    double min_entropy = 0;
    int index = 0;

    for (int i = 0; i < 26; ++i){
        double entropy = 0;
        for (int j = 0; j < 26; ++j){
            // dot product
            entropy -= log(freqs[(i+j)%26])*instances[j];
        }
        if (entropy < min_entropy || i == 0) {
            min_entropy = entropy;
            index = i; // decryption offset
        }
    }
    // restart file pointer for decryption
    // based on selected index-cipher.
    fseek(fp, 0, SEEK_SET);
    do {
        if ( isalpha( c = fgetc(fp)) ) {
            char let = c <= 'Z' ? 'A' : 'a';
            c = (c+index)%let%26 + let;
        }

        if (feof(fp)) { // EOF
            break;
        }
        printf("%c", c);
    } while (1);
    

    fclose(fp);
    return 0;
}