#include "stdlib.h"
#include "stdio.h"

struct polynome_s {

    int deg;
    double* coeff;

};

typedef struct polynome_s polynome;


void print_poly(polynome* p) {
    
    int d = p->deg;
    if (d == -1) {
        printf("X^0 : 0\n");
    } else {
        for (int i=0; i<=d; i++) {
            printf("X^%d : %lf\n", i, p->coeff[i]);
        }
    }

}

int min(int a, int b) {
    return a<b ? a : b;
}

int max(int a, int b){
    return a>b ? a : b;
}

polynome soustraction_poly(polynome* p, polynome* q) {

    int d = max(p->deg, q->deg);
    polynome resultat;
    resultat.deg = d;
    resultat.coeff = malloc(sizeof(double)*(d+1));
    
    for (int i=0; i<=d; i++) {

        resultat.coeff[i] = 0;
        if (i <= p->deg) {
            resultat.coeff[i] += p->coeff[i];
        }
        if (i <= q->deg) {
            resultat.coeff[i] -= q->coeff[i];
        }

    }

    return resultat;

}

polynome produit_poly(polynome* p, polynome* q) {

    if (p->deg == -1 || q->deg == -1) {

        polynome resultat;
        resultat.deg = -1;
        resultat.coeff = NULL;
        return resultat;

    } else if (p->deg == 0 || q->deg == 0) {

        polynome resultat;
        resultat.deg = p->deg + q->deg;
        resultat.coeff = malloc(sizeof(double)*(resultat.deg+1));

        if (p->deg == 0) {

            for (int i=0; i<=q->deg; i++) {
                resultat.coeff[i] = q->coeff[i]*p->coeff[0];
            }

        } else {

            for (int i=0; i<=p->deg; i++) {
                resultat.coeff[i] = p->coeff[i]*q->coeff[0];
            }

        }

        return resultat;

    } else {

        int k = max(p->deg/2, q->deg/2) + 1;

        int degP0 = min(k-1, p->deg);
        int degP1 = max(p->deg - k, -1);

        int degQ0 = min(k-1, q->deg);
        int degQ1 = max(q->deg - k, -1);

        polynome p0;
        p0.deg = degP0;
        p0.coeff = p->coeff;

        polynome p1;
        p1.deg = degP1;
        p1.coeff = degP1 == -1 ? NULL : &(p->coeff[degP0+1]);

        polynome q0;
        q0.deg = degQ0;
        q0.coeff = q->coeff;

        polynome q1;
        q1.deg = degQ1;
        q1.coeff = degQ1 == -1 ? NULL : &(q->coeff[degQ0+1]);

        polynome p0q0 = produit_poly(&p0, &q0);
        polynome p1q1 = produit_poly(&p1, &q1);

        polynome p1Mp0 = soustraction_poly(&p1, &p0);
        polynome q1Mq0 = soustraction_poly(&q1, &q0);
        polynome prodDelta = produit_poly(&p1Mp0, &q1Mq0);

        free(p1Mp0.coeff);
        free(q1Mq0.coeff);

        polynome resultat;
        resultat.deg = p->deg + q->deg;
        resultat.coeff = malloc(sizeof(double)*(resultat.deg+1));

        for (int i=0; i<=resultat.deg; i++) {
            resultat.coeff[i] = 0;
        }

        for (int i=0; i<=p0q0.deg; i++) {
            resultat.coeff[i] += p0q0.coeff[i];
            resultat.coeff[i+k] += p0q0.coeff[i];
        }

        for (int i=0; i<=p1q1.deg; i++) {
            resultat.coeff[i+k] += p1q1.coeff[i];
            resultat.coeff[i+(2*k)] += p1q1.coeff[i];
        }

        for (int i=0; i<=prodDelta.deg; i++) {
            resultat.coeff[i+k] -= prodDelta.coeff[i];
        }

        free(p0q0.coeff);
        free(p1q1.coeff);
        free(prodDelta.coeff);

        return resultat;

    }

}

int main() {

    int d = 2;

    polynome p = {.deg = d, .coeff = malloc(sizeof(double)*(d+1))};
    polynome q = {.deg = d-1, .coeff = malloc(sizeof(double)*(d+1))};
    
    p.coeff[0] = 1;
    p.coeff[1] = 1;
    p.coeff[2] = 1;

    q.coeff[0] = 0;
    q.coeff[1] = 5;

    polynome resultat = produit_poly(&p, &q);

    printf("P : \n");
    print_poly(&p);
    printf("\n");
    printf("Q : \n");
    print_poly(&q);
    printf("\n");

    printf("Resultat : \n");
    print_poly(&resultat);

    return EXIT_SUCCESS;

}