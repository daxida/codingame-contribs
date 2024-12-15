
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#define MAX_SEQUENCES 100
#define MAX_SEQUENCE_LENGTH 1000

const char* START = "AUG";
const char* STOPS[] = {"UAA", "UAG", "UGA"};
const int NUM_STOP_CODONS = 3;

int is_stop_codon(const char* codon) {
    for (int i = 0; i < NUM_STOP_CODONS; i++) {
        if (strcmp(codon, STOPS[i]) == 0) {
            return 1;
        }
    }
    return 0;
}

typedef struct {
    char codon[4];
    char amino_acid;
} CodonMap;

CodonMap codon_table[] = {
    {"UUU", 'F'}, {"CUU", 'L'}, {"AUU", 'I'}, {"GUU", 'V'},
    {"UUC", 'F'}, {"CUC", 'L'}, {"AUC", 'I'}, {"GUC", 'V'},
    {"UUA", 'L'}, {"CUA", 'L'}, {"AUA", 'I'}, {"GUA", 'V'},
    {"UUG", 'L'}, {"CUG", 'L'}, {"AUG", 'M'}, {"GUG", 'V'},
    {"UCU", 'S'}, {"CCU", 'P'}, {"ACU", 'T'}, {"GCU", 'A'},
    {"UCC", 'S'}, {"CCC", 'P'}, {"ACC", 'T'}, {"GCC", 'A'},
    {"UCA", 'S'}, {"CCA", 'P'}, {"ACA", 'T'}, {"GCA", 'A'},
    {"UCG", 'S'}, {"CCG", 'P'}, {"ACG", 'T'}, {"GCG", 'A'},
    {"UAU", 'Y'}, {"CAU", 'H'}, {"AAU", 'N'}, {"GAU", 'D'},
    {"UAC", 'Y'}, {"CAC", 'H'}, {"AAC", 'N'}, {"GAC", 'D'},
    {"UAA", 'X'}, {"CAA", 'Q'}, {"AAA", 'K'}, {"GAA", 'E'},
    {"UAG", 'X'}, {"CAG", 'Q'}, {"AAG", 'K'}, {"GAG", 'E'},
    {"UGU", 'C'}, {"CGU", 'R'}, {"AGU", 'S'}, {"GGU", 'G'},
    {"UGC", 'C'}, {"CGC", 'R'}, {"AGC", 'S'}, {"GGC", 'G'},
    {"UGA", 'X'}, {"CGA", 'R'}, {"AGA", 'R'}, {"GGA", 'G'},
    {"UGG", 'W'}, {"CGG", 'R'}, {"AGG", 'R'}, {"GGG", 'G'}
};

char get_amino_acid(const char* codon) {
    int num_codons = sizeof(codon_table) / sizeof(codon_table[0]);
    for (int i = 0; i < num_codons; i++) {
        if (strcmp(codon_table[i].codon, codon) == 0) {
            return codon_table[i].amino_acid;
        }
    }
    return 'X'; // filler value
}

void solve(const char* rna) {
    int len = strlen(rna);
    char* ans = malloc(MAX_SEQUENCE_LENGTH * sizeof(char));
    int max_n_amino_acids = 0;

    for (int offset = 0; offset < 3; offset++) {
        int state = 0; // 0 for closed, 1 for opened

        char sequences[MAX_SEQUENCES][MAX_SEQUENCE_LENGTH] = {0};
        int sequence_count = 0;
        char sequence[MAX_SEQUENCE_LENGTH] = {0};
        int seq_index = 0;
        int n_amino_acids = 0;

        for (int pos = offset; pos + 2 < len; pos += 3) {
            char codon[4];
            strncpy(codon, rna + pos, 3);
            codon[3] = '\0';

            if (strcmp(codon, START) == 0 && !state) {
                state = 1;
                seq_index = 0;
                memset(sequence, 0, MAX_SEQUENCE_LENGTH);
            }
            if (is_stop_codon(codon) && state) {
                state = 0;
                n_amino_acids += strlen(sequence);
                strncpy(sequences[sequence_count], sequence, MAX_SEQUENCE_LENGTH - 1);
                sequence_count++;
            }
            if (state) {
                char amino_acid = get_amino_acid(codon);
                sequence[seq_index++] = amino_acid;
            }
        }

        if (n_amino_acids > max_n_amino_acids) {
            max_n_amino_acids = n_amino_acids;
            char new_ans[MAX_SEQUENCE_LENGTH] = {0};
            for (int i = 0; i < sequence_count; i++) {
                if (i > 0) {
                    strcat(new_ans, "-"); 
                }
                strcat(new_ans, sequences[i]);
            }
            strcpy(ans, new_ans); 
        }
    }

    printf("%s\n", ans);
    free(ans);
}

int main() {
    int n; 
    scanf("%d", &n);
    for (int i = 0; i < n; i++) {
        char rna[2048 + 1]; 
        scanf(" %[^\n]", rna);
        solve(rna);
    }

    return 0;
}
