/* dat_reader.c */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE 256

/* Struktur untuk menyimpan data tegangan di satu integration point */
typedef struct {
    int elem_id;
    int ip_id;
    double sxx, syy, szz, sxy, sxz, syz;
} StressData;

/* Struktur untuk menampung semua data */
typedef struct {
    StressData *data;
    int count;
    int capacity;
} StressList;

/* Inisialisasi list */
void init_stress_list(StressList *list, int initial_capacity) {
    list->data = (StressData*)malloc(initial_capacity * sizeof(StressData));
    list->count = 0;
    list->capacity = initial_capacity;
}

/* Tambah data ke list */
void add_stress_data(StressList *list, StressData *sd) {
    if (list->count >= list->capacity) {
        list->capacity *= 2;
        list->data = (StressData*)realloc(list->data, list->capacity * sizeof(StressData));
    }
    list->data[list->count] = *sd;
    list->count++;
}

/* Baca file .dat */
StressList* read_dat_file(const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        printf("ERROR: Cannot open file %s\n", filename);
        return NULL;
    }

    StressList *list = (StressList*)malloc(sizeof(StressList));
    init_stress_list(list, 100);

    char line[MAX_LINE];
    int in_stress_block = 0;
    const char *header = "stresses (elem, integ.pnt.,sxx,syy,szz,sxy,sxz,syz)";

    while (fgets(line, MAX_LINE, fp)) {
        /* Cari header */
        if (strstr(line, header) != NULL) {
            in_stress_block = 1;
            continue; /* Lanjut ke baris data berikutnya */
        }

        if (in_stress_block) {
            /* Coba parse baris data */
            StressData sd;
            if (sscanf(line, "%d %d %lf %lf %lf %lf %lf %lf",
                       &sd.elem_id, &sd.ip_id,
                       &sd.sxx, &sd.syy, &sd.szz,
                       &sd.sxy, &sd.sxz, &sd.syz) == 8) {
                add_stress_data(list, &sd);
            } else {
                /* Jika gagal parse, berarti blok stresses sudah habis */
                in_stress_block = 0;
                /* break; */ /* Jangan break, mungkin ada blok lain yang dicari */
            }
        }
    }

    fclose(fp);
    return list;
}

/* Contoh penggunaan (bisa dihapus saat integrasi) */
/*
int main() {
    StressList *list = read_dat_file("hasil.dat");
    if (list) {
        printf("Total data: %d\n", list->count);
        for (int i = 0; i < list->count && i < 5; i++) {
            printf("Elem %d, IP %d: SXX=%f\n", list->data[i].elem_id, list->data[i].ip_id, list->data[i].sxx);
        }
        free(list->data);
        free(list);
    }
    return 0;
}
*/