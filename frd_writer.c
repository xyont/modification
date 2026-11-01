/* frd_writer.c */
#include <stdio.h>
#include <stdlib.h>

/* Struktur Node */
typedef struct {
    int id;
    double x, y, z;
} FrdNode;

/* Struktur Elemen (contoh: C3D8 dengan 8 node) */
typedef struct {
    int id;
    int node_ids[8];
} FrdElement;

/* Struktur Hasil Tegangan di Node */
typedef struct {
    int node_id;
    double sxx, syy, szz, sxy, sxz, syz;
} FrdResult;

/* Tulis file .frd */
void write_frd_file(const char *filename,
                    FrdNode *nodes, int num_nodes,
                    FrdElement *elements, int num_elements,
                    FrdResult *results, int num_results) {
    FILE *fp = fopen(filename, "w");
    if (!fp) {
        printf("ERROR: Cannot create file %s\n", filename);
        return;
    }

    /* Header FRD */
    fprintf(fp, "    1C\n");
    fprintf(fp, "    1UUSER\n");
    fprintf(fp, "    2C\n");
    fprintf(fp, "    2UUSER\n");

    /* Blok NODE */
    fprintf(fp, "    3C\n");
    for (int i = 0; i < num_nodes; i++) {
        fprintf(fp, "%5d%12.5f%12.5f%12.5f\n",
                nodes[i].id, nodes[i].x, nodes[i].y, nodes[i].z);
    }
    fprintf(fp, " -3\n");

    /* Blok ELEMENT (Tipe 1 = Hexahedron) */
    fprintf(fp, "    3C\n");
    for (int i = 0; i < num_elements; i++) {
        fprintf(fp, "%5d    1", elements[i].id);
        for (int j = 0; j < 8; j++) {
            fprintf(fp, "%5d", elements[i].node_ids[j]);
        }
        fprintf(fp, "\n");
    }
    fprintf(fp, " -3\n");

    /* Blok HASIL (STRESS) */
    if (results && num_results > 0) {
        fprintf(fp, "  100C\n");
        fprintf(fp, "  -4  STRESS        4    1\n");
        fprintf(fp, "  -5  SXX          1    2    1    0\n");
        fprintf(fp, "  -5  SYY          1    2    2    0\n");
        fprintf(fp, "  -5  SZZ          1    2    3    0\n");
        fprintf(fp, "  -5  SXY          1    2    4    0\n");
        fprintf(fp, "  -5  SXZ          1    2    5    0\n");
        fprintf(fp, "  -5  SYZ          1    2    6    0\n");
        fprintf(fp, "  -1\n");

        for (int i = 0; i < num_results; i++) {
            fprintf(fp, " -1%5d%12.5E%12.5E%12.5E%12.5E%12.5E%12.5E\n",
                    results[i].node_id,
                    results[i].sxx, results[i].syy, results[i].szz,
                    results[i].sxy, results[i].sxz, results[i].syz);
        }
        fprintf(fp, " -3\n");
    }

    fclose(fp);
    printf("File FRD berhasil ditulis: %s\n", filename);
}