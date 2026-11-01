/* main.c */
#include <stdio.h>
#include <stdlib.h>

// --- Deklarasi Struktur (Sama dengan modul lain) ---
typedef struct { int id; double x, y, z; } FrdNode;
typedef struct { int id; int node_ids[8]; } FrdElement;
typedef struct { int elem_id; int ip_id; double sxx, syy, szz, sxy, sxz, syz; } StressData;
typedef struct { int node_id; double sxx, syy, szz, sxy, sxz, syz; } FrdResult;

// --- Deklarasi Fungsi dari modul lain ---
StressData* read_stresses_from_dat(const char *filename, int *out_count);
void generate_h_beam(int num_segments, double length, double bf, double tf, double hw, double tw, FrdNode **nodes, int *nn, FrdElement **elems, int *ne);
void estimate_ip_coordinates(int total_ips, double length, double *ip_x, double *ip_y, double *ip_z);
void interpolate_stress_to_nodes(FrdNode *nodes, int num_nodes, StressData *stress_data, int num_stress, double *ip_x, double *ip_y, double *ip_z, double power, FrdResult *results);
void write_frd_file(const char *filename, FrdNode *nodes, int num_nodes, FrdElement *elements, int num_elements, FrdResult *results, int num_results);

int main() {
    // 1. Baca data dari .dat
    int num_stress = 0;
    StressData *stress_data = read_stresses_from_dat("hasil_analisis.dat", &num_stress);
    if (!stress_data || num_stress == 0) {
        printf("Gagal membaca data .dat.\n");
        return 1;
    }

    // 2. Buat geometri visualisasi (H-Beam)
    FrdNode *nodes = NULL;
    FrdElement *elems = NULL;
    int num_nodes = 0, num_elements = 0;
    double length = 1000.0; // Panjang balok (sesuaikan)
    generate_h_beam(10, length, 100.0, 10.0, 200.0, 8.0, &nodes, &num_nodes, &elems, &num_elements);

    // 3. Estimasi koordinat Integration Point
    double *ip_x = (double*)malloc(num_stress * sizeof(double));
    double *ip_y = (double*)malloc(num_stress * sizeof(double));
    double *ip_z = (double*)malloc(num_stress * sizeof(double));
    estimate_ip_coordinates(num_stress, length, ip_x, ip_y, ip_z);

    // 4. Interpolasi data stress ke node
    FrdResult *results = (FrdResult*)malloc(num_nodes * sizeof(FrdResult));
    interpolate_stress_to_nodes(nodes, num_nodes, stress_data, num_stress, ip_x, ip_y, ip_z, 2.0, results);

    // 5. Tulis file .frd baru
    write_frd_file("visualisasi_hbeam_stress.frd", nodes, num_nodes, elems, num_elements, results, num_nodes);

    // 6. Bersihkan memori
    free(stress_data);
    free(nodes);
    free(elems);
    free(ip_x); free(ip_y); free(ip_z);
    free(results);

    printf("Selesai! Buka 'visualisasi_hbeam_stress.frd' di CGX.\n");
    return 0;
}