/* interpolator.c */
/* Modul Interpolasi Tegangan dari Integration Point ke Node Visualisasi */
/* ANSI C Compatible */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* --- Definisi Struktur (Harus sama dengan modul lain) --- */
typedef struct {
    int id;
    double x, y, z;
} FrdNode;

typedef struct {
    int elem_id;
    int ip_id;
    double sxx, syy, szz, sxy, sxz, syz;
} StressData;

typedef struct {
    int node_id;
    double sxx, syy, szz, sxy, sxz, syz;
} FrdResult;

/* ========================================================= */
/* FUNGSI BANTU: Hitung posisi (x,y,z) dari Integration Point */
/* ========================================================= */
/*
 * Karena file .dat tidak menyimpan koordinat IP, kita perlu 
 * mengestimasi posisinya berdasarkan skema integrasi.
 * Fungsi ini adalah pendekatan sederhana untuk 1 segmen balok
 * (dari x=0 hingga x=L).
 * 
 * Untuk akurasi tinggi, Anda harus menyesuaikan fungsi ini
 * dengan skema integrasi di beamintscheme.f (H, L, C).
 */
void estimate_ip_coordinates(int total_ips, double length, 
                             double *ip_x, double *ip_y, double *ip_z) {
    // Contoh pendekatan: IP tersebar merata di sepanjang balok
    // dan di dalam penampang (bukan posisi presisi).
    // Untuk visualisasi, ini sudah cukup.
    for (int i = 0; i < total_ips; i++) {
        ip_x[i] = (length / (total_ips - 1)) * i; // Tersebar di sumbu X
        ip_y[i] = 0.0; // Anggap di tengah (bisa disesuaikan)
        ip_z[i] = 0.0; // Anggap di tengah
    }
}

/* ========================================================= */
/* FUNGSI UTAMA: Interpolasi IDW (Inverse Distance Weighting) */
/* ========================================================= */
/*
 * Memetakan nilai tegangan dari integration points ke nodes.
 * 
 * Parameters:
 * - nodes: Array node visualisasi (dari geometry_generator)
 * - num_nodes: Jumlah node visualisasi
 * - stress_data: Array data tegangan (dari dat_reader)
 * - num_stress: Jumlah data tegangan (IP)
 * - ip_x, ip_y, ip_z: Koordinat estimasi IP
 * - power: Faktor pangkat untuk IDW (biasanya 2.0)
 * - results: Array output hasil (dari frd_writer)
 */
void interpolate_stress_to_nodes(
    FrdNode *nodes, int num_nodes,
    StressData *stress_data, int num_stress,
    double *ip_x, double *ip_y, double *ip_z,
    double power,
    FrdResult *results) 
{
    printf("Memulai interpolasi IDW (Power=%.1f)...\n", power);

    for (int i = 0; i < num_nodes; i++) {
        double wx = nodes[i].x;
        double wy = nodes[i].y;
        double wz = nodes[i].z;

        double sum_w = 0.0;
        double sum_sxx = 0.0, sum_syy = 0.0, sum_szz = 0.0;
        double sum_sxy = 0.0, sum_sxz = 0.0, sum_syz = 0.0;

        for (int j = 0; j < num_stress; j++) {
            double dx = wx - ip_x[j];
            double dy = wy - ip_y[j];
            double dz = wz - ip_z[j];
            
            double dist = sqrt(dx*dx + dy*dy + dz*dz);

            // Hindari pembagian dengan nol
            if (dist < 1e-8) {
                dist = 1e-8; 
            }

            // Bobot IDW: 1 / (distance^power)
            double w = 1.0 / pow(dist, power);
            
            sum_w += w;
            sum_sxx += w * stress_data[j].sxx;
            sum_syy += w * stress_data[j].syy;
            sum_szz += w * stress_data[j].szz;
            sum_sxy += w * stress_data[j].sxy;
            sum_sxz += w * stress_data[j].sxz;
            sum_syz += w * stress_data[j].syz;
        }

        // Normalisasi
        if (sum_w > 0.0) {
            results[i].node_id = nodes[i].id;
            results[i].sxx = sum_sxx / sum_w;
            results[i].syy = sum_syy / sum_w;
            results[i].szz = sum_szz / sum_w;
            results[i].sxy = sum_sxy / sum_w;
            results[i].sxz = sum_sxz / sum_w;
            results[i].syz = sum_syz / sum_w;
        } else {
            // Jika tidak ada data, isi nol
            results[i].node_id = nodes[i].id;
            results[i].sxx = results[i].syy = results[i].szz = 0.0;
            results[i].sxy = results[i].sxz = results[i].syz = 0.0;
        }
    }
    printf("Interpolasi selesai untuk %d node.\n", num_nodes);
}