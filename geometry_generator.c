/* geometry_generator.c */
/* Modul Pembuat Geometri Penampang H, L, C (Multi-Hexahedron) */
/* ANSI C Compatible */

#include <stdio.h>
#include <stdlib.h>

/* --- Definisi Struktur --- */
typedef struct {
    int id;
    double x, y, z;
} FrdNode;

typedef struct {
    int id;
    int node_ids[8];
} FrdElement;

/* --- Fungsi Bantu: Menambah Elemen Hexahedron --- */
void add_hex_element(FrdElement *elems, int *elem_count, int n1, int n2, int n3, int n4, int n5, int n6, int n7, int n8) {
    elems[*elem_count].id = *elem_count + 1;
    elems[*elem_count].node_ids[0] = n1;
    elems[*elem_count].node_ids[1] = n2;
    elems[*elem_count].node_ids[2] = n3;
    elems[*elem_count].node_ids[3] = n4;
    elems[*elem_count].node_ids[4] = n5;
    elems[*elem_count].node_ids[5] = n6;
    elems[*elem_count].node_ids[6] = n7;
    elems[*elem_count].node_ids[7] = n8;
    (*elem_count)++;
}

/* ========================================================= */
/* 1. GENERATOR H-BEAM (3 Hex per Segmen)                    */
/* ========================================================= */
void generate_h_beam(int num_segments, double length,
                     double bf, double tf, double hw, double tw,
                     FrdNode **out_nodes, int *out_num_nodes,
                     FrdElement **out_elements, int *out_num_elements) {
    
    // Total node per irisan melintang (cross-section)
    // 4 (flens atas) + 4 (web) + 4 (flens bawah) = 12 node
    int nodes_per_section = 12;
    *out_num_nodes = (num_segments + 1) * nodes_per_section;
    *out_nodes = (FrdNode*)malloc((*out_num_nodes) * sizeof(FrdNode));

    // Titik-titik penampang (y, z) untuk H-Beam
    // Asumsi: sumbu X = panjang, Y = vertikal, Z = horizontal
    double sec_y[12] = { 
         hw/2 + tf,  hw/2 + tf,  hw/2 + tf,  hw/2 + tf,  // Flens Atas
         hw/2,       hw/2,      -hw/2,      -hw/2,       // Web
        -hw/2 - tf, -hw/2 - tf, -hw/2 - tf, -hw/2 - tf   // Flens Bawah
    };
    double sec_z[12] = { 
        -bf/2, bf/2,  bf/2, -bf/2,  // Flens Atas
        -tw/2, tw/2,  tw/2, -tw/2,  // Web
        -bf/2, bf/2,  bf/2, -bf/2   // Flens Bawah
    };

    // Buat Node
    int node_id = 1;
    double dx = length / num_segments;
    for (int i = 0; i <= num_segments; i++) {
        double x = i * dx;
        for (int j = 0; j < nodes_per_section; j++) {
            (*out_nodes)[node_id-1].id = node_id;
            (*out_nodes)[node_id-1].x = x;
            (*out_nodes)[node_id-1].y = sec_y[j];
            (*out_nodes)[node_id-1].z = sec_z[j];
            node_id++;
        }
    }

    // Buat Elemen (3 Hex per segmen)
    *out_num_elements = num_segments * 3;
    *out_elements = (FrdElement*)malloc((*out_num_elements) * sizeof(FrdElement));
    int elem_count = 0;

    for (int i = 0; i < num_segments; i++) {
        int base1 = i * nodes_per_section + 1; // Node irisan kiri
        int base2 = (i + 1) * nodes_per_section + 1; // Node irisan kanan

        // Hex 1: Flens Atas (Node 1-4 kiri, Node 1-4 kanan)
        add_hex_element(*out_elements, &elem_count,
            base1+0, base1+1, base1+2, base1+3,
            base2+0, base2+1, base2+2, base2+3);

        // Hex 2: Web (Node 5-8 kiri, Node 5-8 kanan)
        add_hex_element(*out_elements, &elem_count,
            base1+4, base1+5, base1+6, base1+7,
            base2+4, base2+5, base2+6, base2+7);

        // Hex 3: Flens Bawah (Node 9-12 kiri, Node 9-12 kanan)
        add_hex_element(*out_elements, &elem_count,
            base1+8, base1+9, base1+10, base1+11,
            base2+8, base2+9, base2+10, base2+11);
    }
}

/* ========================================================= */
/* 2. GENERATOR L-ANGLE (2 Hex per Segmen)                   */
/* ========================================================= */
void generate_l_angle(int num_segments, double length,
                      double b, double t,
                      FrdNode **out_nodes, int *out_num_nodes,
                      FrdElement **out_elements, int *out_num_elements) {
    
    // 8 node per irisan (4 untuk kaki horizontal, 4 untuk kaki vertikal)
    int nodes_per_section = 8;
    *out_num_nodes = (num_segments + 1) * nodes_per_section;
    *out_nodes = (FrdNode*)malloc((*out_num_nodes) * sizeof(FrdNode));

    double y0 = t/2.0; // Posisi centroid
    double z0 = t/2.0;
    
    // Titik (y, z)
    double sec_y[8] = { y0, t, t, y0,  y0, b, b, y0 };
    double sec_z[8] = { z0, z0, b, b,  z0, z0, b, b };

    // Buat Node
    int node_id = 1;
    double dx = length / num_segments;
    for (int i = 0; i <= num_segments; i++) {
        double x = i * dx;
        for (int j = 0; j < nodes_per_section; j++) {
            (*out_nodes)[node_id-1].id = node_id;
            (*out_nodes)[node_id-1].x = x;
            (*out_nodes)[node_id-1].y = sec_y[j];
            (*out_nodes)[node_id-1].z = sec_z[j];
            node_id++;
        }
    }

    // Buat Elemen (2 Hex per segmen)
    *out_num_elements = num_segments * 2;
    *out_elements = (FrdElement*)malloc((*out_num_elements) * sizeof(FrdElement));
    int elem_count = 0;

    for (int i = 0; i < num_segments; i++) {
        int base1 = i * nodes_per_section + 1;
        int base2 = (i + 1) * nodes_per_section + 1;

        // Hex 1: Kaki Horizontal
        add_hex_element(*out_elements, &elem_count,
            base1+0, base1+1, base1+2, base1+3,
            base2+0, base2+1, base2+2, base2+3);

        // Hex 2: Kaki Vertikal
        add_hex_element(*out_elements, &elem_count,
            base1+4, base1+5, base1+6, base1+7,
            base2+4, base2+5, base2+6, base2+7);
    }
}

/* ========================================================= */
/* 3. GENERATOR CHANNEL C (3 Hex per Segmen)                 */
/* ========================================================= */
void generate_c_channel(int num_segments, double length,
                        double bf, double tf, double hw, double tw,
                        FrdNode **out_nodes, int *out_num_nodes,
                        FrdElement **out_elements, int *out_num_elements) {
    
    // 12 node per irisan (sama seperti H-Beam)
    int nodes_per_section = 12;
    *out_num_nodes = (num_segments + 1) * nodes_per_section;
    *out_nodes = (FrdNode*)malloc((*out_num_nodes) * sizeof(FrdNode));

    double y0 = hw/2 + tf;
    double z_web = -tw/2;
    double z_flens = tw/2 + bf;

    // Titik (y, z)
    double sec_y[12] = { 
         y0, y0, -y0, -y0,  // Flens Atas
         hw/2, hw/2, -hw/2, -hw/2, // Web
        -y0, -y0, y0, y0    // Flens Bawah (urutan dibalik untuk topologi)
    };
    double sec_z[12] = { 
        z_web, z_flens, z_flens, z_web, // Flens Atas
        z_web, z_web, z_web, z_web,     // Web (tipis)
        z_web, z_flens, z_flens, z_web  // Flens Bawah
    };

    // Buat Node
    int node_id = 1;
    double dx = length / num_segments;
    for (int i = 0; i <= num_segments; i++) {
        double x = i * dx;
        for (int j = 0; j < nodes_per_section; j++) {
            (*out_nodes)[node_id-1].id = node_id;
            (*out_nodes)[node_id-1].x = x;
            (*out_nodes)[node_id-1].y = sec_y[j];
            (*out_nodes)[node_id-1].z = sec_z[j];
            node_id++;
        }
    }

    // Buat Elemen (3 Hex per segmen)
    *out_num_elements = num_segments * 3;
    *out_elements = (FrdElement*)malloc((*out_num_elements) * sizeof(FrdElement));
    int elem_count = 0;

    for (int i = 0; i < num_segments; i++) {
        int base1 = i * nodes_per_section + 1;
        int base2 = (i + 1) * nodes_per_section + 1;

        // Hex 1: Flens Atas
        add_hex_element(*out_elements, &elem_count,
            base1+0, base1+1, base1+2, base1+3,
            base2+0, base2+1, base2+2, base2+3);

        // Hex 2: Web
        add_hex_element(*out_elements, &elem_count,
            base1+4, base1+5, base1+6, base1+7,
            base2+4, base2+5, base2+6, base2+7);

        // Hex 3: Flens Bawah
        add_hex_element(*out_elements, &elem_count,
            base1+8, base1+9, base1+10, base1+11,
            base2+8, base2+9, base2+10, base2+11);
    }
}