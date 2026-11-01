/* geometry_generator.c */
#include <stdlib.h>

/* ... definisi struct FrdNode dan FrdElement ... */

void generate_h_beam(int num_segments, double length,
                     double bf, double tf, double hw, double tw,
                     FrdNode **out_nodes, int *out_num_nodes,
                     FrdElement **out_elements, int *out_num_elements) {
    /* Asumsi: balok sepanjang sumbu X */
    /* Titik-titik penampang (y, z) untuk H-Beam */
    double section_y[8] = { hw/2+tf, hw/2+tf, -hw/2-tf, -hw/2-tf, hw/2, hw/2, -hw/2, -hw/2 };
    double section_z[8] = { -bf/2, bf/2, -bf/2, bf/2, -tw/2, tw/2, -tw/2, tw/2 };

    int nodes_per_section = 8;
    *out_num_nodes = (num_segments + 1) * nodes_per_section;
    *out_nodes = (FrdNode*)malloc((*out_num_nodes) * sizeof(FrdNode));

    int node_id = 1;
    double dx = length / num_segments;

    for (int i = 0; i <= num_segments; i++) {
        double x = i * dx;
        for (int j = 0; j < nodes_per_section; j++) {
            (*out_nodes)[node_id-1].id = node_id;
            (*out_nodes)[node_id-1].x = x;
            (*out_nodes)[node_id-1].y = section_y[j];
            (*out_nodes)[node_id-1].z = section_z[j];
            node_id++;
        }
    }

    /* Membuat elemen (Hexahedron 8-node) */
    *out_num_elements = num_segments * 4; /* 4 elemen per segmen */
    *out_elements = (FrdElement*)malloc((*out_num_elements) * sizeof(FrdElement));

    int elem_id = 1;
    for (int i = 0; i < num_segments; i++) {
        int base1 = i * nodes_per_section + 1;
        int base2 = (i + 1) * nodes_per_section + 1;
        
        /* Contoh: 1 elemen untuk flens atas, 1 web, 1 flens bawah, dll */
        /* (Implementasi detail topologi perlu disesuaikan) */
        /* ... */
        elem_id++;
    }
}