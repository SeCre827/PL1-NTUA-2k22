#include <stdio.h>
#include <stdlib.h>

typedef struct edge {
  int x, y, w;
} edge;

int *parent, *rank;
int Ncities, Nroads;
struct edge* edges;

int compare(const void *a, const void *b) {
  edge *edgeA = (edge *)a;
  edge *edgeB = (edge *)b;

  return (edgeA->w > edgeB->w) - (edgeA->w < edgeB->w);
}

void init() {
    for (int i = 0; i < Ncities; ++i) {
        parent[i] = i;
        rank[i] = 1;
    }
}

int find(int i) {
    // finds root. essentially node-cluster.
    while (i != parent[i]) {
        parent[i] = parent[parent[i]];
        i = parent[i];
    }
    return i;
}


void unite(int i, int j) {
    // change elements with their roots.
    // int i = find(p), j = find(q);
     if (rank[i] < rank[j]) {
        parent[i] = j;
        rank[j] += rank[i];
    }
    else {
      parent[j] = i;
      rank[i] += rank[j];
    }
}

int kruskal() {
    qsort(edges, Nroads, sizeof(edge), compare);
    int val = 0;

    for (int i = 0; i < Nroads; ++i) {
        // printf("\n\ni=%d\n", i);
        int w = edges[i].w;
        int x = edges[i].x;
        int y = edges[i].y;

        // consider edge if there's no cycle.
        x = find(x); y = find(y);
        if (x != y) {
            unite(x, y);
            val = w > val ? w : val;
            // printf("%d %d %d\n", x, y, w);
        }
    }

    return val;
}



int main (int argc, char* argv[]) {
    
    FILE *fp = fopen(argv[1], "r");
    if (fp == NULL) {
        perror("Error in opening file");
        return -1;
    }

    if (!fscanf(fp, "%d %d", &Ncities, &Nroads)) {
        perror("Error in reading first line");
        return -1;
    }

    edges  = (edge *)malloc(Nroads * sizeof(edge));
    rank   = (int *)malloc(Ncities * sizeof(int));
    parent = (int *)malloc(Ncities * sizeof(int));


    // Object newObj1 = (Object){.value = init_value, .id = init_id};

    int i = -1;
    while ((++i) < Nroads) {
        int tmp1, tmp2;
        if (!fscanf(fp, "%d %d %d", &(tmp1), &(tmp2), &(edges[i].w))) {
            perror("Error reading edge!\n");
            return -1;
        }
        edges[i].x = --tmp1; edges[i].y = --tmp2;
    }
    fclose(fp);

    init();

    printf("%d", kruskal());
    
    free(parent);
    free(rank);
    free(edges);
    return 0;
}        