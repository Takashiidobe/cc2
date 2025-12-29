// Test incomplete types - forward declarations
#include <stdlib.h>

// Forward declaration of struct
struct Node;

// Can use pointers to incomplete types
struct Node *create_node(int value);
void process_node(struct Node *n);

// Complete definition
struct Node {
    int value;
    struct Node *next;
};

// Forward declaration of union
union Data;

// Can use pointers to incomplete unions
void process_data(union Data *d);

// Complete definition
union Data {
    int i;
    double d;
};

// Test mutually referential structs
struct A;
struct B;

struct A {
    int x;
    struct B *b_ptr;
};

struct B {
    int y;
    struct A *a_ptr;
};

int main() {
    // Create a simple linked list
    struct Node head;
    struct Node second;
    struct Node third;

    head.value = 1;
    head.next = &second;

    second.value = 2;
    second.next = &third;

    third.value = 3;
    third.next = NULL;

    // Calculate sum
    int sum = 0;
    struct Node *current = &head;
    while (current != NULL) {
        sum = sum + current->value;
        current = current->next;
    }

    // Test union
    union Data data;
    data.i = 42;

    // Test mutually referential structs
    struct A a;
    struct B b;
    a.x = 10;
    a.b_ptr = &b;
    b.y = 20;
    b.a_ptr = &a;

    int result = sum + data.i + a.x + b.y;

    // sum=6, data.i=42, a.x=10, b.y=20
    // Expected: 6 + 42 + 10 + 20 = 78
    return result;
}
